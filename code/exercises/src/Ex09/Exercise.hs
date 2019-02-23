{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex09.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex09.Common
import Ex09.Run

mkStock ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)
mkStock qty p eName =
  foldDyn updateQty (Stock p qty) eName
  where
    updateQty n (Stock p'@(Product n' _) q) =
      Stock p' (if n == n' then q - 1 else q)

ex09 ::
  ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Event t Text)
ex09 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
  dMoney <- dynMoney eAdd eRefund eSpend

  let
    toStock name | name == "Carrot" = dCarrot
                 | name == "Celery" = dCelery
                 | otherwise = dCucumber

    eStock  = current  (dSelected >>= toStock) <@ eBuy
    eSale   = difference (sProduct <$> eStock) eError
    eVend   = pName <$> eSale
    eSpend  = pCost <$> eSale
    eChange = current dMoney <@ eRefund

    canBuy money (Stock (Product _ cost) qty)
      | qty < 1      = Just ItemOutOfStock
      | cost > money = Just NotEnoughMoney
      | otherwise    = Nothing

    eError = attachWithMaybe canBuy (current dMoney) eStock

  dChange <- holdDyn 0 $ leftmost [0 <$ eBuy, eChange]
  dVend   <- holdDyn "" $ leftmost ["" <$ eChange, errorText <$> eError, eVend]

  eBuy <- el "tr" $ do
    elAttr "td" ("colspan" =: "3") blank
    el "td" $ button "Buy"
  eAdd <- el "tr" $ do
    elAttr "td" ("colspan" =: "2") $ text "Money inserted:"
    el "td" $ dynText (moneyDisplay <$> dMoney)
    el "td" $ button "Add money"
  eRefund <- el "tr" $ do
    elAttr "td" ("colspan" =: "2") $ text "Change:"
    el "td" $ dynText (moneyDisplay <$> dChange)
    el "td" $ button "Refund"
  el "tr" $ do
    elAttr "td" ("colspan" =: "2") $ text "Tray:"
    elAttr "td" ("colspan" =: "2") $ dynText dVend

  pure eVend

dynMoney ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  )
  => Event t ()
  -> Event t ()
  -> Event t Money
  -> m (Dynamic t Money)
dynMoney eAdd eRefund eSpend =
  foldDyn ($) 0 . mergeWith (.) $ [
    (+1)     <$  eAdd
  , const 0  <$  eRefund
  , flip (-) <$> eSpend
  ]

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host mkStock ex09
#endif
