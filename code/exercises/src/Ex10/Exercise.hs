{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex10.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex10.Common
import Ex10.Run

grid :: MonadWidget t m
     => m a
     -> m a
grid =
  elClass "div" "container"

row :: MonadWidget t m
    => m a
    -> m b
    -> m c
    -> m d
    -> m d
row ma mb mc md =
  elClass "div" "row" $ do 
    elClass "div" "col-md-3" ma
    elClass "div" "col-md-1" mb
    elClass "div" "col-md-1" mc
    elClass "div" "col-md-2" md

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

ex10 ::
  ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Event t Text)
ex10 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
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

  eBuy <- elClass "div" "row" $ do
    elClass "div" "col-md-5" $ blank
    elClass "div" "col-md-2" $ button "Buy"

  eAdd <- elClass "div" "row" $ do
    elClass "div" "col-md-4" $ text "Money inserted:"
    elClass "div" "col-md-1" $ dynText (moneyDisplay <$> dMoney)
    elClass "div" "col-md-2" $ button "Add money"

  eRefund <- elClass "div" "row" $ do
    elClass "div" "col-md-4" $ text "Change:"
    elClass "div" "col-md-1" $ dynText (moneyDisplay <$> dChange)
    elClass "div" "col-md-2" $ button "Refund"

  elClass "div" "row" $ do
    elClass "div" "col-md-3" $ text "Tray:"
    elClass "div" "col-md-4" $ dynText dVend

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
    host grid row mkStock ex10
#endif
