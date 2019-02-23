{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex14.Exercise where

import Control.Monad.Fix (MonadFix)

import Data.Bool (bool)
import Data.Text (Text, pack)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex14.Common
import Ex14.Run

ex14 ::
  ( MonadWidget t m
  ) =>
  m ()
ex14 = mdo
  dCarrot   <- mkStock 5 carrot eVend
  dCelery   <- mkStock 5 celery eVend
  dCucumber <- mkStock 5 cucumber eVend

  dSelected <- do
    esCarrot   <- stockWidget dCarrot dSelected
    esCelery   <- stockWidget dCelery dSelected
    esCucumber <- stockWidget dCucumber dSelected
    holdDyn "Carrot" $ leftmost [esCarrot, esCelery, esCucumber]

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

radioButton ::
  ( MonadWidget t m
  , Eq a
  ) =>
  Text ->
  Dynamic t a ->
  Dynamic t a ->
  m (Event t a)
radioButton gName dValue dSelected = do
  let
    dChecked = (==) <$> dValue <*> dSelected
    baseAttr = "type" =: "radio" <> "name" =: gName
    checkedAttr = bool Map.empty ("checked" =: "") <$> dChecked
    dAttr = pure baseAttr <> checkedAttr
  (e, _) <- elDynAttr' "input" dAttr blank
  pure $ current dValue <@ domEvent Click e

stockWidget ::
  MonadWidget t m =>
  Dynamic t Stock ->
  Dynamic t Text ->
  m (Event t Text)
stockWidget dStock dSelected =
  elClass "div" "row" $ do 
    let
      dName = pName . sProduct <$> dStock
    elClass "div" "col-md-3" $ dynText dName
    elClass "div" "col-md-1" $ dynText (pack . show . sQuantity <$> dStock)
    elClass "div" "col-md-1" $ dynText (moneyDisplay . pCost . sProduct <$> dStock)
    elClass "div" "col-md-2" $ radioButton "product" dName dSelected 

grid ::
  MonadWidget t m =>
  m a ->
  m a
grid =
  elClass "div" "container"

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
    host ex14
#endif
