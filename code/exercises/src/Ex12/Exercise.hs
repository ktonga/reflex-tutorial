{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ex12.Exercise where

import Control.Monad.Fix (MonadFix)
import Control.Lens ((^.))

import Data.Text (Text, pack)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex12.Common
import Ex12.Run

radioCheckbox ::
  ( MonadWidget t m
  , Eq a
  ) =>
  Dynamic t a ->
  Dynamic t a ->
  m (Event t a)
radioCheckbox dValue dSelected = do
  ePostBuild <- getPostBuild
  let 
    dChecked = (==) <$> dValue <*> dSelected
    eChecked = leftmost [updated dChecked, current dChecked <@ ePostBuild]
  cb <- checkbox False $ def & checkboxConfig_setValue .~ eChecked
  pure $ (current dValue) <@ (cb ^. checkbox_change)

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
    elClass "div" "col-md-2" $ radioCheckbox dName dSelected 

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

ex12 ::
  ( MonadWidget t m
  ) =>
  Inputs t ->
  m (Event t Text)
ex12 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
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
    host grid stockWidget mkStock ex12
#endif
