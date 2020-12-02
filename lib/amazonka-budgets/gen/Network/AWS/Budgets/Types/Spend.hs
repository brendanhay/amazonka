{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Spend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Spend where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amount of cost or usage that is measured for a budget.
--
--
-- For example, a @Spend@ for @3 GB@ of S3 usage would have the following parameters:
--
--     * An @Amount@ of @3@
--
--     * A @unit@ of @GB@
--
--
--
--
-- /See:/ 'spend' smart constructor.
data Spend = Spend' {_sAmount :: !Text, _sUnit :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Spend' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAmount' - The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
--
-- * 'sUnit' - The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
spend ::
  -- | 'sAmount'
  Text ->
  -- | 'sUnit'
  Text ->
  Spend
spend pAmount_ pUnit_ =
  Spend' {_sAmount = pAmount_, _sUnit = pUnit_}

-- | The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
sAmount :: Lens' Spend Text
sAmount = lens _sAmount (\s a -> s {_sAmount = a})

-- | The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
sUnit :: Lens' Spend Text
sUnit = lens _sUnit (\s a -> s {_sUnit = a})

instance FromJSON Spend where
  parseJSON =
    withObject
      "Spend"
      (\x -> Spend' <$> (x .: "Amount") <*> (x .: "Unit"))

instance Hashable Spend

instance NFData Spend

instance ToJSON Spend where
  toJSON Spend' {..} =
    object
      (catMaybes [Just ("Amount" .= _sAmount), Just ("Unit" .= _sUnit)])
