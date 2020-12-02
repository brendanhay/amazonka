{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.CalculatedSpend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.CalculatedSpend where

import Network.AWS.Budgets.Types.Spend
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The spend objects that are associated with this budget. The @actualSpend@ tracks how much you've used, cost, usage, RI units, or Savings Plans units and the @forecastedSpend@ tracks how much you are predicted to spend based on your historical usage profile.
--
--
-- For example, if it is the 20th of the month and you have spent @50@ dollars on Amazon EC2, your @actualSpend@ is @50 USD@ , and your @forecastedSpend@ is @75 USD@ .
--
--
-- /See:/ 'calculatedSpend' smart constructor.
data CalculatedSpend = CalculatedSpend'
  { _csForecastedSpend ::
      !(Maybe Spend),
    _csActualSpend :: !Spend
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CalculatedSpend' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csForecastedSpend' - The amount of cost, usage, RI units, or Savings Plans units that you are forecasted to use.
--
-- * 'csActualSpend' - The amount of cost, usage, RI units, or Savings Plans units that you have used.
calculatedSpend ::
  -- | 'csActualSpend'
  Spend ->
  CalculatedSpend
calculatedSpend pActualSpend_ =
  CalculatedSpend'
    { _csForecastedSpend = Nothing,
      _csActualSpend = pActualSpend_
    }

-- | The amount of cost, usage, RI units, or Savings Plans units that you are forecasted to use.
csForecastedSpend :: Lens' CalculatedSpend (Maybe Spend)
csForecastedSpend = lens _csForecastedSpend (\s a -> s {_csForecastedSpend = a})

-- | The amount of cost, usage, RI units, or Savings Plans units that you have used.
csActualSpend :: Lens' CalculatedSpend Spend
csActualSpend = lens _csActualSpend (\s a -> s {_csActualSpend = a})

instance FromJSON CalculatedSpend where
  parseJSON =
    withObject
      "CalculatedSpend"
      ( \x ->
          CalculatedSpend'
            <$> (x .:? "ForecastedSpend") <*> (x .: "ActualSpend")
      )

instance Hashable CalculatedSpend

instance NFData CalculatedSpend

instance ToJSON CalculatedSpend where
  toJSON CalculatedSpend' {..} =
    object
      ( catMaybes
          [ ("ForecastedSpend" .=) <$> _csForecastedSpend,
            Just ("ActualSpend" .= _csActualSpend)
          ]
      )
