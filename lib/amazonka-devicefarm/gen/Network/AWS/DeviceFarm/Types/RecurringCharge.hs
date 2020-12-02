{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RecurringCharge where

import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies whether charges for devices are recurring.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcFrequency ::
      !(Maybe RecurringChargeFrequency),
    _rcCost :: !(Maybe MonetaryAmount)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcFrequency' - The frequency in which charges recur.
--
-- * 'rcCost' - The cost of the recurring charge.
recurringCharge ::
  RecurringCharge
recurringCharge =
  RecurringCharge' {_rcFrequency = Nothing, _rcCost = Nothing}

-- | The frequency in which charges recur.
rcFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rcFrequency = lens _rcFrequency (\s a -> s {_rcFrequency = a})

-- | The cost of the recurring charge.
rcCost :: Lens' RecurringCharge (Maybe MonetaryAmount)
rcCost = lens _rcCost (\s a -> s {_rcCost = a})

instance FromJSON RecurringCharge where
  parseJSON =
    withObject
      "RecurringCharge"
      ( \x ->
          RecurringCharge' <$> (x .:? "frequency") <*> (x .:? "cost")
      )

instance Hashable RecurringCharge

instance NFData RecurringCharge
