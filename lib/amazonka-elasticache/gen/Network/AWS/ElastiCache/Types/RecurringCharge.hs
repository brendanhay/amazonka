{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.RecurringCharge where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the specific price and frequency of a recurring charges for a reserved cache node, or for a reserved cache node offering.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcRecurringChargeFrequency ::
      !(Maybe Text),
    _rcRecurringChargeAmount :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRecurringChargeFrequency' - The frequency of the recurring charge.
--
-- * 'rcRecurringChargeAmount' - The monetary amount of the recurring charge.
recurringCharge ::
  RecurringCharge
recurringCharge =
  RecurringCharge'
    { _rcRecurringChargeFrequency = Nothing,
      _rcRecurringChargeAmount = Nothing
    }

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\s a -> s {_rcRecurringChargeFrequency = a})

-- | The monetary amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\s a -> s {_rcRecurringChargeAmount = a})

instance FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      <$> (x .@? "RecurringChargeFrequency")
      <*> (x .@? "RecurringChargeAmount")

instance Hashable RecurringCharge

instance NFData RecurringCharge
