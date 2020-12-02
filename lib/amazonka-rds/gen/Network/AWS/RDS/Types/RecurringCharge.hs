{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.RecurringCharge where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a response element in the @DescribeReservedDBInstances@ and @DescribeReservedDBInstancesOfferings@ actions.
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
-- * 'rcRecurringChargeAmount' - The amount of the recurring charge.
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

-- | The amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\s a -> s {_rcRecurringChargeAmount = a})

instance FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      <$> (x .@? "RecurringChargeFrequency")
      <*> (x .@? "RecurringChargeAmount")

instance Hashable RecurringCharge

instance NFData RecurringCharge
