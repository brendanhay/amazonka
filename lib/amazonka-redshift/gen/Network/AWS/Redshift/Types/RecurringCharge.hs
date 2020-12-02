{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.RecurringCharge where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes a recurring charge.
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
-- * 'rcRecurringChargeFrequency' - The frequency at which the recurring charge amount is applied.
--
-- * 'rcRecurringChargeAmount' - The amount charged per the period of time specified by the recurring charge frequency.
recurringCharge ::
  RecurringCharge
recurringCharge =
  RecurringCharge'
    { _rcRecurringChargeFrequency = Nothing,
      _rcRecurringChargeAmount = Nothing
    }

-- | The frequency at which the recurring charge amount is applied.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\s a -> s {_rcRecurringChargeFrequency = a})

-- | The amount charged per the period of time specified by the recurring charge frequency.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\s a -> s {_rcRecurringChargeAmount = a})

instance FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      <$> (x .@? "RecurringChargeFrequency")
      <*> (x .@? "RecurringChargeAmount")

instance Hashable RecurringCharge

instance NFData RecurringCharge
