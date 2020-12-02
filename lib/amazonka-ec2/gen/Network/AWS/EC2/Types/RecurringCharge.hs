{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RecurringCharge where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.RecurringChargeFrequency
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a recurring charge.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcAmount ::
      !(Maybe Double),
    _rcFrequency :: !(Maybe RecurringChargeFrequency)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcAmount' - The amount of the recurring charge.
--
-- * 'rcFrequency' - The frequency of the recurring charge.
recurringCharge ::
  RecurringCharge
recurringCharge =
  RecurringCharge' {_rcAmount = Nothing, _rcFrequency = Nothing}

-- | The amount of the recurring charge.
rcAmount :: Lens' RecurringCharge (Maybe Double)
rcAmount = lens _rcAmount (\s a -> s {_rcAmount = a})

-- | The frequency of the recurring charge.
rcFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rcFrequency = lens _rcFrequency (\s a -> s {_rcFrequency = a})

instance FromXML RecurringCharge where
  parseXML x =
    RecurringCharge' <$> (x .@? "amount") <*> (x .@? "frequency")

instance Hashable RecurringCharge

instance NFData RecurringCharge
