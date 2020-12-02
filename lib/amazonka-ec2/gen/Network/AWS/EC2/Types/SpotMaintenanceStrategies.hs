{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotMaintenanceStrategies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotMaintenanceStrategies where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SpotCapacityRebalance
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
--
--
-- /See:/ 'spotMaintenanceStrategies' smart constructor.
newtype SpotMaintenanceStrategies = SpotMaintenanceStrategies'
  { _smsCapacityRebalance ::
      Maybe SpotCapacityRebalance
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotMaintenanceStrategies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smsCapacityRebalance' - The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
spotMaintenanceStrategies ::
  SpotMaintenanceStrategies
spotMaintenanceStrategies =
  SpotMaintenanceStrategies' {_smsCapacityRebalance = Nothing}

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
smsCapacityRebalance :: Lens' SpotMaintenanceStrategies (Maybe SpotCapacityRebalance)
smsCapacityRebalance = lens _smsCapacityRebalance (\s a -> s {_smsCapacityRebalance = a})

instance FromXML SpotMaintenanceStrategies where
  parseXML x =
    SpotMaintenanceStrategies' <$> (x .@? "capacityRebalance")

instance Hashable SpotMaintenanceStrategies

instance NFData SpotMaintenanceStrategies

instance ToQuery SpotMaintenanceStrategies where
  toQuery SpotMaintenanceStrategies' {..} =
    mconcat ["CapacityRebalance" =: _smsCapacityRebalance]
