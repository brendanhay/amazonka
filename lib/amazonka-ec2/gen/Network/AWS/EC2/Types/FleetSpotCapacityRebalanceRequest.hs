{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetReplacementStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-capacity-rebalance Capacity rebalancing> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
-- /See:/ 'fleetSpotCapacityRebalanceRequest' smart constructor.
newtype FleetSpotCapacityRebalanceRequest = FleetSpotCapacityRebalanceRequest'
  { _fscrrReplacementStrategy ::
      Maybe
        FleetReplacementStrategy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetSpotCapacityRebalanceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fscrrReplacementStrategy' - The replacement strategy to use. Only available for fleets of type @maintain@ . To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . You must specify a value, otherwise you get an error.
fleetSpotCapacityRebalanceRequest ::
  FleetSpotCapacityRebalanceRequest
fleetSpotCapacityRebalanceRequest =
  FleetSpotCapacityRebalanceRequest'
    { _fscrrReplacementStrategy =
        Nothing
    }

-- | The replacement strategy to use. Only available for fleets of type @maintain@ . To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . You must specify a value, otherwise you get an error.
fscrrReplacementStrategy :: Lens' FleetSpotCapacityRebalanceRequest (Maybe FleetReplacementStrategy)
fscrrReplacementStrategy = lens _fscrrReplacementStrategy (\s a -> s {_fscrrReplacementStrategy = a})

instance Hashable FleetSpotCapacityRebalanceRequest

instance NFData FleetSpotCapacityRebalanceRequest

instance ToQuery FleetSpotCapacityRebalanceRequest where
  toQuery FleetSpotCapacityRebalanceRequest' {..} =
    mconcat ["ReplacementStrategy" =: _fscrrReplacementStrategy]
