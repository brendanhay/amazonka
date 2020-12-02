{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EC2InstanceCounts where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Current status of fleet capacity. The number of active instances should match or be in the process of matching the number of desired instances. Pending and terminating counts are non-zero only if fleet capacity is adjusting to an 'UpdateFleetCapacity' request, or if access to resources is temporarily affected.
--
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * 'DescribeFleetAttributes'
--
--     * 'UpdateFleetAttributes'
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
--
-- /See:/ 'ec2InstanceCounts' smart constructor.
data EC2InstanceCounts = EC2InstanceCounts'
  { _eicIdLE ::
      !(Maybe Nat),
    _eicTERMINATING :: !(Maybe Nat),
    _eicPENDING :: !(Maybe Nat),
    _eicMAXIMUM :: !(Maybe Nat),
    _eicDESIRED :: !(Maybe Nat),
    _eicMINIMUM :: !(Maybe Nat),
    _eicACTIVE :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2InstanceCounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eicIdLE' - Number of active instances in the fleet that are not currently hosting a game session.
--
-- * 'eicTERMINATING' - Number of instances in the fleet that are no longer active but haven't yet been terminated.
--
-- * 'eicPENDING' - Number of instances in the fleet that are starting but not yet active.
--
-- * 'eicMAXIMUM' - The maximum value allowed for the fleet's instance count.
--
-- * 'eicDESIRED' - Ideal number of active instances in the fleet.
--
-- * 'eicMINIMUM' - The minimum value allowed for the fleet's instance count.
--
-- * 'eicACTIVE' - Actual number of active instances in the fleet.
ec2InstanceCounts ::
  EC2InstanceCounts
ec2InstanceCounts =
  EC2InstanceCounts'
    { _eicIdLE = Nothing,
      _eicTERMINATING = Nothing,
      _eicPENDING = Nothing,
      _eicMAXIMUM = Nothing,
      _eicDESIRED = Nothing,
      _eicMINIMUM = Nothing,
      _eicACTIVE = Nothing
    }

-- | Number of active instances in the fleet that are not currently hosting a game session.
eicIdLE :: Lens' EC2InstanceCounts (Maybe Natural)
eicIdLE = lens _eicIdLE (\s a -> s {_eicIdLE = a}) . mapping _Nat

-- | Number of instances in the fleet that are no longer active but haven't yet been terminated.
eicTERMINATING :: Lens' EC2InstanceCounts (Maybe Natural)
eicTERMINATING = lens _eicTERMINATING (\s a -> s {_eicTERMINATING = a}) . mapping _Nat

-- | Number of instances in the fleet that are starting but not yet active.
eicPENDING :: Lens' EC2InstanceCounts (Maybe Natural)
eicPENDING = lens _eicPENDING (\s a -> s {_eicPENDING = a}) . mapping _Nat

-- | The maximum value allowed for the fleet's instance count.
eicMAXIMUM :: Lens' EC2InstanceCounts (Maybe Natural)
eicMAXIMUM = lens _eicMAXIMUM (\s a -> s {_eicMAXIMUM = a}) . mapping _Nat

-- | Ideal number of active instances in the fleet.
eicDESIRED :: Lens' EC2InstanceCounts (Maybe Natural)
eicDESIRED = lens _eicDESIRED (\s a -> s {_eicDESIRED = a}) . mapping _Nat

-- | The minimum value allowed for the fleet's instance count.
eicMINIMUM :: Lens' EC2InstanceCounts (Maybe Natural)
eicMINIMUM = lens _eicMINIMUM (\s a -> s {_eicMINIMUM = a}) . mapping _Nat

-- | Actual number of active instances in the fleet.
eicACTIVE :: Lens' EC2InstanceCounts (Maybe Natural)
eicACTIVE = lens _eicACTIVE (\s a -> s {_eicACTIVE = a}) . mapping _Nat

instance FromJSON EC2InstanceCounts where
  parseJSON =
    withObject
      "EC2InstanceCounts"
      ( \x ->
          EC2InstanceCounts'
            <$> (x .:? "IDLE")
            <*> (x .:? "TERMINATING")
            <*> (x .:? "PENDING")
            <*> (x .:? "MAXIMUM")
            <*> (x .:? "DESIRED")
            <*> (x .:? "MINIMUM")
            <*> (x .:? "ACTIVE")
      )

instance Hashable EC2InstanceCounts

instance NFData EC2InstanceCounts
