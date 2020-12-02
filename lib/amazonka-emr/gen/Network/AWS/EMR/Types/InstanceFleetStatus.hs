{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetStatus where

import Network.AWS.EMR.Types.InstanceFleetState
import Network.AWS.EMR.Types.InstanceFleetStateChangeReason
import Network.AWS.EMR.Types.InstanceFleetTimeline
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of the instance fleet.
--
--
--
-- /See:/ 'instanceFleetStatus' smart constructor.
data InstanceFleetStatus = InstanceFleetStatus'
  { _ifsState ::
      !(Maybe InstanceFleetState),
    _ifsStateChangeReason ::
      !(Maybe InstanceFleetStateChangeReason),
    _ifsTimeline :: !(Maybe InstanceFleetTimeline)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceFleetStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifsState' - A code representing the instance fleet status.     * @PROVISIONING@ —The instance fleet is provisioning EC2 resources and is not yet ready to run jobs.     * @BOOTSTRAPPING@ —EC2 instances and other resources have been provisioned and the bootstrap actions specified for the instances are underway.     * @RUNNING@ —EC2 instances and other resources are running. They are either executing jobs or waiting to execute jobs.     * @RESIZING@ —A resize operation is underway. EC2 instances are either being added or removed.     * @SUSPENDED@ —A resize operation could not complete. Existing EC2 instances are running, but instances can't be added or removed.     * @TERMINATING@ —The instance fleet is terminating EC2 instances.     * @TERMINATED@ —The instance fleet is no longer active, and all EC2 instances have been terminated.
--
-- * 'ifsStateChangeReason' - Provides status change reason details for the instance fleet.
--
-- * 'ifsTimeline' - Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
instanceFleetStatus ::
  InstanceFleetStatus
instanceFleetStatus =
  InstanceFleetStatus'
    { _ifsState = Nothing,
      _ifsStateChangeReason = Nothing,
      _ifsTimeline = Nothing
    }

-- | A code representing the instance fleet status.     * @PROVISIONING@ —The instance fleet is provisioning EC2 resources and is not yet ready to run jobs.     * @BOOTSTRAPPING@ —EC2 instances and other resources have been provisioned and the bootstrap actions specified for the instances are underway.     * @RUNNING@ —EC2 instances and other resources are running. They are either executing jobs or waiting to execute jobs.     * @RESIZING@ —A resize operation is underway. EC2 instances are either being added or removed.     * @SUSPENDED@ —A resize operation could not complete. Existing EC2 instances are running, but instances can't be added or removed.     * @TERMINATING@ —The instance fleet is terminating EC2 instances.     * @TERMINATED@ —The instance fleet is no longer active, and all EC2 instances have been terminated.
ifsState :: Lens' InstanceFleetStatus (Maybe InstanceFleetState)
ifsState = lens _ifsState (\s a -> s {_ifsState = a})

-- | Provides status change reason details for the instance fleet.
ifsStateChangeReason :: Lens' InstanceFleetStatus (Maybe InstanceFleetStateChangeReason)
ifsStateChangeReason = lens _ifsStateChangeReason (\s a -> s {_ifsStateChangeReason = a})

-- | Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
ifsTimeline :: Lens' InstanceFleetStatus (Maybe InstanceFleetTimeline)
ifsTimeline = lens _ifsTimeline (\s a -> s {_ifsTimeline = a})

instance FromJSON InstanceFleetStatus where
  parseJSON =
    withObject
      "InstanceFleetStatus"
      ( \x ->
          InstanceFleetStatus'
            <$> (x .:? "State")
            <*> (x .:? "StateChangeReason")
            <*> (x .:? "Timeline")
      )

instance Hashable InstanceFleetStatus

instance NFData InstanceFleetStatus
