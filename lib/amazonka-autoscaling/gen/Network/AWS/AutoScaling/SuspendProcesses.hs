{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SuspendProcesses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends the specified auto scaling processes, or all processes, for the specified Auto Scaling group.
--
--
-- If you suspend either the @Launch@ or @Terminate@ process types, it can prevent other process types from functioning properly. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html Suspending and resuming scaling processes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- To resume processes that have been suspended, call the 'ResumeProcesses' API.
module Network.AWS.AutoScaling.SuspendProcesses
  ( -- * Creating a Request
    suspendProcesses,
    SuspendProcesses,

    -- * Request Lenses
    spScalingProcesses,
    spAutoScalingGroupName,

    -- * Destructuring the Response
    suspendProcessesResponse,
    SuspendProcessesResponse,
  )
where

import Network.AWS.AutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'suspendProcesses' smart constructor.
data SuspendProcesses = SuspendProcesses'
  { _spScalingProcesses ::
      !(Maybe [Text]),
    _spAutoScalingGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuspendProcesses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spScalingProcesses' - One or more of the following processes:     * @Launch@      * @Terminate@      * @AddToLoadBalancer@      * @AlarmNotification@      * @AZRebalance@      * @HealthCheck@      * @InstanceRefresh@      * @ReplaceUnhealthy@      * @ScheduledActions@  If you omit this parameter, all processes are specified.
--
-- * 'spAutoScalingGroupName' - The name of the Auto Scaling group.
suspendProcesses ::
  -- | 'spAutoScalingGroupName'
  Text ->
  SuspendProcesses
suspendProcesses pAutoScalingGroupName_ =
  SuspendProcesses'
    { _spScalingProcesses = Nothing,
      _spAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more of the following processes:     * @Launch@      * @Terminate@      * @AddToLoadBalancer@      * @AlarmNotification@      * @AZRebalance@      * @HealthCheck@      * @InstanceRefresh@      * @ReplaceUnhealthy@      * @ScheduledActions@  If you omit this parameter, all processes are specified.
spScalingProcesses :: Lens' SuspendProcesses [Text]
spScalingProcesses = lens _spScalingProcesses (\s a -> s {_spScalingProcesses = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
spAutoScalingGroupName :: Lens' SuspendProcesses Text
spAutoScalingGroupName = lens _spAutoScalingGroupName (\s a -> s {_spAutoScalingGroupName = a})

instance AWSRequest SuspendProcesses where
  type Rs SuspendProcesses = SuspendProcessesResponse
  request = postQuery autoScaling
  response = receiveNull SuspendProcessesResponse'

instance Hashable SuspendProcesses

instance NFData SuspendProcesses

instance ToHeaders SuspendProcesses where
  toHeaders = const mempty

instance ToPath SuspendProcesses where
  toPath = const "/"

instance ToQuery SuspendProcesses where
  toQuery SuspendProcesses' {..} =
    mconcat
      [ "Action" =: ("SuspendProcesses" :: ByteString),
        "Version" =: ("2011-01-01" :: ByteString),
        "ScalingProcesses"
          =: toQuery (toQueryList "member" <$> _spScalingProcesses),
        "AutoScalingGroupName" =: _spAutoScalingGroupName
      ]

-- | /See:/ 'suspendProcessesResponse' smart constructor.
data SuspendProcessesResponse = SuspendProcessesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuspendProcessesResponse' with the minimum fields required to make a request.
suspendProcessesResponse ::
  SuspendProcessesResponse
suspendProcessesResponse = SuspendProcessesResponse'

instance NFData SuspendProcessesResponse
