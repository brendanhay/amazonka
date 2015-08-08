{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SuspendProcesses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends the specified Auto Scaling processes for the specified Auto
-- Scaling group. To suspend specific processes, use the @ScalingProcesses@
-- parameter. To suspend all processes, omit the @ScalingProcesses@
-- parameter.
--
-- Note that if you suspend either the @Launch@ or @Terminate@ process
-- types, it can prevent other process types from functioning properly.
--
-- To resume processes that have been suspended, use ResumeProcesses.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US_SuspendResume.html Suspend and Resume Auto Scaling Processes>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SuspendProcesses.html AWS API Reference> for SuspendProcesses.
module Network.AWS.AutoScaling.SuspendProcesses
    (
    -- * Creating a Request
      SuspendProcesses
    , suspendProcesses
    -- * Request Lenses
    , spScalingProcesses
    , spAutoScalingGroupName

    -- * Destructuring the Response
    , SuspendProcessesResponse
    , suspendProcessesResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'suspendProcesses' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spScalingProcesses'
--
-- * 'spAutoScalingGroupName'
data SuspendProcesses = SuspendProcesses'
    { _spScalingProcesses     :: !(Maybe [Text])
    , _spAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SuspendProcesses' smart constructor.
suspendProcesses :: Text -> SuspendProcesses
suspendProcesses pAutoScalingGroupName_ =
    SuspendProcesses'
    { _spScalingProcesses = Nothing
    , _spAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more of the following processes:
--
-- -   @Launch@
--
-- -   @Terminate@
--
-- -   @HealthCheck@
--
-- -   @ReplaceUnhealthy@
--
-- -   @AZRebalance@
--
-- -   @AlarmNotification@
--
-- -   @ScheduledActions@
--
-- -   @AddToLoadBalancer@
--
spScalingProcesses :: Lens' SuspendProcesses [Text]
spScalingProcesses = lens _spScalingProcesses (\ s a -> s{_spScalingProcesses = a}) . _Default . _Coerce;

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
spAutoScalingGroupName :: Lens' SuspendProcesses Text
spAutoScalingGroupName = lens _spAutoScalingGroupName (\ s a -> s{_spAutoScalingGroupName = a});

instance AWSRequest SuspendProcesses where
        type Sv SuspendProcesses = AutoScaling
        type Rs SuspendProcesses = SuspendProcessesResponse
        request = postQuery
        response = receiveNull SuspendProcessesResponse'

instance ToHeaders SuspendProcesses where
        toHeaders = const mempty

instance ToPath SuspendProcesses where
        toPath = const "/"

instance ToQuery SuspendProcesses where
        toQuery SuspendProcesses'{..}
          = mconcat
              ["Action" =: ("SuspendProcesses" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "ScalingProcesses" =:
                 toQuery
                   (toQueryList "member" <$> _spScalingProcesses),
               "AutoScalingGroupName" =: _spAutoScalingGroupName]

-- | /See:/ 'suspendProcessesResponse' smart constructor.
data SuspendProcessesResponse =
    SuspendProcessesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SuspendProcessesResponse' smart constructor.
suspendProcessesResponse :: SuspendProcessesResponse
suspendProcessesResponse = SuspendProcessesResponse'
