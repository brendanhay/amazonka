{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.ResumeProcesses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Resumes the specified suspended Auto Scaling processes for the specified
-- Auto Scaling group. To resume specific processes, use the
-- @ScalingProcesses@ parameter. To resume all processes, omit the
-- @ScalingProcesses@ parameter. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US_SuspendResume.html Suspend and Resume Auto Scaling Processes>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ResumeProcesses.html>
module Network.AWS.AutoScaling.ResumeProcesses
    (
    -- * Request
      ResumeProcesses
    -- ** Request constructor
    , resumeProcesses
    -- ** Request lenses
    , rpScalingProcesses
    , rpAutoScalingGroupName

    -- * Response
    , ResumeProcessesResponse
    -- ** Response constructor
    , resumeProcessesResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'resumeProcesses' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpScalingProcesses'
--
-- * 'rpAutoScalingGroupName'
data ResumeProcesses = ResumeProcesses'
    { _rpScalingProcesses     :: !(Maybe [Text])
    , _rpAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResumeProcesses' smart constructor.
resumeProcesses :: Text -> ResumeProcesses
resumeProcesses pAutoScalingGroupName_ =
    ResumeProcesses'
    { _rpScalingProcesses = Nothing
    , _rpAutoScalingGroupName = pAutoScalingGroupName_
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
rpScalingProcesses :: Lens' ResumeProcesses [Text]
rpScalingProcesses = lens _rpScalingProcesses (\ s a -> s{_rpScalingProcesses = a}) . _Default;

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
rpAutoScalingGroupName :: Lens' ResumeProcesses Text
rpAutoScalingGroupName = lens _rpAutoScalingGroupName (\ s a -> s{_rpAutoScalingGroupName = a});

instance AWSRequest ResumeProcesses where
        type Sv ResumeProcesses = AutoScaling
        type Rs ResumeProcesses = ResumeProcessesResponse
        request = post
        response = receiveNull ResumeProcessesResponse'

instance ToHeaders ResumeProcesses where
        toHeaders = const mempty

instance ToPath ResumeProcesses where
        toPath = const "/"

instance ToQuery ResumeProcesses where
        toQuery ResumeProcesses'{..}
          = mconcat
              ["Action" =: ("ResumeProcesses" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "ScalingProcesses" =:
                 toQuery
                   (toQueryList "member" <$> _rpScalingProcesses),
               "AutoScalingGroupName" =: _rpAutoScalingGroupName]

-- | /See:/ 'resumeProcessesResponse' smart constructor.
data ResumeProcessesResponse =
    ResumeProcessesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResumeProcessesResponse' smart constructor.
resumeProcessesResponse :: ResumeProcessesResponse
resumeProcessesResponse = ResumeProcessesResponse'
