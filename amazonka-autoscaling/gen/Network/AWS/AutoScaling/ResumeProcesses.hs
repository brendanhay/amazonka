{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.ResumeProcesses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes the specified suspended Auto Scaling processes, or all suspended process, for the specified Auto Scaling group.
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-suspend-resume-processes.html Suspending and Resuming Auto Scaling Processes> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.ResumeProcesses
    (
    -- * Creating a Request
      resumeProcesses
    , ResumeProcesses
    -- * Request Lenses
    , rpScalingProcesses
    , rpAutoScalingGroupName

    -- * Destructuring the Response
    , resumeProcessesResponse
    , ResumeProcessesResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resumeProcesses' smart constructor.
data ResumeProcesses = ResumeProcesses'
  { _rpScalingProcesses     :: !(Maybe [Text])
  , _rpAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResumeProcesses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpScalingProcesses' - One or more of the following processes. If you omit this parameter, all processes are specified.     * @Launch@      * @Terminate@      * @HealthCheck@      * @ReplaceUnhealthy@      * @AZRebalance@      * @AlarmNotification@      * @ScheduledActions@      * @AddToLoadBalancer@
--
-- * 'rpAutoScalingGroupName' - The name of the Auto Scaling group.
resumeProcesses
    :: Text -- ^ 'rpAutoScalingGroupName'
    -> ResumeProcesses
resumeProcesses pAutoScalingGroupName_ =
  ResumeProcesses'
    { _rpScalingProcesses = Nothing
    , _rpAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | One or more of the following processes. If you omit this parameter, all processes are specified.     * @Launch@      * @Terminate@      * @HealthCheck@      * @ReplaceUnhealthy@      * @AZRebalance@      * @AlarmNotification@      * @ScheduledActions@      * @AddToLoadBalancer@
rpScalingProcesses :: Lens' ResumeProcesses [Text]
rpScalingProcesses = lens _rpScalingProcesses (\ s a -> s{_rpScalingProcesses = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
rpAutoScalingGroupName :: Lens' ResumeProcesses Text
rpAutoScalingGroupName = lens _rpAutoScalingGroupName (\ s a -> s{_rpAutoScalingGroupName = a})

instance AWSRequest ResumeProcesses where
        type Rs ResumeProcesses = ResumeProcessesResponse
        request = postQuery autoScaling
        response = receiveNull ResumeProcessesResponse'

instance Hashable ResumeProcesses where

instance NFData ResumeProcesses where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResumeProcessesResponse' with the minimum fields required to make a request.
--
resumeProcessesResponse
    :: ResumeProcessesResponse
resumeProcessesResponse = ResumeProcessesResponse'


instance NFData ResumeProcessesResponse where
