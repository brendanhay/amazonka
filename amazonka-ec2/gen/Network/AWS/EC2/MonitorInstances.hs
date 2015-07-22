{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables monitoring for a running instance. For more information about
-- monitoring instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring Your Instances and Volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MonitorInstances.html>
module Network.AWS.EC2.MonitorInstances
    (
    -- * Request
      MonitorInstances
    -- ** Request constructor
    , monitorInstances
    -- ** Request lenses
    , mirqDryRun
    , mirqInstanceIds

    -- * Response
    , MonitorInstancesResponse
    -- ** Response constructor
    , monitorInstancesResponse
    -- ** Response lenses
    , mirsInstanceMonitorings
    , mirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'monitorInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mirqDryRun'
--
-- * 'mirqInstanceIds'
data MonitorInstances = MonitorInstances'
    { _mirqDryRun      :: !(Maybe Bool)
    , _mirqInstanceIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MonitorInstances' smart constructor.
monitorInstances :: MonitorInstances
monitorInstances =
    MonitorInstances'
    { _mirqDryRun = Nothing
    , _mirqInstanceIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
mirqDryRun :: Lens' MonitorInstances (Maybe Bool)
mirqDryRun = lens _mirqDryRun (\ s a -> s{_mirqDryRun = a});

-- | One or more instance IDs.
mirqInstanceIds :: Lens' MonitorInstances [Text]
mirqInstanceIds = lens _mirqInstanceIds (\ s a -> s{_mirqInstanceIds = a});

instance AWSRequest MonitorInstances where
        type Sv MonitorInstances = EC2
        type Rs MonitorInstances = MonitorInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 MonitorInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders MonitorInstances where
        toHeaders = const mempty

instance ToPath MonitorInstances where
        toPath = const "/"

instance ToQuery MonitorInstances where
        toQuery MonitorInstances'{..}
          = mconcat
              ["Action" =: ("MonitorInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _mirqDryRun,
               toQueryList "InstanceId" _mirqInstanceIds]

-- | /See:/ 'monitorInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mirsInstanceMonitorings'
--
-- * 'mirsStatus'
data MonitorInstancesResponse = MonitorInstancesResponse'
    { _mirsInstanceMonitorings :: !(Maybe [InstanceMonitoring])
    , _mirsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MonitorInstancesResponse' smart constructor.
monitorInstancesResponse :: Int -> MonitorInstancesResponse
monitorInstancesResponse pStatus =
    MonitorInstancesResponse'
    { _mirsInstanceMonitorings = Nothing
    , _mirsStatus = pStatus
    }

-- | Monitoring information for one or more instances.
mirsInstanceMonitorings :: Lens' MonitorInstancesResponse [InstanceMonitoring]
mirsInstanceMonitorings = lens _mirsInstanceMonitorings (\ s a -> s{_mirsInstanceMonitorings = a}) . _Default;

-- | FIXME: Undocumented member.
mirsStatus :: Lens' MonitorInstancesResponse Int
mirsStatus = lens _mirsStatus (\ s a -> s{_mirsStatus = a});
