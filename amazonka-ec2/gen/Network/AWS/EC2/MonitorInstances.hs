{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables monitoring for a running instance. For more information about
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
    , miDryRun
    , miInstanceIds

    -- * Response
    , MonitorInstancesResponse
    -- ** Response constructor
    , monitorInstancesResponse
    -- ** Response lenses
    , mirInstanceMonitorings
    , mirStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'monitorInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'miDryRun'
--
-- * 'miInstanceIds'
data MonitorInstances = MonitorInstances'
    { _miDryRun      :: !(Maybe Bool)
    , _miInstanceIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MonitorInstances' smart constructor.
monitorInstances :: MonitorInstances
monitorInstances =
    MonitorInstances'
    { _miDryRun = Nothing
    , _miInstanceIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
miDryRun :: Lens' MonitorInstances (Maybe Bool)
miDryRun = lens _miDryRun (\ s a -> s{_miDryRun = a});

-- | One or more instance IDs.
miInstanceIds :: Lens' MonitorInstances [Text]
miInstanceIds = lens _miInstanceIds (\ s a -> s{_miInstanceIds = a});

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
               "DryRun" =: _miDryRun,
               toQueryList "InstanceId" _miInstanceIds]

-- | /See:/ 'monitorInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mirInstanceMonitorings'
--
-- * 'mirStatus'
data MonitorInstancesResponse = MonitorInstancesResponse'
    { _mirInstanceMonitorings :: !(Maybe [InstanceMonitoring])
    , _mirStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MonitorInstancesResponse' smart constructor.
monitorInstancesResponse :: Int -> MonitorInstancesResponse
monitorInstancesResponse pStatus =
    MonitorInstancesResponse'
    { _mirInstanceMonitorings = Nothing
    , _mirStatus = pStatus
    }

-- | Monitoring information for one or more instances.
mirInstanceMonitorings :: Lens' MonitorInstancesResponse [InstanceMonitoring]
mirInstanceMonitorings = lens _mirInstanceMonitorings (\ s a -> s{_mirInstanceMonitorings = a}) . _Default;

-- | FIXME: Undocumented member.
mirStatus :: Lens' MonitorInstancesResponse Int
mirStatus = lens _mirStatus (\ s a -> s{_mirStatus = a});
