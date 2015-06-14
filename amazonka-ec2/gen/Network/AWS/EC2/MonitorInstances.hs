{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'monitorInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'miDryRun'
--
-- * 'miInstanceIds'
data MonitorInstances = MonitorInstances'{_miDryRun :: Maybe Bool, _miInstanceIds :: [Text]} deriving (Eq, Read, Show)

-- | 'MonitorInstances' smart constructor.
monitorInstances :: [Text] -> MonitorInstances
monitorInstances pInstanceIds = MonitorInstances'{_miDryRun = Nothing, _miInstanceIds = pInstanceIds};

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
                 MonitorInstancesResponse' <$> parseXMLList "item" x)

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
               "InstanceId" =: _miInstanceIds]

-- | /See:/ 'monitorInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mirInstanceMonitorings'
newtype MonitorInstancesResponse = MonitorInstancesResponse'{_mirInstanceMonitorings :: [InstanceMonitoring]} deriving (Eq, Read, Show)

-- | 'MonitorInstancesResponse' smart constructor.
monitorInstancesResponse :: MonitorInstancesResponse
monitorInstancesResponse = MonitorInstancesResponse'{_mirInstanceMonitorings = mempty};

-- | Monitoring information for one or more instances.
mirInstanceMonitorings :: Lens' MonitorInstancesResponse [InstanceMonitoring]
mirInstanceMonitorings = lens _mirInstanceMonitorings (\ s a -> s{_mirInstanceMonitorings = a});
