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
-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables monitoring for a running instance. For more information about
-- monitoring instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring Your Instances and Volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MonitorInstances.html AWS API Reference> for MonitorInstances.
module Network.AWS.EC2.MonitorInstances
    (
    -- * Creating a Request
      monitorInstances
    , MonitorInstances
    -- * Request Lenses
    , miDryRun
    , miInstanceIds

    -- * Destructuring the Response
    , monitorInstancesResponse
    , MonitorInstancesResponse
    -- * Response Lenses
    , mirsInstanceMonitorings
    , mirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'monitorInstances' smart constructor.
data MonitorInstances = MonitorInstances'
    { _miDryRun      :: !(Maybe Bool)
    , _miInstanceIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MonitorInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miDryRun'
--
-- * 'miInstanceIds'
monitorInstances
    :: MonitorInstances
monitorInstances =
    MonitorInstances'
    { _miDryRun = Nothing
    , _miInstanceIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
miDryRun :: Lens' MonitorInstances (Maybe Bool)
miDryRun = lens _miDryRun (\ s a -> s{_miDryRun = a});

-- | One or more instance IDs.
miInstanceIds :: Lens' MonitorInstances [Text]
miInstanceIds = lens _miInstanceIds (\ s a -> s{_miInstanceIds = a}) . _Coerce;

instance AWSRequest MonitorInstances where
        type Rs MonitorInstances = MonitorInstancesResponse
        request = postQuery eC2
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
data MonitorInstancesResponse = MonitorInstancesResponse'
    { _mirsInstanceMonitorings :: !(Maybe [InstanceMonitoring])
    , _mirsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MonitorInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mirsInstanceMonitorings'
--
-- * 'mirsStatus'
monitorInstancesResponse
    :: Int -- ^ 'mirsStatus'
    -> MonitorInstancesResponse
monitorInstancesResponse pStatus_ =
    MonitorInstancesResponse'
    { _mirsInstanceMonitorings = Nothing
    , _mirsStatus = pStatus_
    }

-- | Monitoring information for one or more instances.
mirsInstanceMonitorings :: Lens' MonitorInstancesResponse [InstanceMonitoring]
mirsInstanceMonitorings = lens _mirsInstanceMonitorings (\ s a -> s{_mirsInstanceMonitorings = a}) . _Default . _Coerce;

-- | The response status code.
mirsStatus :: Lens' MonitorInstancesResponse Int
mirsStatus = lens _mirsStatus (\ s a -> s{_mirsStatus = a});
