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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables detailed monitoring for a running instance. Otherwise, basic monitoring is enabled. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring Your Instances and Volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- To disable detailed monitoring, see .
--
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
    , mirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for MonitorInstances.
--
--
--
-- /See:/ 'monitorInstances' smart constructor.
data MonitorInstances = MonitorInstances'
  { _miDryRun      :: !(Maybe Bool)
  , _miInstanceIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MonitorInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'miInstanceIds' - One or more instance IDs.
monitorInstances
    :: MonitorInstances
monitorInstances =
  MonitorInstances' {_miDryRun = Nothing, _miInstanceIds = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
miDryRun :: Lens' MonitorInstances (Maybe Bool)
miDryRun = lens _miDryRun (\ s a -> s{_miDryRun = a})

-- | One or more instance IDs.
miInstanceIds :: Lens' MonitorInstances [Text]
miInstanceIds = lens _miInstanceIds (\ s a -> s{_miInstanceIds = a}) . _Coerce

instance AWSRequest MonitorInstances where
        type Rs MonitorInstances = MonitorInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 MonitorInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable MonitorInstances where

instance NFData MonitorInstances where

instance ToHeaders MonitorInstances where
        toHeaders = const mempty

instance ToPath MonitorInstances where
        toPath = const "/"

instance ToQuery MonitorInstances where
        toQuery MonitorInstances'{..}
          = mconcat
              ["Action" =: ("MonitorInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _miDryRun,
               toQueryList "InstanceId" _miInstanceIds]

-- | Contains the output of MonitorInstances.
--
--
--
-- /See:/ 'monitorInstancesResponse' smart constructor.
data MonitorInstancesResponse = MonitorInstancesResponse'
  { _mirsInstanceMonitorings :: !(Maybe [InstanceMonitoring])
  , _mirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MonitorInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mirsInstanceMonitorings' - The monitoring information.
--
-- * 'mirsResponseStatus' - -- | The response status code.
monitorInstancesResponse
    :: Int -- ^ 'mirsResponseStatus'
    -> MonitorInstancesResponse
monitorInstancesResponse pResponseStatus_ =
  MonitorInstancesResponse'
    {_mirsInstanceMonitorings = Nothing, _mirsResponseStatus = pResponseStatus_}


-- | The monitoring information.
mirsInstanceMonitorings :: Lens' MonitorInstancesResponse [InstanceMonitoring]
mirsInstanceMonitorings = lens _mirsInstanceMonitorings (\ s a -> s{_mirsInstanceMonitorings = a}) . _Default . _Coerce

-- | -- | The response status code.
mirsResponseStatus :: Lens' MonitorInstancesResponse Int
mirsResponseStatus = lens _mirsResponseStatus (\ s a -> s{_mirsResponseStatus = a})

instance NFData MonitorInstancesResponse where
