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
-- Module      : Network.AWS.EC2.UnmonitorInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables detailed monitoring for a running instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring Your Instances and Volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
module Network.AWS.EC2.UnmonitorInstances
    (
    -- * Creating a Request
      unmonitorInstances
    , UnmonitorInstances
    -- * Request Lenses
    , uiDryRun
    , uiInstanceIds

    -- * Destructuring the Response
    , unmonitorInstancesResponse
    , UnmonitorInstancesResponse
    -- * Response Lenses
    , uirsInstanceMonitorings
    , uirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for UnmonitorInstances.
--
--
--
-- /See:/ 'unmonitorInstances' smart constructor.
data UnmonitorInstances = UnmonitorInstances'
  { _uiDryRun      :: !(Maybe Bool)
  , _uiInstanceIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnmonitorInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'uiInstanceIds' - One or more instance IDs.
unmonitorInstances
    :: UnmonitorInstances
unmonitorInstances =
  UnmonitorInstances' {_uiDryRun = Nothing, _uiInstanceIds = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
uiDryRun :: Lens' UnmonitorInstances (Maybe Bool)
uiDryRun = lens _uiDryRun (\ s a -> s{_uiDryRun = a})

-- | One or more instance IDs.
uiInstanceIds :: Lens' UnmonitorInstances [Text]
uiInstanceIds = lens _uiInstanceIds (\ s a -> s{_uiInstanceIds = a}) . _Coerce

instance AWSRequest UnmonitorInstances where
        type Rs UnmonitorInstances =
             UnmonitorInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 UnmonitorInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable UnmonitorInstances where

instance NFData UnmonitorInstances where

instance ToHeaders UnmonitorInstances where
        toHeaders = const mempty

instance ToPath UnmonitorInstances where
        toPath = const "/"

instance ToQuery UnmonitorInstances where
        toQuery UnmonitorInstances'{..}
          = mconcat
              ["Action" =: ("UnmonitorInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _uiDryRun,
               toQueryList "InstanceId" _uiInstanceIds]

-- | Contains the output of UnmonitorInstances.
--
--
--
-- /See:/ 'unmonitorInstancesResponse' smart constructor.
data UnmonitorInstancesResponse = UnmonitorInstancesResponse'
  { _uirsInstanceMonitorings :: !(Maybe [InstanceMonitoring])
  , _uirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnmonitorInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uirsInstanceMonitorings' - The monitoring information.
--
-- * 'uirsResponseStatus' - -- | The response status code.
unmonitorInstancesResponse
    :: Int -- ^ 'uirsResponseStatus'
    -> UnmonitorInstancesResponse
unmonitorInstancesResponse pResponseStatus_ =
  UnmonitorInstancesResponse'
    {_uirsInstanceMonitorings = Nothing, _uirsResponseStatus = pResponseStatus_}


-- | The monitoring information.
uirsInstanceMonitorings :: Lens' UnmonitorInstancesResponse [InstanceMonitoring]
uirsInstanceMonitorings = lens _uirsInstanceMonitorings (\ s a -> s{_uirsInstanceMonitorings = a}) . _Default . _Coerce

-- | -- | The response status code.
uirsResponseStatus :: Lens' UnmonitorInstancesResponse Int
uirsResponseStatus = lens _uirsResponseStatus (\ s a -> s{_uirsResponseStatus = a})

instance NFData UnmonitorInstancesResponse where
