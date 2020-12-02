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
-- Module      : Network.AWS.EC2.TerminateInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shuts down one or more instances. This operation is idempotent; if you terminate an instance more than once, each call succeeds.
--
--
-- If you specify multiple instances and the request fails (for example, because of a single incorrect instance ID), none of the instances are terminated.
--
-- Terminated instances remain visible after termination (for approximately one hour).
--
-- By default, Amazon EC2 deletes all EBS volumes that were attached when the instance launched. Volumes attached after instance launch continue running.
--
-- You can stop, start, and terminate EBS-backed instances. You can only terminate instance store-backed instances. What happens to an instance differs if you stop it or terminate it. For example, when you stop an instance, the root device and any other devices attached to the instance persist. When you terminate an instance, any attached EBS volumes with the @DeleteOnTermination@ block device mapping parameter set to @true@ are automatically deleted. For more information about the differences between stopping and terminating instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance Lifecycle> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- For more information about troubleshooting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesShuttingDown.html Troubleshooting Terminating Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.TerminateInstances
    (
    -- * Creating a Request
      terminateInstances
    , TerminateInstances
    -- * Request Lenses
    , tiDryRun
    , tiInstanceIds

    -- * Destructuring the Response
    , terminateInstancesResponse
    , TerminateInstancesResponse
    -- * Response Lenses
    , tirsTerminatingInstances
    , tirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for TerminateInstances.
--
--
--
-- /See:/ 'terminateInstances' smart constructor.
data TerminateInstances = TerminateInstances'
  { _tiDryRun      :: !(Maybe Bool)
  , _tiInstanceIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'tiInstanceIds' - One or more instance IDs. Constraints: Up to 1000 instance IDs. We recommend breaking up this request into smaller batches.
terminateInstances
    :: TerminateInstances
terminateInstances =
  TerminateInstances' {_tiDryRun = Nothing, _tiInstanceIds = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
tiDryRun :: Lens' TerminateInstances (Maybe Bool)
tiDryRun = lens _tiDryRun (\ s a -> s{_tiDryRun = a})

-- | One or more instance IDs. Constraints: Up to 1000 instance IDs. We recommend breaking up this request into smaller batches.
tiInstanceIds :: Lens' TerminateInstances [Text]
tiInstanceIds = lens _tiInstanceIds (\ s a -> s{_tiInstanceIds = a}) . _Coerce

instance AWSRequest TerminateInstances where
        type Rs TerminateInstances =
             TerminateInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 TerminateInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable TerminateInstances where

instance NFData TerminateInstances where

instance ToHeaders TerminateInstances where
        toHeaders = const mempty

instance ToPath TerminateInstances where
        toPath = const "/"

instance ToQuery TerminateInstances where
        toQuery TerminateInstances'{..}
          = mconcat
              ["Action" =: ("TerminateInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _tiDryRun,
               toQueryList "InstanceId" _tiInstanceIds]

-- | Contains the output of TerminateInstances.
--
--
--
-- /See:/ 'terminateInstancesResponse' smart constructor.
data TerminateInstancesResponse = TerminateInstancesResponse'
  { _tirsTerminatingInstances :: !(Maybe [InstanceStateChange])
  , _tirsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tirsTerminatingInstances' - Information about one or more terminated instances.
--
-- * 'tirsResponseStatus' - -- | The response status code.
terminateInstancesResponse
    :: Int -- ^ 'tirsResponseStatus'
    -> TerminateInstancesResponse
terminateInstancesResponse pResponseStatus_ =
  TerminateInstancesResponse'
    { _tirsTerminatingInstances = Nothing
    , _tirsResponseStatus = pResponseStatus_
    }


-- | Information about one or more terminated instances.
tirsTerminatingInstances :: Lens' TerminateInstancesResponse [InstanceStateChange]
tirsTerminatingInstances = lens _tirsTerminatingInstances (\ s a -> s{_tirsTerminatingInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
tirsResponseStatus :: Lens' TerminateInstancesResponse Int
tirsResponseStatus = lens _tirsResponseStatus (\ s a -> s{_tirsResponseStatus = a})

instance NFData TerminateInstancesResponse where
