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
-- Module      : Network.AWS.EC2.StopInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon EBS-backed instance.
--
--
-- We don't charge usage for a stopped instance, or data transfer fees; however, your root partition Amazon EBS volume remains and continues to persist your data, and you are charged for Amazon EBS volume usage. Every time you start your Windows instance, Amazon EC2 charges you for a full instance hour. If you stop and restart your Windows instance, a new instance hour begins and Amazon EC2 charges you for another full instance hour even if you are still within the same 60-minute period when it was stopped. Every time you start your Linux instance, Amazon EC2 charges a one-minute minimum for instance usage, and thereafter charges per second for instance usage.
--
-- You can't start or stop Spot Instances, and you can't stop instance store-backed instances.
--
-- When you stop an instance, we shut it down. You can restart your instance at any time. Before stopping an instance, make sure it is in a state from which it can be restarted. Stopping an instance does not preserve data stored in RAM.
--
-- Stopping an instance is different to rebooting or terminating it. For example, when you stop an instance, the root device and any other devices attached to the instance persist. When you terminate an instance, the root device and any other devices attached during the instance launch are automatically deleted. For more information about the differences between rebooting, stopping, and terminating instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance Lifecycle> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- When you stop an instance, we attempt to shut it down forcibly after a short while. If your instance appears stuck in the stopping state after a period of time, there may be an issue with the underlying host computer. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesStopping.html Troubleshooting Stopping Your Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.StopInstances
    (
    -- * Creating a Request
      stopInstances
    , StopInstances
    -- * Request Lenses
    , siForce
    , siDryRun
    , siInstanceIds

    -- * Destructuring the Response
    , stopInstancesResponse
    , StopInstancesResponse
    -- * Response Lenses
    , sirsStoppingInstances
    , sirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for StopInstances.
--
--
--
-- /See:/ 'stopInstances' smart constructor.
data StopInstances = StopInstances'
  { _siForce       :: !(Maybe Bool)
  , _siDryRun      :: !(Maybe Bool)
  , _siInstanceIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siForce' - Forces the instances to stop. The instances do not have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures. This option is not recommended for Windows instances. Default: @false@
--
-- * 'siDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'siInstanceIds' - One or more instance IDs.
stopInstances
    :: StopInstances
stopInstances =
  StopInstances'
    {_siForce = Nothing, _siDryRun = Nothing, _siInstanceIds = mempty}


-- | Forces the instances to stop. The instances do not have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures. This option is not recommended for Windows instances. Default: @false@
siForce :: Lens' StopInstances (Maybe Bool)
siForce = lens _siForce (\ s a -> s{_siForce = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
siDryRun :: Lens' StopInstances (Maybe Bool)
siDryRun = lens _siDryRun (\ s a -> s{_siDryRun = a})

-- | One or more instance IDs.
siInstanceIds :: Lens' StopInstances [Text]
siInstanceIds = lens _siInstanceIds (\ s a -> s{_siInstanceIds = a}) . _Coerce

instance AWSRequest StopInstances where
        type Rs StopInstances = StopInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 StopInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable StopInstances where

instance NFData StopInstances where

instance ToHeaders StopInstances where
        toHeaders = const mempty

instance ToPath StopInstances where
        toPath = const "/"

instance ToQuery StopInstances where
        toQuery StopInstances'{..}
          = mconcat
              ["Action" =: ("StopInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Force" =: _siForce, "DryRun" =: _siDryRun,
               toQueryList "InstanceId" _siInstanceIds]

-- | Contains the output of StopInstances.
--
--
--
-- /See:/ 'stopInstancesResponse' smart constructor.
data StopInstancesResponse = StopInstancesResponse'
  { _sirsStoppingInstances :: !(Maybe [InstanceStateChange])
  , _sirsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirsStoppingInstances' - Information about one or more stopped instances.
--
-- * 'sirsResponseStatus' - -- | The response status code.
stopInstancesResponse
    :: Int -- ^ 'sirsResponseStatus'
    -> StopInstancesResponse
stopInstancesResponse pResponseStatus_ =
  StopInstancesResponse'
    {_sirsStoppingInstances = Nothing, _sirsResponseStatus = pResponseStatus_}


-- | Information about one or more stopped instances.
sirsStoppingInstances :: Lens' StopInstancesResponse [InstanceStateChange]
sirsStoppingInstances = lens _sirsStoppingInstances (\ s a -> s{_sirsStoppingInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
sirsResponseStatus :: Lens' StopInstancesResponse Int
sirsResponseStatus = lens _sirsResponseStatus (\ s a -> s{_sirsResponseStatus = a})

instance NFData StopInstancesResponse where
