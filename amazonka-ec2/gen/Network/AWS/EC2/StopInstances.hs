{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StopInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon EBS-backed instance. Each time you transition an
-- instance from stopped to started, Amazon EC2 charges a full instance
-- hour, even if transitions happen multiple times within a single hour.
--
-- You can\'t start or stop Spot Instances.
--
-- Instances that use Amazon EBS volumes as their root devices can be
-- quickly stopped and started. When an instance is stopped, the compute
-- resources are released and you are not billed for hourly instance usage.
-- However, your root partition Amazon EBS volume remains, continues to
-- persist your data, and you are charged for Amazon EBS volume usage. You
-- can restart your instance at any time.
--
-- Before stopping an instance, make sure it is in a state from which it
-- can be restarted. Stopping an instance does not preserve data stored in
-- RAM.
--
-- Performing this operation on an instance that uses an instance store as
-- its root device returns an error.
--
-- You can stop, start, and terminate EBS-backed instances. You can only
-- terminate instance store-backed instances. What happens to an instance
-- differs if you stop it or terminate it. For example, when you stop an
-- instance, the root device and any other devices attached to the instance
-- persist. When you terminate an instance, the root device and any other
-- devices attached during the instance launch are automatically deleted.
-- For more information about the differences between stopping and
-- terminating instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance Lifecycle>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For more information about troubleshooting, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesStopping.html Troubleshooting Stopping Your Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StopInstances.html>
module Network.AWS.EC2.StopInstances
    (
    -- * Request
      StopInstances
    -- ** Request constructor
    , stopInstances
    -- ** Request lenses
    , sirqForce
    , sirqDryRun
    , sirqInstanceIds

    -- * Response
    , StopInstancesResponse
    -- ** Response constructor
    , stopInstancesResponse
    -- ** Response lenses
    , sirsStoppingInstances
    , sirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'stopInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sirqForce'
--
-- * 'sirqDryRun'
--
-- * 'sirqInstanceIds'
data StopInstances = StopInstances'
    { _sirqForce       :: !(Maybe Bool)
    , _sirqDryRun      :: !(Maybe Bool)
    , _sirqInstanceIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StopInstances' smart constructor.
stopInstances :: StopInstances
stopInstances =
    StopInstances'
    { _sirqForce = Nothing
    , _sirqDryRun = Nothing
    , _sirqInstanceIds = mempty
    }

-- | Forces the instances to stop. The instances do not have an opportunity
-- to flush file system caches or file system metadata. If you use this
-- option, you must perform file system check and repair procedures. This
-- option is not recommended for Windows instances.
--
-- Default: @false@
sirqForce :: Lens' StopInstances (Maybe Bool)
sirqForce = lens _sirqForce (\ s a -> s{_sirqForce = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
sirqDryRun :: Lens' StopInstances (Maybe Bool)
sirqDryRun = lens _sirqDryRun (\ s a -> s{_sirqDryRun = a});

-- | One or more instance IDs.
sirqInstanceIds :: Lens' StopInstances [Text]
sirqInstanceIds = lens _sirqInstanceIds (\ s a -> s{_sirqInstanceIds = a});

instance AWSRequest StopInstances where
        type Sv StopInstances = EC2
        type Rs StopInstances = StopInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 StopInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders StopInstances where
        toHeaders = const mempty

instance ToPath StopInstances where
        toPath = const "/"

instance ToQuery StopInstances where
        toQuery StopInstances'{..}
          = mconcat
              ["Action" =: ("StopInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Force" =: _sirqForce, "DryRun" =: _sirqDryRun,
               toQueryList "InstanceId" _sirqInstanceIds]

-- | /See:/ 'stopInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sirsStoppingInstances'
--
-- * 'sirsStatus'
data StopInstancesResponse = StopInstancesResponse'
    { _sirsStoppingInstances :: !(Maybe [InstanceStateChange])
    , _sirsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StopInstancesResponse' smart constructor.
stopInstancesResponse :: Int -> StopInstancesResponse
stopInstancesResponse pStatus_ =
    StopInstancesResponse'
    { _sirsStoppingInstances = Nothing
    , _sirsStatus = pStatus_
    }

-- | Information about one or more stopped instances.
sirsStoppingInstances :: Lens' StopInstancesResponse [InstanceStateChange]
sirsStoppingInstances = lens _sirsStoppingInstances (\ s a -> s{_sirsStoppingInstances = a}) . _Default;

-- | FIXME: Undocumented member.
sirsStatus :: Lens' StopInstancesResponse Int
sirsStatus = lens _sirsStatus (\ s a -> s{_sirsStatus = a});
