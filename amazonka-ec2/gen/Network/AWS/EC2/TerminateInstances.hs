{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.TerminateInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Shuts down one or more instances. This operation is idempotent; if you
-- terminate an instance more than once, each call succeeds.
--
-- Terminated instances remain visible after termination (for approximately
-- one hour).
--
-- By default, Amazon EC2 deletes all EBS volumes that were attached when
-- the instance launched. Volumes attached after instance launch continue
-- running.
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesShuttingDown.html Troubleshooting Terminating Your Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-TerminateInstances.html>
module Network.AWS.EC2.TerminateInstances
    (
    -- * Request
      TerminateInstances
    -- ** Request constructor
    , terminateInstances
    -- ** Request lenses
    , tiDryRun
    , tiInstanceIds

    -- * Response
    , TerminateInstancesResponse
    -- ** Response constructor
    , terminateInstancesResponse
    -- ** Response lenses
    , tirsTerminatingInstances
    , tirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'terminateInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tiDryRun'
--
-- * 'tiInstanceIds'
data TerminateInstances = TerminateInstances'
    { _tiDryRun      :: !(Maybe Bool)
    , _tiInstanceIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TerminateInstances' smart constructor.
terminateInstances :: TerminateInstances
terminateInstances =
    TerminateInstances'
    { _tiDryRun = Nothing
    , _tiInstanceIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
tiDryRun :: Lens' TerminateInstances (Maybe Bool)
tiDryRun = lens _tiDryRun (\ s a -> s{_tiDryRun = a});

-- | One or more instance IDs.
tiInstanceIds :: Lens' TerminateInstances [Text]
tiInstanceIds = lens _tiInstanceIds (\ s a -> s{_tiInstanceIds = a});

instance AWSRequest TerminateInstances where
        type Sv TerminateInstances = EC2
        type Rs TerminateInstances =
             TerminateInstancesResponse
        request = post "TerminateInstances"
        response
          = receiveXML
              (\ s h x ->
                 TerminateInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders TerminateInstances where
        toHeaders = const mempty

instance ToPath TerminateInstances where
        toPath = const "/"

instance ToQuery TerminateInstances where
        toQuery TerminateInstances'{..}
          = mconcat
              ["Action" =: ("TerminateInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _tiDryRun,
               toQueryList "InstanceId" _tiInstanceIds]

-- | /See:/ 'terminateInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tirsTerminatingInstances'
--
-- * 'tirsStatus'
data TerminateInstancesResponse = TerminateInstancesResponse'
    { _tirsTerminatingInstances :: !(Maybe [InstanceStateChange])
    , _tirsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TerminateInstancesResponse' smart constructor.
terminateInstancesResponse :: Int -> TerminateInstancesResponse
terminateInstancesResponse pStatus_ =
    TerminateInstancesResponse'
    { _tirsTerminatingInstances = Nothing
    , _tirsStatus = pStatus_
    }

-- | Information about one or more terminated instances.
tirsTerminatingInstances :: Lens' TerminateInstancesResponse [InstanceStateChange]
tirsTerminatingInstances = lens _tirsTerminatingInstances (\ s a -> s{_tirsTerminatingInstances = a}) . _Default;

-- | FIXME: Undocumented member.
tirsStatus :: Lens' TerminateInstancesResponse Int
tirsStatus = lens _tirsStatus (\ s a -> s{_tirsStatus = a});
