{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.RebootInstances
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

-- | Requests a reboot of one or more instances. This operation is
-- asynchronous; it only queues a request to reboot the specified
-- instances. The operation succeeds if the instances are valid and belong
-- to you. Requests to reboot terminated instances are ignored.
--
-- If a Linux\/Unix instance does not cleanly shut down within four
-- minutes, Amazon EC2 performs a hard reboot.
--
-- For more information about troubleshooting, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html Getting Console Output and Rebooting Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RebootInstances.html>
module Network.AWS.EC2.RebootInstances
    (
    -- * Request
      RebootInstances
    -- ** Request constructor
    , rebootInstances
    -- ** Request lenses
    , rebDryRun
    , rebInstanceIds

    -- * Response
    , RebootInstancesResponse
    -- ** Response constructor
    , rebootInstancesResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'rebootInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rebDryRun'
--
-- * 'rebInstanceIds'
data RebootInstances = RebootInstances'{_rebDryRun :: Maybe Bool, _rebInstanceIds :: [Text]} deriving (Eq, Read, Show)

-- | 'RebootInstances' smart constructor.
rebootInstances :: RebootInstances
rebootInstances = RebootInstances'{_rebDryRun = Nothing, _rebInstanceIds = mempty};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rebDryRun :: Lens' RebootInstances (Maybe Bool)
rebDryRun = lens _rebDryRun (\ s a -> s{_rebDryRun = a});

-- | One or more instance IDs.
rebInstanceIds :: Lens' RebootInstances [Text]
rebInstanceIds = lens _rebInstanceIds (\ s a -> s{_rebInstanceIds = a});

instance AWSRequest RebootInstances where
        type Sv RebootInstances = EC2
        type Rs RebootInstances = RebootInstancesResponse
        request = post
        response = receiveNull RebootInstancesResponse'

instance ToHeaders RebootInstances where
        toHeaders = const mempty

instance ToPath RebootInstances where
        toPath = const "/"

instance ToQuery RebootInstances where
        toQuery RebootInstances'{..}
          = mconcat
              ["Action" =: ("RebootInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _rebDryRun,
               toQueryList "InstanceId" _rebInstanceIds]

-- | /See:/ 'rebootInstancesResponse' smart constructor.
data RebootInstancesResponse = RebootInstancesResponse' deriving (Eq, Read, Show)

-- | 'RebootInstancesResponse' smart constructor.
rebootInstancesResponse :: RebootInstancesResponse
rebootInstancesResponse = RebootInstancesResponse';
