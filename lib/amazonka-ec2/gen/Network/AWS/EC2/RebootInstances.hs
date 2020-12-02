{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RebootInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a reboot of the specified instances. This operation is asynchronous; it only queues a request to reboot the specified instances. The operation succeeds if the instances are valid and belong to you. Requests to reboot terminated instances are ignored.
--
--
-- If an instance does not cleanly shut down within a few minutes, Amazon EC2 performs a hard reboot.
--
-- For more information about troubleshooting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html Getting console output and rebooting instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.RebootInstances
  ( -- * Creating a Request
    rebootInstances,
    RebootInstances,

    -- * Request Lenses
    rDryRun,
    rInstanceIds,

    -- * Destructuring the Response
    rebootInstancesResponse,
    RebootInstancesResponse,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rebootInstances' smart constructor.
data RebootInstances = RebootInstances'
  { _rDryRun :: !(Maybe Bool),
    _rInstanceIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebootInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rInstanceIds' - The instance IDs.
rebootInstances ::
  RebootInstances
rebootInstances =
  RebootInstances' {_rDryRun = Nothing, _rInstanceIds = mempty}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rDryRun :: Lens' RebootInstances (Maybe Bool)
rDryRun = lens _rDryRun (\s a -> s {_rDryRun = a})

-- | The instance IDs.
rInstanceIds :: Lens' RebootInstances [Text]
rInstanceIds = lens _rInstanceIds (\s a -> s {_rInstanceIds = a}) . _Coerce

instance AWSRequest RebootInstances where
  type Rs RebootInstances = RebootInstancesResponse
  request = postQuery ec2
  response = receiveNull RebootInstancesResponse'

instance Hashable RebootInstances

instance NFData RebootInstances

instance ToHeaders RebootInstances where
  toHeaders = const mempty

instance ToPath RebootInstances where
  toPath = const "/"

instance ToQuery RebootInstances where
  toQuery RebootInstances' {..} =
    mconcat
      [ "Action" =: ("RebootInstances" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _rDryRun,
        toQueryList "InstanceId" _rInstanceIds
      ]

-- | /See:/ 'rebootInstancesResponse' smart constructor.
data RebootInstancesResponse = RebootInstancesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebootInstancesResponse' with the minimum fields required to make a request.
rebootInstancesResponse ::
  RebootInstancesResponse
rebootInstancesResponse = RebootInstancesResponse'

instance NFData RebootInstancesResponse
