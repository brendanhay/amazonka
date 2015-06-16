{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribeElasticIPs
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

-- | Describes
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP addresses>.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeElasticIPs.html>
module Network.AWS.OpsWorks.DescribeElasticIPs
    (
    -- * Request
      DescribeElasticIPs
    -- ** Request constructor
    , describeElasticIPs
    -- ** Request lenses
    , deiInstanceId
    , deiIPs
    , deiStackId

    -- * Response
    , DescribeElasticIPsResponse
    -- ** Response constructor
    , describeElasticIPsResponse
    -- ** Response lenses
    , deirElasticIPs
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'describeElasticIPs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deiInstanceId'
--
-- * 'deiIPs'
--
-- * 'deiStackId'
data DescribeElasticIPs = DescribeElasticIPs'{_deiInstanceId :: Maybe Text, _deiIPs :: Maybe [Text], _deiStackId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeElasticIPs' smart constructor.
describeElasticIPs :: DescribeElasticIPs
describeElasticIPs = DescribeElasticIPs'{_deiInstanceId = Nothing, _deiIPs = Nothing, _deiStackId = Nothing};

-- | The instance ID. If you include this parameter, @DescribeElasticIps@
-- returns a description of the Elastic IP addresses associated with the
-- specified instance.
deiInstanceId :: Lens' DescribeElasticIPs (Maybe Text)
deiInstanceId = lens _deiInstanceId (\ s a -> s{_deiInstanceId = a});

-- | An array of Elastic IP addresses to be described. If you include this
-- parameter, @DescribeElasticIps@ returns a description of the specified
-- Elastic IP addresses. Otherwise, it returns a description of every
-- Elastic IP address.
deiIPs :: Lens' DescribeElasticIPs [Text]
deiIPs = lens _deiIPs (\ s a -> s{_deiIPs = a}) . _Default;

-- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns
-- a description of the Elastic IP addresses that are registered with the
-- specified stack.
deiStackId :: Lens' DescribeElasticIPs (Maybe Text)
deiStackId = lens _deiStackId (\ s a -> s{_deiStackId = a});

instance AWSRequest DescribeElasticIPs where
        type Sv DescribeElasticIPs = OpsWorks
        type Rs DescribeElasticIPs =
             DescribeElasticIPsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeElasticIPsResponse' <$>
                   (x .?> "ElasticIps" .!@ mempty))

instance ToHeaders DescribeElasticIPs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeElasticIPs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeElasticIPs where
        toJSON DescribeElasticIPs'{..}
          = object
              ["InstanceId" .= _deiInstanceId, "Ips" .= _deiIPs,
               "StackId" .= _deiStackId]

instance ToPath DescribeElasticIPs where
        toPath = const "/"

instance ToQuery DescribeElasticIPs where
        toQuery = const mempty

-- | /See:/ 'describeElasticIPsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deirElasticIPs'
newtype DescribeElasticIPsResponse = DescribeElasticIPsResponse'{_deirElasticIPs :: Maybe [ElasticIP]} deriving (Eq, Read, Show)

-- | 'DescribeElasticIPsResponse' smart constructor.
describeElasticIPsResponse :: DescribeElasticIPsResponse
describeElasticIPsResponse = DescribeElasticIPsResponse'{_deirElasticIPs = Nothing};

-- | An @ElasticIps@ object that describes the specified Elastic IP
-- addresses.
deirElasticIPs :: Lens' DescribeElasticIPsResponse [ElasticIP]
deirElasticIPs = lens _deirElasticIPs (\ s a -> s{_deirElasticIPs = a}) . _Default;
