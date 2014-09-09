{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeElasticIps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes Elastic IP addresses. You must specify at least one of the
-- parameters. Required Permissions: To use this action, an IAM user must have
-- a Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeElasticIps
    (
    -- * Request
      DescribeElasticIps
    -- ** Request constructor
    , mkDescribeElasticIps
    -- ** Request lenses
    , dei1InstanceId
    , dei1StackId
    , dei1Ips

    -- * Response
    , DescribeElasticIpsResponse
    -- ** Response constructor
    , mkDescribeElasticIpsResponse
    -- ** Response lenses
    , deirElasticIps
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeElasticIps = DescribeElasticIps
    { _dei1InstanceId :: Maybe Text
    , _dei1StackId :: Maybe Text
    , _dei1Ips :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeElasticIps' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @StackId ::@ @Maybe Text@
--
-- * @Ips ::@ @[Text]@
--
mkDescribeElasticIps :: DescribeElasticIps
mkDescribeElasticIps = DescribeElasticIps
    { _dei1InstanceId = Nothing
    , _dei1StackId = Nothing
    , _dei1Ips = mempty
    }

-- | The instance ID. If you include this parameter, DescribeElasticIps returns
-- a description of the Elastic IP addresses associated with the specified
-- instance.
dei1InstanceId :: Lens' DescribeElasticIps (Maybe Text)
dei1InstanceId = lens _dei1InstanceId (\s a -> s { _dei1InstanceId = a })

-- | A stack ID. If you include this parameter, DescribeElasticIps returns a
-- description of the Elastic IP addresses that are registered with the
-- specified stack.
dei1StackId :: Lens' DescribeElasticIps (Maybe Text)
dei1StackId = lens _dei1StackId (\s a -> s { _dei1StackId = a })

-- | An array of Elastic IP addresses to be described. If you include this
-- parameter, DescribeElasticIps returns a description of the specified
-- Elastic IP addresses. Otherwise, it returns a description of every Elastic
-- IP address.
dei1Ips :: Lens' DescribeElasticIps [Text]
dei1Ips = lens _dei1Ips (\s a -> s { _dei1Ips = a })

instance ToPath DescribeElasticIps

instance ToQuery DescribeElasticIps

instance ToHeaders DescribeElasticIps

instance ToJSON DescribeElasticIps

-- | Contains the response to a DescribeElasticIps request.
newtype DescribeElasticIpsResponse = DescribeElasticIpsResponse
    { _deirElasticIps :: [ElasticIp]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeElasticIpsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ElasticIps ::@ @[ElasticIp]@
--
mkDescribeElasticIpsResponse :: DescribeElasticIpsResponse
mkDescribeElasticIpsResponse = DescribeElasticIpsResponse
    { _deirElasticIps = mempty
    }

-- | An ElasticIps object that describes the specified Elastic IP addresses.
deirElasticIps :: Lens' DescribeElasticIpsResponse [ElasticIp]
deirElasticIps = lens _deirElasticIps (\s a -> s { _deirElasticIps = a })

instance FromJSON DescribeElasticIpsResponse

instance AWSRequest DescribeElasticIps where
    type Sv DescribeElasticIps = OpsWorks
    type Rs DescribeElasticIps = DescribeElasticIpsResponse

    request = get
    response _ = jsonResponse
