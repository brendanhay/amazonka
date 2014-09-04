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
    , mkDescribeElasticIpsRequest
    -- ** Request lenses
    , deisInstanceId
    , deisStackId
    , deisIps

    -- * Response
    , DescribeElasticIpsResponse
    -- ** Response lenses
    , deitElasticIps
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeElasticIps' request.
mkDescribeElasticIpsRequest :: DescribeElasticIps
mkDescribeElasticIpsRequest = DescribeElasticIps
    { _deisInstanceId = Nothing
    , _deisStackId = Nothing
    , _deisIps = mempty
    }
{-# INLINE mkDescribeElasticIpsRequest #-}

data DescribeElasticIps = DescribeElasticIps
    { _deisInstanceId :: Maybe Text
      -- ^ The instance ID. If you include this parameter,
      -- DescribeElasticIps returns a description of the Elastic IP
      -- addresses associated with the specified instance.
    , _deisStackId :: Maybe Text
      -- ^ A stack ID. If you include this parameter, DescribeElasticIps
      -- returns a description of the Elastic IP addresses that are
      -- registered with the specified stack.
    , _deisIps :: [Text]
      -- ^ An array of Elastic IP addresses to be described. If you include
      -- this parameter, DescribeElasticIps returns a description of the
      -- specified Elastic IP addresses. Otherwise, it returns a
      -- description of every Elastic IP address.
    } deriving (Show, Generic)

-- | The instance ID. If you include this parameter, DescribeElasticIps returns
-- a description of the Elastic IP addresses associated with the specified
-- instance.
deisInstanceId :: Lens' DescribeElasticIps (Maybe Text)
deisInstanceId = lens _deisInstanceId (\s a -> s { _deisInstanceId = a })
{-# INLINE deisInstanceId #-}

-- | A stack ID. If you include this parameter, DescribeElasticIps returns a
-- description of the Elastic IP addresses that are registered with the
-- specified stack.
deisStackId :: Lens' DescribeElasticIps (Maybe Text)
deisStackId = lens _deisStackId (\s a -> s { _deisStackId = a })
{-# INLINE deisStackId #-}

-- | An array of Elastic IP addresses to be described. If you include this
-- parameter, DescribeElasticIps returns a description of the specified
-- Elastic IP addresses. Otherwise, it returns a description of every Elastic
-- IP address.
deisIps :: Lens' DescribeElasticIps ([Text])
deisIps = lens _deisIps (\s a -> s { _deisIps = a })
{-# INLINE deisIps #-}

instance ToPath DescribeElasticIps

instance ToQuery DescribeElasticIps

instance ToHeaders DescribeElasticIps

instance ToJSON DescribeElasticIps

newtype DescribeElasticIpsResponse = DescribeElasticIpsResponse
    { _deitElasticIps :: [ElasticIp]
      -- ^ An ElasticIps object that describes the specified Elastic IP
      -- addresses.
    } deriving (Show, Generic)

-- | An ElasticIps object that describes the specified Elastic IP addresses.
deitElasticIps :: Lens' DescribeElasticIpsResponse ([ElasticIp])
deitElasticIps = lens _deitElasticIps (\s a -> s { _deitElasticIps = a })
{-# INLINE deitElasticIps #-}

instance FromJSON DescribeElasticIpsResponse

instance AWSRequest DescribeElasticIps where
    type Sv DescribeElasticIps = OpsWorks
    type Rs DescribeElasticIps = DescribeElasticIpsResponse

    request = get
    response _ = jsonResponse
