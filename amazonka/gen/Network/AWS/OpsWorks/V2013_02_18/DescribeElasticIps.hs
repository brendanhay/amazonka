{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeElasticIps where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeElasticIps' request.
describeElasticIps :: DescribeElasticIps
describeElasticIps = DescribeElasticIps
    { _deitInstanceId = Nothing
    , _deitStackId = Nothing
    , _deitIps = mempty
    }

data DescribeElasticIps = DescribeElasticIps
    { _deitInstanceId :: Maybe Text
      -- ^ The instance ID. If you include this parameter,
      -- DescribeElasticIps returns a description of the Elastic IP
      -- addresses associated with the specified instance.
    , _deitStackId :: Maybe Text
      -- ^ A stack ID. If you include this parameter, DescribeElasticIps
      -- returns a description of the Elastic IP addresses that are
      -- registered with the specified stack.
    , _deitIps :: [Text]
      -- ^ An array of Elastic IP addresses to be described. If you include
      -- this parameter, DescribeElasticIps returns a description of the
      -- specified Elastic IP addresses. Otherwise, it returns a
      -- description of every Elastic IP address.
    } deriving (Generic)

makeLenses ''DescribeElasticIps

instance ToPath DescribeElasticIps

instance ToQuery DescribeElasticIps

instance ToHeaders DescribeElasticIps

instance ToJSON DescribeElasticIps

data DescribeElasticIpsResponse = DescribeElasticIpsResponse
    { _deiuElasticIps :: [ElasticIp]
      -- ^ An ElasticIps object that describes the specified Elastic IP
      -- addresses.
    } deriving (Generic)

makeLenses ''DescribeElasticIpsResponse

instance FromJSON DescribeElasticIpsResponse

instance AWSRequest DescribeElasticIps where
    type Sv DescribeElasticIps = OpsWorks
    type Rs DescribeElasticIps = DescribeElasticIpsResponse

    request = get
    response _ = jsonResponse
