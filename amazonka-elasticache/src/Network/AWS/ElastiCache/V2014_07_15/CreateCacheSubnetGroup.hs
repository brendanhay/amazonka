{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.CreateCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheSubnetGroup operation creates a new cache subnet group. Use
-- this parameter only when you are creating a cluster in an Amazon Virtual
-- Private Cloud (VPC). https://elasticache.amazonaws.com/
-- ?Action=CreateCacheSubnetGroup &CacheSubnetGroupName=myCachesubnetgroup
-- &CacheSubnetGroupDescription=My%20new%20CacheSubnetGroup
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 990524496922 My new
-- CacheSubnetGroup myCachesubnetgroup Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.ElastiCache.V2014_07_15.CreateCacheSubnetGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

data CreateCacheSubnetGroup = CreateCacheSubnetGroup
    { _ccsgnCacheSubnetGroupName :: Text
      -- ^ A name for the cache subnet group. This value is stored as a
      -- lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters or hyphens. Example: mysubnetgroup.
    , _ccsgnCacheSubnetGroupDescription :: Text
      -- ^ A description for the cache subnet group.
    , _ccsgnSubnetIds :: [Text]
      -- ^ A list of VPC subnet IDs for the cache subnet group.
    } deriving (Show, Generic)

makeLenses ''CreateCacheSubnetGroup

instance ToQuery CreateCacheSubnetGroup where
    toQuery = genericQuery def

data CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse
    { _csgzCacheSubnetGroup :: Maybe CacheSubnetGroup
      -- ^ Represents the output of one of the following operations:
      -- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
    } deriving (Show, Generic)

makeLenses ''CreateCacheSubnetGroupResponse

instance FromXML CreateCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheSubnetGroup where
    type Sv CreateCacheSubnetGroup = ElastiCache
    type Rs CreateCacheSubnetGroup = CreateCacheSubnetGroupResponse

    request = post "CreateCacheSubnetGroup"
    response _ = xmlResponse
