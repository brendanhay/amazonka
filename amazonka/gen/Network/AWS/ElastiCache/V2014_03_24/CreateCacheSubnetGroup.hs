{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.CreateCacheSubnetGroup
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
-- &Version=2014-03-24 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 My new CacheSubnetGroup myCachesubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.ElastiCache.V2014_03_24.CreateCacheSubnetGroup where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElastiCache.V2014_03_24.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CreateCacheSubnetGroup = CreateCacheSubnetGroup
    { _ccsgnCacheSubnetGroupName :: Text
      -- ^ A name for the cache subnet group. This value is stored as a
      -- lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters or hyphens. Example: mysubnetgroup.
    , _ccsgnCacheSubnetGroupDescription :: Text
      -- ^ A description for the cache subnet group.
    , _ccsgnSubnetIds :: [Text]
      -- ^ A list of VPC subnet IDs for the cache subnet group.
    } deriving (Generic)

instance ToQuery CreateCacheSubnetGroup where
    toQuery = genericToQuery def

instance AWSRequest CreateCacheSubnetGroup where
    type Sv CreateCacheSubnetGroup = ElastiCache
    type Rs CreateCacheSubnetGroup = CreateCacheSubnetGroupResponse

    request = post "CreateCacheSubnetGroup"
    response _ = xmlResponse

data CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse
    { _csgzCacheSubnetGroup :: Maybe CacheSubnetGroup
      -- ^ Represents the output of one of the following operations:
      -- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
    } deriving (Generic)

instance FromXML CreateCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions
