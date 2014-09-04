{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.ElastiCache.V2014_07_15.CreateCacheSubnetGroup
    (
    -- * Request
      CreateCacheSubnetGroup
    -- ** Request constructor
    , createCacheSubnetGroup
    -- ** Request lenses
    , ccsgnCacheSubnetGroupName
    , ccsgnCacheSubnetGroupDescription
    , ccsgnSubnetIds

    -- * Response
    , CreateCacheSubnetGroupResponse
    -- ** Response lenses
    , csgyCacheSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateCacheSubnetGroup' request.
createCacheSubnetGroup :: Text -- ^ 'ccsgnCacheSubnetGroupName'
                       -> Text -- ^ 'ccsgnCacheSubnetGroupDescription'
                       -> [Text] -- ^ 'ccsgnSubnetIds'
                       -> CreateCacheSubnetGroup
createCacheSubnetGroup p1 p2 p3 = CreateCacheSubnetGroup
    { _ccsgnCacheSubnetGroupName = p1
    , _ccsgnCacheSubnetGroupDescription = p2
    , _ccsgnSubnetIds = p3
    }
{-# INLINE createCacheSubnetGroup #-}

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

-- | A name for the cache subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric characters
-- or hyphens. Example: mysubnetgroup.
ccsgnCacheSubnetGroupName :: Lens' CreateCacheSubnetGroup (Text)
ccsgnCacheSubnetGroupName f x =
    f (_ccsgnCacheSubnetGroupName x)
        <&> \y -> x { _ccsgnCacheSubnetGroupName = y }
{-# INLINE ccsgnCacheSubnetGroupName #-}

-- | A description for the cache subnet group.
ccsgnCacheSubnetGroupDescription :: Lens' CreateCacheSubnetGroup (Text)
ccsgnCacheSubnetGroupDescription f x =
    f (_ccsgnCacheSubnetGroupDescription x)
        <&> \y -> x { _ccsgnCacheSubnetGroupDescription = y }
{-# INLINE ccsgnCacheSubnetGroupDescription #-}

-- | A list of VPC subnet IDs for the cache subnet group.
ccsgnSubnetIds :: Lens' CreateCacheSubnetGroup ([Text])
ccsgnSubnetIds f x =
    f (_ccsgnSubnetIds x)
        <&> \y -> x { _ccsgnSubnetIds = y }
{-# INLINE ccsgnSubnetIds #-}

instance ToQuery CreateCacheSubnetGroup where
    toQuery = genericQuery def

data CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse
    { _csgyCacheSubnetGroup :: Maybe CacheSubnetGroup
      -- ^ Represents the output of one of the following operations:
      -- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
    } deriving (Show, Generic)

-- | Represents the output of one of the following operations:
-- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
csgyCacheSubnetGroup :: Lens' CreateCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
csgyCacheSubnetGroup f x =
    f (_csgyCacheSubnetGroup x)
        <&> \y -> x { _csgyCacheSubnetGroup = y }
{-# INLINE csgyCacheSubnetGroup #-}

instance FromXML CreateCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheSubnetGroup where
    type Sv CreateCacheSubnetGroup = ElastiCache
    type Rs CreateCacheSubnetGroup = CreateCacheSubnetGroupResponse

    request = post "CreateCacheSubnetGroup"
    response _ = xmlResponse
