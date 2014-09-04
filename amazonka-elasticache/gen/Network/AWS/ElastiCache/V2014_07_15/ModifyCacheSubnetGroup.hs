{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheSubnetGroup operation modifies an existing cache subnet
-- group. https://elasticache.amazonaws.com/ ?Action=ModifyCacheSubnetGroup
-- &CacheSubnetGroupName=myCachesubnetgroup
-- &CacheSubnetGroupDescription=My%20modified%20CacheSubnetGroup
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 990524496922 My modified
-- CacheSubnetGroup myCachesubnetgroup Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.ElastiCache.V2014_07_15.ModifyCacheSubnetGroup
    (
    -- * Request
      ModifyCacheSubnetGroup
    -- ** Request constructor
    , modifyCacheSubnetGroup
    -- ** Request lenses
    , mcsgmCacheSubnetGroupName
    , mcsgmCacheSubnetGroupDescription
    , mcsgmSubnetIds

    -- * Response
    , ModifyCacheSubnetGroupResponse
    -- ** Response lenses
    , csgzCacheSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyCacheSubnetGroup' request.
modifyCacheSubnetGroup :: Text -- ^ 'mcsgmCacheSubnetGroupName'
                       -> ModifyCacheSubnetGroup
modifyCacheSubnetGroup p1 = ModifyCacheSubnetGroup
    { _mcsgmCacheSubnetGroupName = p1
    , _mcsgmCacheSubnetGroupDescription = Nothing
    , _mcsgmSubnetIds = mempty
    }
{-# INLINE modifyCacheSubnetGroup #-}

data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup
    { _mcsgmCacheSubnetGroupName :: Text
      -- ^ The name for the cache subnet group. This value is stored as a
      -- lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters or hyphens. Example: mysubnetgroup.
    , _mcsgmCacheSubnetGroupDescription :: Maybe Text
      -- ^ A description for the cache subnet group.
    , _mcsgmSubnetIds :: [Text]
      -- ^ The EC2 subnet IDs for the cache subnet group.
    } deriving (Show, Generic)

-- | The name for the cache subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric characters
-- or hyphens. Example: mysubnetgroup.
mcsgmCacheSubnetGroupName :: Lens' ModifyCacheSubnetGroup (Text)
mcsgmCacheSubnetGroupName f x =
    f (_mcsgmCacheSubnetGroupName x)
        <&> \y -> x { _mcsgmCacheSubnetGroupName = y }
{-# INLINE mcsgmCacheSubnetGroupName #-}

-- | A description for the cache subnet group.
mcsgmCacheSubnetGroupDescription :: Lens' ModifyCacheSubnetGroup (Maybe Text)
mcsgmCacheSubnetGroupDescription f x =
    f (_mcsgmCacheSubnetGroupDescription x)
        <&> \y -> x { _mcsgmCacheSubnetGroupDescription = y }
{-# INLINE mcsgmCacheSubnetGroupDescription #-}

-- | The EC2 subnet IDs for the cache subnet group.
mcsgmSubnetIds :: Lens' ModifyCacheSubnetGroup ([Text])
mcsgmSubnetIds f x =
    f (_mcsgmSubnetIds x)
        <&> \y -> x { _mcsgmSubnetIds = y }
{-# INLINE mcsgmSubnetIds #-}

instance ToQuery ModifyCacheSubnetGroup where
    toQuery = genericQuery def

data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse
    { _csgzCacheSubnetGroup :: Maybe CacheSubnetGroup
      -- ^ Represents the output of one of the following operations:
      -- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
    } deriving (Show, Generic)

-- | Represents the output of one of the following operations:
-- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
csgzCacheSubnetGroup :: Lens' ModifyCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
csgzCacheSubnetGroup f x =
    f (_csgzCacheSubnetGroup x)
        <&> \y -> x { _csgzCacheSubnetGroup = y }
{-# INLINE csgzCacheSubnetGroup #-}

instance FromXML ModifyCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheSubnetGroup where
    type Sv ModifyCacheSubnetGroup = ElastiCache
    type Rs ModifyCacheSubnetGroup = ModifyCacheSubnetGroupResponse

    request = post "ModifyCacheSubnetGroup"
    response _ = xmlResponse
