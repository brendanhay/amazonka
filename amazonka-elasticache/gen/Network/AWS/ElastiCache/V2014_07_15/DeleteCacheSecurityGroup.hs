{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DeleteCacheSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheSecurityGroup operation deletes a cache security group. You
-- cannot delete a cache security group if it is associated with any cache
-- clusters. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DeleteCacheSecurityGroup
-- &CacheSecurityGroupName=mycachesecuritygroup3 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= c130cfb7-3650-11e0-ae57-f96cfe56749c.
module Network.AWS.ElastiCache.V2014_07_15.DeleteCacheSecurityGroup
    (
    -- * Request
      DeleteCacheSecurityGroup
    -- ** Request constructor
    , deleteCacheSecurityGroup
    -- ** Request lenses
    , dcsgmCacheSecurityGroupName

    -- * Response
    , DeleteCacheSecurityGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteCacheSecurityGroup' request.
deleteCacheSecurityGroup :: Text -- ^ 'dcsgmCacheSecurityGroupName'
                         -> DeleteCacheSecurityGroup
deleteCacheSecurityGroup p1 = DeleteCacheSecurityGroup
    { _dcsgmCacheSecurityGroupName = p1
    }
{-# INLINE deleteCacheSecurityGroup #-}

data DeleteCacheSecurityGroup = DeleteCacheSecurityGroup
    { _dcsgmCacheSecurityGroupName :: Text
      -- ^ The name of the cache security group to delete. You cannot delete
      -- the default security group.
    } deriving (Show, Generic)

-- | The name of the cache security group to delete. You cannot delete the
-- default security group.
dcsgmCacheSecurityGroupName :: Lens' DeleteCacheSecurityGroup (Text)
dcsgmCacheSecurityGroupName f x =
    f (_dcsgmCacheSecurityGroupName x)
        <&> \y -> x { _dcsgmCacheSecurityGroupName = y }
{-# INLINE dcsgmCacheSecurityGroupName #-}

instance ToQuery DeleteCacheSecurityGroup where
    toQuery = genericQuery def

data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteCacheSecurityGroup where
    type Sv DeleteCacheSecurityGroup = ElastiCache
    type Rs DeleteCacheSecurityGroup = DeleteCacheSecurityGroupResponse

    request = post "DeleteCacheSecurityGroup"
    response _ = nullaryResponse DeleteCacheSecurityGroupResponse
