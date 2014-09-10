{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheSubnetGroup operation deletes a cache subnet group. You
-- cannot delete a cache subnet group if it is associated with any cache
-- clusters. https://elasticache.amazonaws.com/ ?Action=DeleteCacheSubnetGroup
-- &CacheSubnetGroupName=mysubnetgroup &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- 5d013245-4172-11df-8520-e7e1e602a915.
module Network.AWS.ElastiCache.DeleteCacheSubnetGroup
    (
    -- * Request
      DeleteCacheSubnetGroup
    -- ** Request constructor
    , mkDeleteCacheSubnetGroup
    -- ** Request lenses
    , dcsg1CacheSubnetGroupName

    -- * Response
    , DeleteCacheSubnetGroupResponse
    -- ** Response constructor
    , mkDeleteCacheSubnetGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DeleteCacheSubnetGroup operation.
newtype DeleteCacheSubnetGroup = DeleteCacheSubnetGroup
    { _dcsg1CacheSubnetGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteCacheSubnetGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSubnetGroupName ::@ @Text@
--
mkDeleteCacheSubnetGroup :: Text -- ^ 'dcsg1CacheSubnetGroupName'
                         -> DeleteCacheSubnetGroup
mkDeleteCacheSubnetGroup p1 = DeleteCacheSubnetGroup
    { _dcsg1CacheSubnetGroupName = p1
    }

-- | The name of the cache subnet group to delete. Constraints: Must contain no
-- more than 255 alphanumeric characters or hyphens.
dcsg1CacheSubnetGroupName :: Lens' DeleteCacheSubnetGroup Text
dcsg1CacheSubnetGroupName =
    lens _dcsg1CacheSubnetGroupName
         (\s a -> s { _dcsg1CacheSubnetGroupName = a })

instance ToQuery DeleteCacheSubnetGroup where
    toQuery = genericQuery def

data DeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteCacheSubnetGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteCacheSubnetGroupResponse :: DeleteCacheSubnetGroupResponse
mkDeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse

instance AWSRequest DeleteCacheSubnetGroup where
    type Sv DeleteCacheSubnetGroup = ElastiCache
    type Rs DeleteCacheSubnetGroup = DeleteCacheSubnetGroupResponse

    request = post "DeleteCacheSubnetGroup"
    response _ = nullaryResponse DeleteCacheSubnetGroupResponse
