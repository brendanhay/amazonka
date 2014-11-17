{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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

-- | The DeleteCacheSubnetGroup operation deletes a cache subnet group.
module Network.AWS.ElastiCache.DeleteCacheSubnetGroup
    (
    -- * Request
      DeleteCacheSubnetGroup
    -- ** Request constructor
    , deleteCacheSubnetGroup
    -- ** Request lenses
    , dcsg1CacheSubnetGroupName

    -- * Response
    , DeleteCacheSubnetGroupResponse
    -- ** Response constructor
    , deleteCacheSubnetGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

newtype DeleteCacheSubnetGroup = DeleteCacheSubnetGroup
    { _dcsg1CacheSubnetGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteCacheSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsg1CacheSubnetGroupName' @::@ 'Text'
--
deleteCacheSubnetGroup :: Text -- ^ 'dcsg1CacheSubnetGroupName'
                       -> DeleteCacheSubnetGroup
deleteCacheSubnetGroup p1 = DeleteCacheSubnetGroup
    { _dcsg1CacheSubnetGroupName = p1
    }

-- | The name of the cache subnet group to delete. Constraints: Must contain
-- no more than 255 alphanumeric characters or hyphens.
dcsg1CacheSubnetGroupName :: Lens' DeleteCacheSubnetGroup Text
dcsg1CacheSubnetGroupName =
    lens _dcsg1CacheSubnetGroupName
        (\s a -> s { _dcsg1CacheSubnetGroupName = a })

data DeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCacheSubnetGroupResponse' constructor.
deleteCacheSubnetGroupResponse :: DeleteCacheSubnetGroupResponse
deleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse

instance AWSRequest DeleteCacheSubnetGroup where
    type Sv DeleteCacheSubnetGroup = ElastiCache
    type Rs DeleteCacheSubnetGroup = DeleteCacheSubnetGroupResponse

    request  = post "DeleteCacheSubnetGroup"
    response = nullResponse DeleteCacheSubnetGroupResponse

instance ToPath DeleteCacheSubnetGroup where
    toPath = const "/"

instance ToHeaders DeleteCacheSubnetGroup

instance ToQuery DeleteCacheSubnetGroup
