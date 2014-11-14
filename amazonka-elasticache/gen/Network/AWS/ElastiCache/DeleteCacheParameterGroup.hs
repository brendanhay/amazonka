{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheParameterGroup operation deletes the specified cache
-- parameter group. You cannot delete a cache parameter group if it is
-- associated with any cache clusters.
module Network.AWS.ElastiCache.DeleteCacheParameterGroup
    (
    -- * Request
      DeleteCacheParameterGroup
    -- ** Request constructor
    , deleteCacheParameterGroup
    -- ** Request lenses
    , dcpg1CacheParameterGroupName

    -- * Response
    , DeleteCacheParameterGroupResponse
    -- ** Response constructor
    , deleteCacheParameterGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

newtype DeleteCacheParameterGroup = DeleteCacheParameterGroup
    { _dcpg1CacheParameterGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteCacheParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpg1CacheParameterGroupName' @::@ 'Text'
--
deleteCacheParameterGroup :: Text -- ^ 'dcpg1CacheParameterGroupName'
                          -> DeleteCacheParameterGroup
deleteCacheParameterGroup p1 = DeleteCacheParameterGroup
    { _dcpg1CacheParameterGroupName = p1
    }

-- | The name of the cache parameter group to delete.
dcpg1CacheParameterGroupName :: Lens' DeleteCacheParameterGroup Text
dcpg1CacheParameterGroupName =
    lens _dcpg1CacheParameterGroupName
        (\s a -> s { _dcpg1CacheParameterGroupName = a })

instance ToQuery DeleteCacheParameterGroup

instance ToPath DeleteCacheParameterGroup where
    toPath = const "/"

data DeleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCacheParameterGroupResponse' constructor.
deleteCacheParameterGroupResponse :: DeleteCacheParameterGroupResponse
deleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse

instance AWSRequest DeleteCacheParameterGroup where
    type Sv DeleteCacheParameterGroup = ElastiCache
    type Rs DeleteCacheParameterGroup = DeleteCacheParameterGroupResponse

    request  = post "DeleteCacheParameterGroup"
    response = nullaryResponse DeleteCacheParameterGroupResponse
