{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      DeleteCacheSubnetGroupMessage
    -- ** Request constructor
    , deleteCacheSubnetGroupMessage
    -- ** Request lenses
    , dcsgm1CacheSubnetGroupName

    -- * Response
    , DeleteCacheSubnetGroupResponse
    -- ** Response constructor
    , deleteCacheSubnetGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

newtype DeleteCacheSubnetGroupMessage = DeleteCacheSubnetGroupMessage
    { _dcsgm1CacheSubnetGroupName :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteCacheSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgm1CacheSubnetGroupName' @::@ 'Text'
--
deleteCacheSubnetGroupMessage :: Text -- ^ 'dcsgm1CacheSubnetGroupName'
                              -> DeleteCacheSubnetGroupMessage
deleteCacheSubnetGroupMessage p1 = DeleteCacheSubnetGroupMessage
    { _dcsgm1CacheSubnetGroupName = p1
    }

-- | The name of the cache subnet group to delete. Constraints: Must contain
-- no more than 255 alphanumeric characters or hyphens.
dcsgm1CacheSubnetGroupName :: Lens' DeleteCacheSubnetGroupMessage Text
dcsgm1CacheSubnetGroupName =
    lens _dcsgm1CacheSubnetGroupName
        (\s a -> s { _dcsgm1CacheSubnetGroupName = a })
instance ToQuery DeleteCacheSubnetGroupMessage

instance ToPath DeleteCacheSubnetGroupMessage where
    toPath = const "/"

data DeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCacheSubnetGroupResponse' constructor.
deleteCacheSubnetGroupResponse :: DeleteCacheSubnetGroupResponse
deleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse

instance FromXML DeleteCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteCacheSubnetGroupResponse"

instance AWSRequest DeleteCacheSubnetGroupMessage where
    type Sv DeleteCacheSubnetGroupMessage = ElastiCache
    type Rs DeleteCacheSubnetGroupMessage = DeleteCacheSubnetGroupResponse

    request  = post "DeleteCacheSubnetGroup"
    response = nullaryResponse DeleteCacheSubnetGroupResponse
