{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSubnetGroup
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
-- &CacheSubnetGroupName=mysubnetgroup &Version=2014-03-24 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2014-03-15T17%3A48%3A21.746Z
-- &AWSAccessKeyId= &Signature= 5d013245-4172-11df-8520-e7e1e602a915.
module Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSubnetGroup where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data DeleteCacheSubnetGroup = DeleteCacheSubnetGroup
    { _dcsgpCacheSubnetGroupName :: Text
      -- ^ The name of the cache subnet group to delete. Constraints: Must
      -- contain no more than 255 alphanumeric characters or hyphens.
    } deriving (Generic)

makeLenses ''DeleteCacheSubnetGroup

instance ToQuery DeleteCacheSubnetGroup where
    toQuery = genericToQuery def

data DeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteCacheSubnetGroupResponse

instance AWSRequest DeleteCacheSubnetGroup where
    type Sv DeleteCacheSubnetGroup = ElastiCache
    type Rs DeleteCacheSubnetGroup = DeleteCacheSubnetGroupResponse

    request = post "DeleteCacheSubnetGroup"
    response _ _ = return (Right DeleteCacheSubnetGroupResponse)
