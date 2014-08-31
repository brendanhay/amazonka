{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DeleteCacheParameterGroup
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
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DeleteCacheParameterGroup &CacheParameterGroupName=myparametergroup
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential=
-- d0a417cb-575b-11e0-8869-cd22b4f9d96f.
module Network.AWS.ElastiCache.V2014_07_15.DeleteCacheParameterGroup where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

data DeleteCacheParameterGroup = DeleteCacheParameterGroup
    { _dcpgnCacheParameterGroupName :: Text
      -- ^ The name of the cache parameter group to delete. The specified
      -- cache security group must not be associated with any cache
      -- clusters.
    } deriving (Show, Generic)

makeLenses ''DeleteCacheParameterGroup

instance ToQuery DeleteCacheParameterGroup where
    toQuery = genericQuery def

data DeleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteCacheParameterGroupResponse

instance AWSRequest DeleteCacheParameterGroup where
    type Sv DeleteCacheParameterGroup = ElastiCache
    type Rs DeleteCacheParameterGroup = DeleteCacheParameterGroupResponse

    request = post "DeleteCacheParameterGroup"
    response _ = nullaryResponse DeleteCacheParameterGroupResponse
