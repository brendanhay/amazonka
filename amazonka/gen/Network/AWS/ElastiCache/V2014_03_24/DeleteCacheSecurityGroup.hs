{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSecurityGroup
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
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-27T02%3A54%3A12.418Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE c130cfb7-3650-11e0-ae57-f96cfe56749c.
module Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSecurityGroup where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data DeleteCacheSecurityGroup = DeleteCacheSecurityGroup
    { _dcsgmCacheSecurityGroupName :: Text
      -- ^ The name of the cache security group to delete. You cannot delete
      -- the default security group.
    } deriving (Generic)

makeLenses ''DeleteCacheSecurityGroup

instance ToQuery DeleteCacheSecurityGroup where
    toQuery = genericToQuery def

data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteCacheSecurityGroupResponse

instance AWSRequest DeleteCacheSecurityGroup where
    type Sv DeleteCacheSecurityGroup = ElastiCache
    type Rs DeleteCacheSecurityGroup = DeleteCacheSecurityGroupResponse

    request = post "DeleteCacheSecurityGroup"
    response _ _ = return (Right DeleteCacheSecurityGroupResponse)
