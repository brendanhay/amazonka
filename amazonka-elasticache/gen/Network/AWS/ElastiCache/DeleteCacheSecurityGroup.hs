{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheSecurityGroup
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
module Network.AWS.ElastiCache
    (
    -- * Request
      DeleteCacheSecurityGroup
    -- ** Request constructor
    , mkDeleteCacheSecurityGroup
    -- ** Request lenses
    , dcsgCacheSecurityGroupName

    -- * Response
    , DeleteCacheSecurityGroupResponse
    -- ** Response constructor
    , mkDeleteCacheSecurityGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DeleteCacheSecurityGroup operation.
newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup
    { _dcsgCacheSecurityGroupName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteCacheSecurityGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSecurityGroupName ::@ @Text@
--
mkDeleteCacheSecurityGroup :: Text -- ^ 'dcsgCacheSecurityGroupName'
                           -> DeleteCacheSecurityGroup
mkDeleteCacheSecurityGroup p1 = DeleteCacheSecurityGroup
    { _dcsgCacheSecurityGroupName = p1
    }

-- | The name of the cache security group to delete. You cannot delete the
-- default security group.
dcsgCacheSecurityGroupName :: Lens' DeleteCacheSecurityGroup Text
dcsgCacheSecurityGroupName =
    lens _dcsgCacheSecurityGroupName
         (\s a -> s { _dcsgCacheSecurityGroupName = a })

instance ToQuery DeleteCacheSecurityGroup where
    toQuery = genericQuery def

data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteCacheSecurityGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse
mkDeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse

instance AWSRequest DeleteCacheSecurityGroup where
    type Sv DeleteCacheSecurityGroup = ElastiCache
    type Rs DeleteCacheSecurityGroup = DeleteCacheSecurityGroupResponse

    request = post "DeleteCacheSecurityGroup"
    response _ = nullaryResponse DeleteCacheSecurityGroupResponse
