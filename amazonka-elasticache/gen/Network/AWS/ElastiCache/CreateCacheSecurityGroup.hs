{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheSecurityGroup operation creates a new cache security group.
-- Use a cache security group to control access to one or more cache clusters.
-- Cache security groups are only used when you are creating a cache cluster
-- outside of an Amazon Virtual Private Cloud (VPC). If you are creating a
-- cache cluster inside of a VPC, use a cache subnet group instead. For more
-- information, see CreateCacheSubnetGroup.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSecurityGroup.html>
module Network.AWS.ElastiCache.CreateCacheSecurityGroup
    (
    -- * Request
      CreateCacheSecurityGroup
    -- ** Request constructor
    , createCacheSecurityGroup
    -- ** Request lenses
    , ccsgCacheSecurityGroupName
    , ccsgDescription

    -- * Response
    , CreateCacheSecurityGroupResponse
    -- ** Response constructor
    , createCacheSecurityGroupResponse
    -- ** Response lenses
    , ccsgrCacheSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data CreateCacheSecurityGroup = CreateCacheSecurityGroup
    { _ccsgCacheSecurityGroupName :: Text
    , _ccsgDescription            :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCacheSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgCacheSecurityGroupName' @::@ 'Text'
--
-- * 'ccsgDescription' @::@ 'Text'
--
createCacheSecurityGroup :: Text -- ^ 'ccsgCacheSecurityGroupName'
                         -> Text -- ^ 'ccsgDescription'
                         -> CreateCacheSecurityGroup
createCacheSecurityGroup p1 p2 = CreateCacheSecurityGroup
    { _ccsgCacheSecurityGroupName = p1
    , _ccsgDescription            = p2
    }

-- | A name for the cache security group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric
-- characters. Cannot be the word "Default". Example: mysecuritygroup.
ccsgCacheSecurityGroupName :: Lens' CreateCacheSecurityGroup Text
ccsgCacheSecurityGroupName =
    lens _ccsgCacheSecurityGroupName
        (\s a -> s { _ccsgCacheSecurityGroupName = a })

-- | A description for the cache security group.
ccsgDescription :: Lens' CreateCacheSecurityGroup Text
ccsgDescription = lens _ccsgDescription (\s a -> s { _ccsgDescription = a })

newtype CreateCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse
    { _ccsgrCacheSecurityGroup :: Maybe CacheSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateCacheSecurityGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrCacheSecurityGroup' @::@ 'Maybe' 'CacheSecurityGroup'
--
createCacheSecurityGroupResponse :: CreateCacheSecurityGroupResponse
createCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse
    { _ccsgrCacheSecurityGroup = Nothing
    }

ccsgrCacheSecurityGroup :: Lens' CreateCacheSecurityGroupResponse (Maybe CacheSecurityGroup)
ccsgrCacheSecurityGroup =
    lens _ccsgrCacheSecurityGroup (\s a -> s { _ccsgrCacheSecurityGroup = a })

instance AWSRequest CreateCacheSecurityGroup where
    type Sv CreateCacheSecurityGroup = ElastiCache
    type Rs CreateCacheSecurityGroup = CreateCacheSecurityGroupResponse

    request  = post "CreateCacheSecurityGroup"
    response = xmlResponse

instance FromXML CreateCacheSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateCacheSecurityGroupResponse"

instance ToPath CreateCacheSecurityGroup where
    toPath = const "/"

instance ToHeaders CreateCacheSecurityGroup

instance ToQuery CreateCacheSecurityGroup
