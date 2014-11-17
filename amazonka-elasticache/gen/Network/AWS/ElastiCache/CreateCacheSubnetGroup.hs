{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheSubnetGroup operation creates a new cache subnet group. Use
-- this parameter only when you are creating a cluster in an Amazon Virtual
-- Private Cloud (VPC).
--
-- <CreateCacheSubnetGroup.html>
module Network.AWS.ElastiCache.CreateCacheSubnetGroup
    (
    -- * Request
      CreateCacheSubnetGroup
    -- ** Request constructor
    , createCacheSubnetGroup
    -- ** Request lenses
    , ccsgCacheSubnetGroupDescription
    , ccsgCacheSubnetGroupName
    , ccsgSubnetIds

    -- * Response
    , CreateCacheSubnetGroupResponse
    -- ** Response constructor
    , createCacheSubnetGroupResponse
    -- ** Response lenses
    , ccsgrCacheSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data CreateCacheSubnetGroup = CreateCacheSubnetGroup
    { _ccsgCacheSubnetGroupDescription :: Text
    , _ccsgCacheSubnetGroupName        :: Text
    , _ccsgSubnetIds                   :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCacheSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgCacheSubnetGroupDescription' @::@ 'Text'
--
-- * 'ccsgCacheSubnetGroupName' @::@ 'Text'
--
-- * 'ccsgSubnetIds' @::@ ['Text']
--
createCacheSubnetGroup :: Text -- ^ 'ccsgCacheSubnetGroupName'
                       -> Text -- ^ 'ccsgCacheSubnetGroupDescription'
                       -> CreateCacheSubnetGroup
createCacheSubnetGroup p1 p2 = CreateCacheSubnetGroup
    { _ccsgCacheSubnetGroupName        = p1
    , _ccsgCacheSubnetGroupDescription = p2
    , _ccsgSubnetIds                   = mempty
    }

-- | A description for the cache subnet group.
ccsgCacheSubnetGroupDescription :: Lens' CreateCacheSubnetGroup Text
ccsgCacheSubnetGroupDescription =
    lens _ccsgCacheSubnetGroupDescription
        (\s a -> s { _ccsgCacheSubnetGroupDescription = a })

-- | A name for the cache subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Example: mysubnetgroup.
ccsgCacheSubnetGroupName :: Lens' CreateCacheSubnetGroup Text
ccsgCacheSubnetGroupName =
    lens _ccsgCacheSubnetGroupName
        (\s a -> s { _ccsgCacheSubnetGroupName = a })

-- | A list of VPC subnet IDs for the cache subnet group.
ccsgSubnetIds :: Lens' CreateCacheSubnetGroup [Text]
ccsgSubnetIds = lens _ccsgSubnetIds (\s a -> s { _ccsgSubnetIds = a })

newtype CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse
    { _ccsgrCacheSubnetGroup :: Maybe CacheSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateCacheSubnetGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrCacheSubnetGroup' @::@ 'Maybe' 'CacheSubnetGroup'
--
createCacheSubnetGroupResponse :: CreateCacheSubnetGroupResponse
createCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse
    { _ccsgrCacheSubnetGroup = Nothing
    }

ccsgrCacheSubnetGroup :: Lens' CreateCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
ccsgrCacheSubnetGroup =
    lens _ccsgrCacheSubnetGroup (\s a -> s { _ccsgrCacheSubnetGroup = a })

instance AWSRequest CreateCacheSubnetGroup where
    type Sv CreateCacheSubnetGroup = ElastiCache
    type Rs CreateCacheSubnetGroup = CreateCacheSubnetGroupResponse

    request  = post "CreateCacheSubnetGroup"
    response = xmlResponse

instance FromXML CreateCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateCacheSubnetGroupResponse"

instance ToPath CreateCacheSubnetGroup where
    toPath = const "/"

instance ToHeaders CreateCacheSubnetGroup

instance ToQuery CreateCacheSubnetGroup
