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
module Network.AWS.ElastiCache.CreateCacheSubnetGroup
    (
    -- * Request
      CreateCacheSubnetGroupMessage
    -- ** Request constructor
    , createCacheSubnetGroup
    -- ** Request lenses
    , ccsgmCacheSubnetGroupDescription
    , ccsgmCacheSubnetGroupName
    , ccsgmSubnetIds

    -- * Response
    , CreateCacheSubnetGroupResult
    -- ** Response constructor
    , createCacheSubnetGroupResponse
    -- ** Response lenses
    , ccsgrCacheSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data CreateCacheSubnetGroupMessage = CreateCacheSubnetGroupMessage
    { _ccsgmCacheSubnetGroupDescription :: Text
    , _ccsgmCacheSubnetGroupName        :: Text
    , _ccsgmSubnetIds                   :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCacheSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgmCacheSubnetGroupDescription' @::@ 'Text'
--
-- * 'ccsgmCacheSubnetGroupName' @::@ 'Text'
--
-- * 'ccsgmSubnetIds' @::@ ['Text']
--
createCacheSubnetGroup :: Text -- ^ 'ccsgmCacheSubnetGroupName'
                       -> Text -- ^ 'ccsgmCacheSubnetGroupDescription'
                       -> CreateCacheSubnetGroupMessage
createCacheSubnetGroup p1 p2 = CreateCacheSubnetGroupMessage
    { _ccsgmCacheSubnetGroupName        = p1
    , _ccsgmCacheSubnetGroupDescription = p2
    , _ccsgmSubnetIds                   = mempty
    }

-- | A description for the cache subnet group.
ccsgmCacheSubnetGroupDescription :: Lens' CreateCacheSubnetGroupMessage Text
ccsgmCacheSubnetGroupDescription =
    lens _ccsgmCacheSubnetGroupDescription
        (\s a -> s { _ccsgmCacheSubnetGroupDescription = a })

-- | A name for the cache subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Example: mysubnetgroup.
ccsgmCacheSubnetGroupName :: Lens' CreateCacheSubnetGroupMessage Text
ccsgmCacheSubnetGroupName =
    lens _ccsgmCacheSubnetGroupName
        (\s a -> s { _ccsgmCacheSubnetGroupName = a })

-- | A list of VPC subnet IDs for the cache subnet group.
ccsgmSubnetIds :: Lens' CreateCacheSubnetGroupMessage [Text]
ccsgmSubnetIds = lens _ccsgmSubnetIds (\s a -> s { _ccsgmSubnetIds = a })

instance ToQuery CreateCacheSubnetGroupMessage

instance ToPath CreateCacheSubnetGroupMessage where
    toPath = const "/"

newtype CreateCacheSubnetGroupResult = CreateCacheSubnetGroupResult
    { _ccsgrCacheSubnetGroup :: Maybe CacheSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateCacheSubnetGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrCacheSubnetGroup' @::@ 'Maybe' 'CacheSubnetGroup'
--
createCacheSubnetGroupResponse :: CreateCacheSubnetGroupResult
createCacheSubnetGroupResponse = CreateCacheSubnetGroupResult
    { _ccsgrCacheSubnetGroup = Nothing
    }

ccsgrCacheSubnetGroup :: Lens' CreateCacheSubnetGroupResult (Maybe CacheSubnetGroup)
ccsgrCacheSubnetGroup =
    lens _ccsgrCacheSubnetGroup (\s a -> s { _ccsgrCacheSubnetGroup = a })

instance FromXML CreateCacheSubnetGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateCacheSubnetGroupResult"

instance AWSRequest CreateCacheSubnetGroupMessage where
    type Sv CreateCacheSubnetGroupMessage = ElastiCache
    type Rs CreateCacheSubnetGroupMessage = CreateCacheSubnetGroupResult

    request  = post "CreateCacheSubnetGroup"
    response = xmlResponse $ \h x -> CreateCacheSubnetGroupResult
        <$> x %| "CacheSubnetGroup"
