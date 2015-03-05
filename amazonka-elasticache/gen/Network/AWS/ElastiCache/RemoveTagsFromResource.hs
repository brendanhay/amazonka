{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.RemoveTagsFromResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /RemoveTagsFromResource/ action removes the tags identified by the 'TagKeys'
-- list from the named resource.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_RemoveTagsFromResource.html>
module Network.AWS.ElastiCache.RemoveTagsFromResource
    (
    -- * Request
      RemoveTagsFromResource
    -- ** Request constructor
    , removeTagsFromResource
    -- ** Request lenses
    , rtfrResourceName
    , rtfrTagKeys

    -- * Response
    , RemoveTagsFromResourceResponse
    -- ** Response constructor
    , removeTagsFromResourceResponse
    -- ** Response lenses
    , rtfrrTagList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data RemoveTagsFromResource = RemoveTagsFromResource
    { _rtfrResourceName :: Text
    , _rtfrTagKeys      :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RemoveTagsFromResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfrResourceName' @::@ 'Text'
--
-- * 'rtfrTagKeys' @::@ ['Text']
--
removeTagsFromResource :: Text -- ^ 'rtfrResourceName'
                       -> RemoveTagsFromResource
removeTagsFromResource p1 = RemoveTagsFromResource
    { _rtfrResourceName = p1
    , _rtfrTagKeys      = mempty
    }

-- | The name of the ElastiCache resource from which you want the listed tags
-- removed, for example 'arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster'.
rtfrResourceName :: Lens' RemoveTagsFromResource Text
rtfrResourceName = lens _rtfrResourceName (\s a -> s { _rtfrResourceName = a })

-- | A list of 'TagKeys' identifying the tags you want removed from the named
-- resource. For example, 'TagKeys.member.1=Region' removes the cost allocation
-- tag with the key name 'Region' from the resource named by the /ResourceName/
-- parameter.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\s a -> s { _rtfrTagKeys = a }) . _List

newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse
    { _rtfrrTagList :: List "member" Tag
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList RemoveTagsFromResourceResponse where
    type Item RemoveTagsFromResourceResponse = Tag

    fromList = RemoveTagsFromResourceResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _rtfrrTagList

-- | 'RemoveTagsFromResourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfrrTagList' @::@ ['Tag']
--
removeTagsFromResourceResponse :: RemoveTagsFromResourceResponse
removeTagsFromResourceResponse = RemoveTagsFromResourceResponse
    { _rtfrrTagList = mempty
    }

-- | A list of cost allocation tags as key-value pairs.
rtfrrTagList :: Lens' RemoveTagsFromResourceResponse [Tag]
rtfrrTagList = lens _rtfrrTagList (\s a -> s { _rtfrrTagList = a }) . _List

instance ToPath RemoveTagsFromResource where
    toPath = const "/"

instance ToQuery RemoveTagsFromResource where
    toQuery RemoveTagsFromResource{..} = mconcat
        [ "ResourceName" =? _rtfrResourceName
        , "TagKeys"      =? _rtfrTagKeys
        ]

instance ToHeaders RemoveTagsFromResource

instance AWSRequest RemoveTagsFromResource where
    type Sv RemoveTagsFromResource = ElastiCache
    type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse

    request  = post "RemoveTagsFromResource"
    response = xmlResponse

instance FromXML RemoveTagsFromResourceResponse where
    parseXML = withElement "RemoveTagsFromResourceResult" $ \x -> RemoveTagsFromResourceResponse
        <$> x .@? "TagList" .!@ mempty
