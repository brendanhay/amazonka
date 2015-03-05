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

-- Module      : Network.AWS.ElastiCache.ListTagsForResource
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

-- | The /ListTagsForResource/ action lists all cost allocation tags currently on
-- the named resource. A /cost allocation tag/ is a key-value pair where the key
-- is case-sensitive and the value is optional. Cost allocation tags can be used
-- to categorize and track your AWS costs.
--
-- You can have a maximum of 10 cost allocation tags on an ElastiCache
-- resource. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/BestPractices.html Using Cost Allocation Tags in AmazonElastiCache>.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ListTagsForResource.html>
module Network.AWS.ElastiCache.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , listTagsForResource
    -- ** Request lenses
    , ltfrResourceName

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response constructor
    , listTagsForResourceResponse
    -- ** Response lenses
    , ltfrrTagList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

newtype ListTagsForResource = ListTagsForResource
    { _ltfrResourceName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'ListTagsForResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrResourceName' @::@ 'Text'
--
listTagsForResource :: Text -- ^ 'ltfrResourceName'
                    -> ListTagsForResource
listTagsForResource p1 = ListTagsForResource
    { _ltfrResourceName = p1
    }

-- | The name of the resource for which you want the list of tags, for example 'arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster'.
ltfrResourceName :: Lens' ListTagsForResource Text
ltfrResourceName = lens _ltfrResourceName (\s a -> s { _ltfrResourceName = a })

newtype ListTagsForResourceResponse = ListTagsForResourceResponse
    { _ltfrrTagList :: List "member" Tag
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ListTagsForResourceResponse where
    type Item ListTagsForResourceResponse = Tag

    fromList = ListTagsForResourceResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ltfrrTagList

-- | 'ListTagsForResourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrrTagList' @::@ ['Tag']
--
listTagsForResourceResponse :: ListTagsForResourceResponse
listTagsForResourceResponse = ListTagsForResourceResponse
    { _ltfrrTagList = mempty
    }

-- | A list of cost allocation tags as key-value pairs.
ltfrrTagList :: Lens' ListTagsForResourceResponse [Tag]
ltfrrTagList = lens _ltfrrTagList (\s a -> s { _ltfrrTagList = a }) . _List

instance ToPath ListTagsForResource where
    toPath = const "/"

instance ToQuery ListTagsForResource where
    toQuery ListTagsForResource{..} = mconcat
        [ "ResourceName" =? _ltfrResourceName
        ]

instance ToHeaders ListTagsForResource

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = ElastiCache
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request  = post "ListTagsForResource"
    response = xmlResponse

instance FromXML ListTagsForResourceResponse where
    parseXML = withElement "ListTagsForResourceResult" $ \x -> ListTagsForResourceResponse
        <$> x .@? "TagList" .!@ mempty
