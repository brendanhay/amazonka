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

-- Module      : Network.AWS.Route53.ListTagsForResources
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

-- | <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListTagsForResources.html>
module Network.AWS.Route53.ListTagsForResources
    (
    -- * Request
      ListTagsForResources
    -- ** Request constructor
    , listTagsForResources
    -- ** Request lenses
    , ltfr1ResourceIds
    , ltfr1ResourceType

    -- * Response
    , ListTagsForResourcesResponse
    -- ** Response constructor
    , listTagsForResourcesResponse
    -- ** Response lenses
    , ltfrrResourceTagSets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data ListTagsForResources = ListTagsForResources
    { _ltfr1ResourceIds  :: List1 "ResourceId" Text
    , _ltfr1ResourceType :: TagResourceType
    } deriving (Eq, Show)

-- | 'ListTagsForResources' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfr1ResourceIds' @::@ 'NonEmpty' 'Text'
--
-- * 'ltfr1ResourceType' @::@ 'TagResourceType'
--
listTagsForResources :: TagResourceType -- ^ 'ltfr1ResourceType'
                     -> NonEmpty Text -- ^ 'ltfr1ResourceIds'
                     -> ListTagsForResources
listTagsForResources p1 p2 = ListTagsForResources
    { _ltfr1ResourceType = p1
    , _ltfr1ResourceIds  = withIso _List1 (const id) p2
    }

-- | A complex type that contains the ResourceId element for each resource for
-- which you want to get a list of tags.
ltfr1ResourceIds :: Lens' ListTagsForResources (NonEmpty Text)
ltfr1ResourceIds = lens _ltfr1ResourceIds (\s a -> s { _ltfr1ResourceIds = a }) . _List1

-- | The type of the resources. The resource type for health checks is 'healthcheck'.
ltfr1ResourceType :: Lens' ListTagsForResources TagResourceType
ltfr1ResourceType =
    lens _ltfr1ResourceType (\s a -> s { _ltfr1ResourceType = a })

newtype ListTagsForResourcesResponse = ListTagsForResourcesResponse
    { _ltfrrResourceTagSets :: List "ResourceTagSet" ResourceTagSet
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ListTagsForResourcesResponse where
    type Item ListTagsForResourcesResponse = ResourceTagSet

    fromList = ListTagsForResourcesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ltfrrResourceTagSets

-- | 'ListTagsForResourcesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrrResourceTagSets' @::@ ['ResourceTagSet']
--
listTagsForResourcesResponse :: ListTagsForResourcesResponse
listTagsForResourcesResponse = ListTagsForResourcesResponse
    { _ltfrrResourceTagSets = mempty
    }

-- | A list of 'ResourceTagSet's containing tags associated with the specified
-- resources.
ltfrrResourceTagSets :: Lens' ListTagsForResourcesResponse [ResourceTagSet]
ltfrrResourceTagSets =
    lens _ltfrrResourceTagSets (\s a -> s { _ltfrrResourceTagSets = a })
        . _List

instance ToPath ListTagsForResources where
    toPath ListTagsForResources{..} = mconcat
        [ "/2013-04-01/tags/"
        , toText _ltfr1ResourceType
        ]

instance ToQuery ListTagsForResources where
    toQuery = const mempty

instance ToHeaders ListTagsForResources

instance ToXMLRoot ListTagsForResources where
    toXMLRoot ListTagsForResources{..} = namespaced ns "ListTagsForResources"
        [ "ResourceIds" =@ _ltfr1ResourceIds
        ]

instance ToXML ListTagsForResources

instance AWSRequest ListTagsForResources where
    type Sv ListTagsForResources = Route53
    type Rs ListTagsForResources = ListTagsForResourcesResponse

    request  = post
    response = xmlResponse

instance FromXML ListTagsForResourcesResponse where
    parseXML x = ListTagsForResourcesResponse
        <$> x .@? "ResourceTagSets" .!@ mempty
