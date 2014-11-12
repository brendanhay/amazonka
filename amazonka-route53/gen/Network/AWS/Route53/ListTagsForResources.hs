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

-- Module      : Network.AWS.Route53.ListTagsForResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | 
module Network.AWS.Route53.ListTagsForResources
    (
    -- * Request
      ListTagsForResources
    -- ** Request constructor
    , listTagsForResources
    -- ** Request lenses
    , ltfrResourceIds
    , ltfrResourceType

    -- * Response
    , ListTagsForResourcesResponse
    -- ** Response constructor
    , listTagsForResourcesResponse
    -- ** Response lenses
    , ltfrrResourceTagSets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53.Types

data ListTagsForResources = ListTagsForResources
    { _ltfrResourceIds  :: List1 Text
    , _ltfrResourceType :: Text
    } (Eq, Ord, Show, Generic)

-- | 'ListTagsForResources' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrResourceIds' @::@ 'NonEmpty' 'Text'
--
-- * 'ltfrResourceType' @::@ 'Text'
--
listTagsForResources :: Text -- ^ 'ltfrResourceType'
                     -> NonEmpty Text -- ^ 'ltfrResourceIds'
                     -> ListTagsForResources
listTagsForResources p1 p2 = ListTagsForResources
    { _ltfrResourceType = p1
    , _ltfrResourceIds  = withIso _List1 (const id) p2
    }

-- | A complex type that contains the ResourceId element for each resource for
-- which you want to get a list of tags.
ltfrResourceIds :: Lens' ListTagsForResources (NonEmpty Text)
ltfrResourceIds = lens _ltfrResourceIds (\s a -> s { _ltfrResourceIds = a })
    . _List1

-- | The type of the resources. The resource type for health checks is
-- healthcheck.
ltfrResourceType :: Lens' ListTagsForResources Text
ltfrResourceType = lens _ltfrResourceType (\s a -> s { _ltfrResourceType = a })

instance ToPath ListTagsForResources where
    toPath ListTagsForResources{..} = mconcat
        [ "/2013-04-01/tags/"
        , toText _ltfrResourceType
        ]

instance ToQuery ListTagsForResources where
    toQuery = const mempty

instance ToHeaders ListTagsForResources

instance ToBody ListTagsForResources where
    toBody = toBody . encodeXML . _ltfrResourceIds

newtype ListTagsForResourcesResponse = ListTagsForResourcesResponse
    { _ltfrrResourceTagSets :: [ResourceTagSet]
    } (Eq, Show, Generic, Foldable, Traversable, Monoid, Semigroup)

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

-- | A list of ResourceTagSets containing tags associated with the specified
-- resources.
ltfrrResourceTagSets :: Lens' ListTagsForResourcesResponse [ResourceTagSet]
ltfrrResourceTagSets =
    lens _ltfrrResourceTagSets (\s a -> s { _ltfrrResourceTagSets = a })

instance FromXML ListTagsForResourcesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListTagsForResourcesResponse"
instance AWSRequest ListTagsForResources where
    type Sv ListTagsForResources = Route53
    type Rs ListTagsForResources = ListTagsForResourcesResponse

    request  = post
    response = xmlResponse $ \h x -> ListTagsForResourcesResponse
        <$> x %| "ResourceTagSets"
