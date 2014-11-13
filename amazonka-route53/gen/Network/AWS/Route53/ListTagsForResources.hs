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
import Network.AWS.Request
import Network.AWS.Route53.Types

data ListTagsForResources = ListTagsForResources
    { _ltfr1ResourceIds  :: List1 Text
    , _ltfr1ResourceType :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListTagsForResources' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfr1ResourceIds' @::@ 'NonEmpty' 'Text'
--
-- * 'ltfr1ResourceType' @::@ 'Text'
--
listTagsForResources :: Text -- ^ 'ltfr1ResourceType'
                     -> NonEmpty Text -- ^ 'ltfr1ResourceIds'
                     -> ListTagsForResources
listTagsForResources p1 p2 = ListTagsForResources
    { _ltfr1ResourceType = p1
    , _ltfr1ResourceIds  = withIso _List1 (const id) p2
    }

-- | A complex type that contains the ResourceId element for each resource for
-- which you want to get a list of tags.
ltfr1ResourceIds :: Lens' ListTagsForResources (NonEmpty Text)
ltfr1ResourceIds = lens _ltfr1ResourceIds (\s a -> s { _ltfr1ResourceIds = a })
    . _List1

-- | The type of the resources. The resource type for health checks is
-- healthcheck.
ltfr1ResourceType :: Lens' ListTagsForResources Text
ltfr1ResourceType =
    lens _ltfr1ResourceType (\s a -> s { _ltfr1ResourceType = a })

instance ToPath ListTagsForResources where
    toPath ListTagsForResources{..} = mconcat
        [ "/2013-04-01/tags/"
        , toText _ltfr1ResourceType
        ]

instance ToQuery ListTagsForResources where
    toQuery = const mempty

instance ToHeaders ListTagsForResources

instance ToBody ListTagsForResources where
    toBody = toBody . encodeXML . _ltfr1ResourceIds

newtype ListTagsForResourcesResponse = ListTagsForResourcesResponse
    { _ltfrrResourceTagSets :: [ResourceTagSet]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList ListTagsForResourcesResponse where
    type Item ListTagsForResourcesResponse = ResourceTagSet

    fromList = ListTagsForResourcesResponse . fromList
    toList   = toList . _ltfrrResourceTagSets

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

instance AWSRequest ListTagsForResources where
    type Sv ListTagsForResources = Route53
    type Rs ListTagsForResources = ListTagsForResourcesResponse

    request  = post
    response = xmlResponse $ \h x -> ListTagsForResourcesResponse
        <$> x %| "ResourceTagSets"
