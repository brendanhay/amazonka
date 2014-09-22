{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

module Network.AWS.Route53.ListTagsForResources
    (
    -- * Request
      ListTagsForResources
    -- ** Request constructor
    , listTagsForResources
    -- ** Request lenses
    , ltfr1ResourceType
    , ltfr1ResourceId

    -- * Response
    , ListTagsForResourcesResponse
    -- ** Response constructor
    , listTagsForResourcesResponse
    -- ** Response lenses
    , ltfrrrResourceTagSet
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type containing information about a request for a list of the
-- tags that are associated with up to 10 specified resources.
data ListTagsForResources = ListTagsForResources
    { _ltfr1ResourceType :: TagResourceType
    , _ltfr1ResourceId :: List1 Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForResources' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceType ::@ @TagResourceType@
--
-- * @ResourceId ::@ @List1 Text@
--
listTagsForResources :: TagResourceType -- ^ 'ltfr1ResourceType'
                     -> List1 Text -- ^ 'ltfr1ResourceId'
                     -> ListTagsForResources
listTagsForResources p1 p2 = ListTagsForResources
    { _ltfr1ResourceType = p1
    , _ltfr1ResourceId = p2
    }

-- | The type of the resources. The resource type for health checks is
-- healthcheck.
ltfr1ResourceType :: Lens' ListTagsForResources TagResourceType
ltfr1ResourceType =
    lens _ltfr1ResourceType (\s a -> s { _ltfr1ResourceType = a })

-- | A complex type that contains the ResourceId element for each resource for
-- which you want to get a list of tags.
ltfr1ResourceId :: Lens' ListTagsForResources (List1 Text)
ltfr1ResourceId = lens _ltfr1ResourceId (\s a -> s { _ltfr1ResourceId = a })

instance ToPath ListTagsForResources

instance ToQuery ListTagsForResources

instance ToHeaders ListTagsForResources

instance ToXML ListTagsForResources where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListTagsForResources"

-- | A complex type containing tags for the specified resources.
newtype ListTagsForResourcesResponse = ListTagsForResourcesResponse
    { _ltfrrrResourceTagSet :: [ResourceTagSet]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForResourcesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceTagSet ::@ @[ResourceTagSet]@
--
listTagsForResourcesResponse :: [ResourceTagSet] -- ^ 'ltfrrrResourceTagSet'
                             -> ListTagsForResourcesResponse
listTagsForResourcesResponse p1 = ListTagsForResourcesResponse
    { _ltfrrrResourceTagSet = p1
    }

-- | A list of ResourceTagSets containing tags associated with the specified
-- resources.
ltfrrrResourceTagSet :: Lens' ListTagsForResourcesResponse [ResourceTagSet]
ltfrrrResourceTagSet =
    lens _ltfrrrResourceTagSet (\s a -> s { _ltfrrrResourceTagSet = a })

instance FromXML ListTagsForResourcesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListTagsForResources where
    type Sv ListTagsForResources = Route53
    type Rs ListTagsForResources = ListTagsForResourcesResponse

    request = get
    response _ = xmlResponse
