{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ListTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , listTagsForResource
    -- ** Request lenses
    , ltfrResourceType
    , ltfrResourceId

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response constructor
    , listTagsForResourceResponse
    -- ** Response lenses
    , ltfrrResourceTagSet
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type containing information about a request for a list of the
-- tags that are associated with an individual resource.
data ListTagsForResource = ListTagsForResource
    { _ltfrResourceType :: TagResourceType
    , _ltfrResourceId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForResource' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceType ::@ @TagResourceType@
--
-- * @ResourceId ::@ @Text@
--
listTagsForResource :: TagResourceType -- ^ 'ltfrResourceType'
                    -> Text -- ^ 'ltfrResourceId'
                    -> ListTagsForResource
listTagsForResource p1 p2 = ListTagsForResource
    { _ltfrResourceType = p1
    , _ltfrResourceId = p2
    }

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ltfrResourceType :: Lens' ListTagsForResource TagResourceType
ltfrResourceType =
    lens _ltfrResourceType (\s a -> s { _ltfrResourceType = a })

-- | The ID of the resource for which you want to retrieve tags.
ltfrResourceId :: Lens' ListTagsForResource Text
ltfrResourceId = lens _ltfrResourceId (\s a -> s { _ltfrResourceId = a })

instance ToPath ListTagsForResource

instance ToQuery ListTagsForResource

instance ToHeaders ListTagsForResource

instance ToXML ListTagsForResource where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListTagsForResourceRequest"

-- | A complex type containing tags for the specified resource.
newtype ListTagsForResourceResponse = ListTagsForResourceResponse
    { _ltfrrResourceTagSet :: ResourceTagSet
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForResourceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceTagSet ::@ @ResourceTagSet@
--
listTagsForResourceResponse :: ResourceTagSet -- ^ 'ltfrrResourceTagSet'
                            -> ListTagsForResourceResponse
listTagsForResourceResponse p1 = ListTagsForResourceResponse
    { _ltfrrResourceTagSet = p1
    }

-- | A ResourceTagSet containing tags associated with the specified resource.
ltfrrResourceTagSet :: Lens' ListTagsForResourceResponse ResourceTagSet
ltfrrResourceTagSet =
    lens _ltfrrResourceTagSet (\s a -> s { _ltfrrResourceTagSet = a })

instance FromXML ListTagsForResourceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = Route53
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request = get
    response _ = xmlResponse
