{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ListTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , mkListTagsForResourceRequest
    -- ** Request lenses
    , ltfrrResourceType
    , ltfrrResourceId

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response lenses
    , ltfrsResourceTagSet
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForResource' request.
mkListTagsForResourceRequest :: TagResourceType -- ^ 'ltfrrResourceType'
                             -> Text -- ^ 'ltfrrResourceId'
                             -> ListTagsForResource
mkListTagsForResourceRequest p1 p2 = ListTagsForResource
    { _ltfrrResourceType = p1
    , _ltfrrResourceId = p2
    }
{-# INLINE mkListTagsForResourceRequest #-}

data ListTagsForResource = ListTagsForResource
    { _ltfrrResourceType :: TagResourceType
      -- ^ The type of the resource. The resource type for health checks is
      -- healthcheck.
    , _ltfrrResourceId :: Text
      -- ^ The ID of the resource for which you want to retrieve tags.
    } deriving (Show, Generic)

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ltfrrResourceType :: Lens' ListTagsForResource (TagResourceType)
ltfrrResourceType = lens _ltfrrResourceType (\s a -> s { _ltfrrResourceType = a })
{-# INLINE ltfrrResourceType #-}

-- | The ID of the resource for which you want to retrieve tags.
ltfrrResourceId :: Lens' ListTagsForResource (Text)
ltfrrResourceId = lens _ltfrrResourceId (\s a -> s { _ltfrrResourceId = a })
{-# INLINE ltfrrResourceId #-}

instance ToPath ListTagsForResource where
    toPath ListTagsForResource{..} = mconcat
        [ "/2013-04-01/tags/"
        , toBS _ltfrrResourceType
        , "/"
        , toBS _ltfrrResourceId
        ]

instance ToQuery ListTagsForResource

instance ToHeaders ListTagsForResource

instance ToXML ListTagsForResource where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListTagsForResourceRequest"

newtype ListTagsForResourceResponse = ListTagsForResourceResponse
    { _ltfrsResourceTagSet :: ResourceTagSet
      -- ^ A ResourceTagSet containing tags associated with the specified
      -- resource.
    } deriving (Show, Generic)

-- | A ResourceTagSet containing tags associated with the specified resource.
ltfrsResourceTagSet :: Lens' ListTagsForResourceResponse (ResourceTagSet)
ltfrsResourceTagSet = lens _ltfrsResourceTagSet (\s a -> s { _ltfrsResourceTagSet = a })
{-# INLINE ltfrsResourceTagSet #-}

instance FromXML ListTagsForResourceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = Route53
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request = get
    response _ = xmlResponse
