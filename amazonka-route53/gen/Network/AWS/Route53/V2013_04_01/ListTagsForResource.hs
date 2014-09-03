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
    , listTagsForResource
    -- ** Request lenses
    , ltfrrResourceId
    , ltfrrResourceType

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response lenses
    , ltfrsResourceTagSet
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListTagsForResource' request.
listTagsForResource :: Text -- ^ 'ltfrrResourceId'
                    -> TagResourceType -- ^ 'ltfrrResourceType'
                    -> ListTagsForResource
listTagsForResource p1 p2 = ListTagsForResource
    { _ltfrrResourceId = p1
    , _ltfrrResourceType = p2
    }

data ListTagsForResource = ListTagsForResource
    { _ltfrrResourceId :: Text
      -- ^ The ID of the resource for which you want to retrieve tags.
    , _ltfrrResourceType :: TagResourceType
      -- ^ The type of the resource. The resource type for health checks is
      -- healthcheck.
    } deriving (Show, Generic)

-- | The ID of the resource for which you want to retrieve tags.
ltfrrResourceId
    :: Functor f
    => (Text
    -> f (Text))
    -> ListTagsForResource
    -> f ListTagsForResource
ltfrrResourceId f x =
    (\y -> x { _ltfrrResourceId = y })
       <$> f (_ltfrrResourceId x)
{-# INLINE ltfrrResourceId #-}

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ltfrrResourceType
    :: Functor f
    => (TagResourceType
    -> f (TagResourceType))
    -> ListTagsForResource
    -> f ListTagsForResource
ltfrrResourceType f x =
    (\y -> x { _ltfrrResourceType = y })
       <$> f (_ltfrrResourceType x)
{-# INLINE ltfrrResourceType #-}

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

data ListTagsForResourceResponse = ListTagsForResourceResponse
    { _ltfrsResourceTagSet :: ResourceTagSet
      -- ^ A ResourceTagSet containing tags associated with the specified
      -- resource.
    } deriving (Show, Generic)

-- | A ResourceTagSet containing tags associated with the specified resource.
ltfrsResourceTagSet
    :: Functor f
    => (ResourceTagSet
    -> f (ResourceTagSet))
    -> ListTagsForResourceResponse
    -> f ListTagsForResourceResponse
ltfrsResourceTagSet f x =
    (\y -> x { _ltfrsResourceTagSet = y })
       <$> f (_ltfrsResourceTagSet x)
{-# INLINE ltfrsResourceTagSet #-}

instance FromXML ListTagsForResourceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = Route53
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request = get
    response _ = xmlResponse
