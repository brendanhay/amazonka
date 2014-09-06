{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ChangeTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.ChangeTagsForResource
    (
    -- * Request
      ChangeTagsForResource
    -- ** Request constructor
    , mkChangeTagsForResource
    -- ** Request lenses
    , ctfrResourceType
    , ctfrResourceId
    , ctfrAddTags
    , ctfrRemoveTagKeys

    -- * Response
    , ChangeTagsForResourceResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type containing information about a request to add, change, or
-- delete the tags that are associated with a resource.
data ChangeTagsForResource = ChangeTagsForResource
    { _ctfrResourceType :: TagResourceType
    , _ctfrResourceId :: Text
    , _ctfrAddTags :: Maybe [Tag]
    , _ctfrRemoveTagKeys :: Maybe [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangeTagsForResource' request.
mkChangeTagsForResource :: TagResourceType -- ^ 'ctfrResourceType'
                        -> Text -- ^ 'ctfrResourceId'
                        -> ChangeTagsForResource
mkChangeTagsForResource p1 p2 = ChangeTagsForResource
    { _ctfrResourceType = p1
    , _ctfrResourceId = p2
    , _ctfrAddTags = Nothing
    , _ctfrRemoveTagKeys = Nothing
    }
{-# INLINE mkChangeTagsForResource #-}

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ctfrResourceType :: Lens' ChangeTagsForResource TagResourceType
ctfrResourceType =
    lens _ctfrResourceType (\s a -> s { _ctfrResourceType = a })
{-# INLINE ctfrResourceType #-}

-- | The ID of the resource for which you want to add, change, or delete tags.
ctfrResourceId :: Lens' ChangeTagsForResource Text
ctfrResourceId = lens _ctfrResourceId (\s a -> s { _ctfrResourceId = a })
{-# INLINE ctfrResourceId #-}

-- | A complex type that contains a list of Tag elements. Each Tag element
-- identifies a tag that you want to add or update for the specified resource.
ctfrAddTags :: Lens' ChangeTagsForResource (Maybe [Tag])
ctfrAddTags = lens _ctfrAddTags (\s a -> s { _ctfrAddTags = a })
{-# INLINE ctfrAddTags #-}

-- | A list of Tag keys that you want to remove from the specified resource.
ctfrRemoveTagKeys :: Lens' ChangeTagsForResource (Maybe [Text])
ctfrRemoveTagKeys =
    lens _ctfrRemoveTagKeys (\s a -> s { _ctfrRemoveTagKeys = a })
{-# INLINE ctfrRemoveTagKeys #-}

instance ToPath ChangeTagsForResource where
    toPath ChangeTagsForResource{..} = mconcat
        [ "/2013-04-01/tags/"
        , toBS _ctfrResourceType
        , "/"
        , toBS _ctfrResourceId
        ]

instance ToQuery ChangeTagsForResource

instance ToHeaders ChangeTagsForResource

instance ToXML ChangeTagsForResource where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeTagsForResourceRequest"

-- | Empty response for the request.
    deriving (Eq, Show, Generic)

instance AWSRequest ChangeTagsForResource where
    type Sv ChangeTagsForResource = Route53
    type Rs ChangeTagsForResource = ChangeTagsForResourceResponse

    request = post
    response _ = nullaryResponse ChangeTagsForResourceResponse
