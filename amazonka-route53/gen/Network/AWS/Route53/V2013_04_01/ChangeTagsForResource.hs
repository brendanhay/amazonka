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
    , mkChangeTagsForResourceRequest
    -- ** Request lenses
    , ctfrrResourceType
    , ctfrrResourceId
    , ctfrrAddTags
    , ctfrrRemoveTagKeys

    -- * Response
    , ChangeTagsForResourceResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangeTagsForResource' request.
mkChangeTagsForResourceRequest :: TagResourceType -- ^ 'ctfrrResourceType'
                               -> Text -- ^ 'ctfrrResourceId'
                               -> ChangeTagsForResource
mkChangeTagsForResourceRequest p1 p2 = ChangeTagsForResource
    { _ctfrrResourceType = p1
    , _ctfrrResourceId = p2
    , _ctfrrAddTags = Nothing
    , _ctfrrRemoveTagKeys = Nothing
    }
{-# INLINE mkChangeTagsForResourceRequest #-}

data ChangeTagsForResource = ChangeTagsForResource
    { _ctfrrResourceType :: TagResourceType
      -- ^ The type of the resource. The resource type for health checks is
      -- healthcheck.
    , _ctfrrResourceId :: Text
      -- ^ The ID of the resource for which you want to add, change, or
      -- delete tags.
    , _ctfrrAddTags :: Maybe [Tag]
      -- ^ A complex type that contains a list of Tag elements. Each Tag
      -- element identifies a tag that you want to add or update for the
      -- specified resource.
    , _ctfrrRemoveTagKeys :: Maybe [Text]
      -- ^ A list of Tag keys that you want to remove from the specified
      -- resource.
    } deriving (Show, Generic)

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ctfrrResourceType :: Lens' ChangeTagsForResource (TagResourceType)
ctfrrResourceType = lens _ctfrrResourceType (\s a -> s { _ctfrrResourceType = a })
{-# INLINE ctfrrResourceType #-}

-- | The ID of the resource for which you want to add, change, or delete tags.
ctfrrResourceId :: Lens' ChangeTagsForResource (Text)
ctfrrResourceId = lens _ctfrrResourceId (\s a -> s { _ctfrrResourceId = a })
{-# INLINE ctfrrResourceId #-}

-- | A complex type that contains a list of Tag elements. Each Tag element
-- identifies a tag that you want to add or update for the specified resource.
ctfrrAddTags :: Lens' ChangeTagsForResource (Maybe [Tag])
ctfrrAddTags = lens _ctfrrAddTags (\s a -> s { _ctfrrAddTags = a })
{-# INLINE ctfrrAddTags #-}

-- | A list of Tag keys that you want to remove from the specified resource.
ctfrrRemoveTagKeys :: Lens' ChangeTagsForResource (Maybe [Text])
ctfrrRemoveTagKeys = lens _ctfrrRemoveTagKeys (\s a -> s { _ctfrrRemoveTagKeys = a })
{-# INLINE ctfrrRemoveTagKeys #-}

instance ToPath ChangeTagsForResource where
    toPath ChangeTagsForResource{..} = mconcat
        [ "/2013-04-01/tags/"
        , toBS _ctfrrResourceType
        , "/"
        , toBS _ctfrrResourceId
        ]

instance ToQuery ChangeTagsForResource

instance ToHeaders ChangeTagsForResource

instance ToXML ChangeTagsForResource where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeTagsForResourceRequest"

    deriving (Eq, Show, Generic)

instance AWSRequest ChangeTagsForResource where
    type Sv ChangeTagsForResource = Route53
    type Rs ChangeTagsForResource = ChangeTagsForResourceResponse

    request = post
    response _ = nullaryResponse ChangeTagsForResourceResponse
