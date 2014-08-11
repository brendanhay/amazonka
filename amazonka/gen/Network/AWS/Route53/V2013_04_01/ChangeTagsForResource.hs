{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ChangeTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.ChangeTagsForResource where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ChangeTagsForResource' request.
changeTagsForResource :: Text -- ^ '_ctfrrResourceId'
                      -> TagResourceType -- ^ '_ctfrrResourceType'
                      -> ChangeTagsForResource
changeTagsForResource p1 p2 = ChangeTagsForResource
    { _ctfrrResourceId = p1
    , _ctfrrResourceType = p2
    , _ctfrrRemoveTagKeys = Nothing
    , _ctfrrAddTags = Nothing
    }

data ChangeTagsForResource = ChangeTagsForResource
    { _ctfrrResourceId :: Text
      -- ^ The ID of the resource for which you want to add, change, or
      -- delete tags.
    , _ctfrrResourceType :: TagResourceType
      -- ^ The type of the resource. The resource type for health checks is
      -- healthcheck.
    , _ctfrrRemoveTagKeys :: Maybe [Text]
      -- ^ A list of Tag keys that you want to remove from the specified
      -- resource.
    , _ctfrrAddTags :: Maybe [Tag]
      -- ^ A complex type that contains a list of Tag elements. Each Tag
      -- element identifies a tag that you want to add or update for the
      -- specified resource.
    } deriving (Show, Generic)

makeLenses ''ChangeTagsForResource

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

data ChangeTagsForResourceResponse = ChangeTagsForResourceResponse
    deriving (Eq, Show, Generic)

makeLenses ''ChangeTagsForResourceResponse

instance AWSRequest ChangeTagsForResource where
    type Sv ChangeTagsForResource = Route53
    type Rs ChangeTagsForResource = ChangeTagsForResourceResponse

    request = post
    response _ = nullaryResponse ChangeTagsForResourceResponse
