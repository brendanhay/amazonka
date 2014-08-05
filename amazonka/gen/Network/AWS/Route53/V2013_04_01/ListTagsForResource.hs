{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ListTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.ListTagsForResource where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

data ListTagsForResource = ListTagsForResource
    { _ltfrrResourceId :: Text
      -- ^ The ID of the resource for which you want to retrieve tags.
    , _ltfrrResourceType :: TagResourceType
      -- ^ The type of the resource. The resource type for health checks is
      -- healthcheck.
    } deriving (Show, Generic)

makeLenses ''ListTagsForResource

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

makeLenses ''ListTagsForResourceResponse

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = Route53
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure ListTagsForResourceResponse
            <*> xml %| "ResourceTagSet"
