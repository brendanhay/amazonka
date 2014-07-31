{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ListTagsForResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.ListTagsForResources where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

data ListTagsForResources = ListTagsForResources
    { _ltfrtResourceIds :: [Text]
      -- ^ A complex type that contains the ResourceId element for each
      -- resource for which you want to get a list of tags.
    , _ltfrtResourceType :: TagResourceType
      -- ^ The type of the resources. The resource type for health checks is
      -- healthcheck.
    } deriving (Generic)

instance ToPath ListTagsForResources where
    toPath ListTagsForResources{..} = mconcat
        [ "/2013-04-01/tags/"
        , toBS _ltfrtResourceType
        ]

instance ToQuery ListTagsForResources

instance ToHeaders ListTagsForResources

instance ToXML ListTagsForResources where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListTagsForResourcesRequest"

instance AWSRequest ListTagsForResources where
    type Sv ListTagsForResources = Route53
    type Rs ListTagsForResources = ListTagsForResourcesResponse

    request = post
    response _ = xmlResponse

data ListTagsForResourcesResponse = ListTagsForResourcesResponse
    { _ltfruResourceTagSets :: [ResourceTagSet]
      -- ^ A list of ResourceTagSets containing tags associated with the
      -- specified resources.
    } deriving (Generic)

instance FromXML ListTagsForResourcesResponse where
    fromXMLOptions = xmlOptions
