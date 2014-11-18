{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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

-- | 
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListTagsForResource.html>
module Network.AWS.Route53.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , listTagsForResource
    -- ** Request lenses
    , ltfrResourceId
    , ltfrResourceType

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response constructor
    , listTagsForResourceResponse
    -- ** Response lenses
    , ltfrrResourceTagSet
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data ListTagsForResource = ListTagsForResource
    { _ltfrResourceId   :: Text
    , _ltfrResourceType :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListTagsForResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrResourceId' @::@ 'Text'
--
-- * 'ltfrResourceType' @::@ 'Text'
--
listTagsForResource :: Text -- ^ 'ltfrResourceType'
                    -> Text -- ^ 'ltfrResourceId'
                    -> ListTagsForResource
listTagsForResource p1 p2 = ListTagsForResource
    { _ltfrResourceType = p1
    , _ltfrResourceId   = p2
    }

-- | The ID of the resource for which you want to retrieve tags.
ltfrResourceId :: Lens' ListTagsForResource Text
ltfrResourceId = lens _ltfrResourceId (\s a -> s { _ltfrResourceId = a })

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ltfrResourceType :: Lens' ListTagsForResource Text
ltfrResourceType = lens _ltfrResourceType (\s a -> s { _ltfrResourceType = a })

newtype ListTagsForResourceResponse = ListTagsForResourceResponse
    { _ltfrrResourceTagSet :: ResourceTagSet
    } deriving (Eq, Show, Generic)

-- | 'ListTagsForResourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrrResourceTagSet' @::@ 'ResourceTagSet'
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

instance ToPath ListTagsForResource where
    toPath ListTagsForResource{..} = mconcat
        [ "/2013-04-01/tags/"
        , toText _ltfrResourceType
        , "/"
        , toText _ltfrResourceId
        ]

instance ToQuery ListTagsForResource where
    toQuery = const mempty

instance ToHeaders ListTagsForResource

instance ToXMLRoot ListTagsForResource where
    toXMLRoot = const (element "ListTagsForResource" [])

instance ToXML ListTagsForResource

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = Route53
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request  = get
    response = xmlResponse

instance FromXML ListTagsForResourceResponse where
    parseXML x = ListTagsForResourceResponse
        <$> x .@ "ResourceTagSet"
