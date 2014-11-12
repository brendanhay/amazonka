{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.Route53.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , listTagsForResource
    -- ** Request lenses
    , ltfr1ResourceId
    , ltfr1ResourceType

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response constructor
    , listTagsForResourceResponse
    -- ** Response lenses
    , ltfrrResourceTagSet
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53.Types

data ListTagsForResource = ListTagsForResource
    { _ltfr1ResourceId   :: Text
    , _ltfr1ResourceType :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListTagsForResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfr1ResourceId' @::@ 'Text'
--
-- * 'ltfr1ResourceType' @::@ 'Text'
--
listTagsForResource :: Text -- ^ 'ltfr1ResourceType'
                    -> Text -- ^ 'ltfr1ResourceId'
                    -> ListTagsForResource
listTagsForResource p1 p2 = ListTagsForResource
    { _ltfr1ResourceType = p1
    , _ltfr1ResourceId   = p2
    }

-- | The ID of the resource for which you want to retrieve tags.
ltfr1ResourceId :: Lens' ListTagsForResource Text
ltfr1ResourceId = lens _ltfr1ResourceId (\s a -> s { _ltfr1ResourceId = a })

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ltfr1ResourceType :: Lens' ListTagsForResource Text
ltfr1ResourceType =
    lens _ltfr1ResourceType (\s a -> s { _ltfr1ResourceType = a })

instance ToPath ListTagsForResource where
    toPath ListTagsForResource{..} = mconcat
        [ "/2013-04-01/tags/"
        , toText _ltfr1ResourceType
        , "/"
        , toText _ltfr1ResourceId
        ]

instance ToQuery ListTagsForResource where
    toQuery = const mempty

instance ToHeaders ListTagsForResource

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

instance FromXML ListTagsForResourceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListTagsForResourceResponse"
instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = Route53
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request  = get
    response = xmlResponse $ \h x -> ListTagsForResourceResponse
        <$> x %| "ResourceTagSet"
