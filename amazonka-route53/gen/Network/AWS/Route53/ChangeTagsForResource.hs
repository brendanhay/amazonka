{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ChangeTagsForResource
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
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeTagsForResource.html>
module Network.AWS.Route53.ChangeTagsForResource
    (
    -- * Request
      ChangeTagsForResource
    -- ** Request constructor
    , changeTagsForResource
    -- ** Request lenses
    , ctfrAddTags
    , ctfrRemoveTagKeys
    , ctfrResourceId
    , ctfrResourceType

    -- * Response
    , ChangeTagsForResourceResponse
    -- ** Response constructor
    , changeTagsForResourceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data ChangeTagsForResource = ChangeTagsForResource
    { _ctfrAddTags       :: List1 "Tag" Tag
    , _ctfrRemoveTagKeys :: List1 "Key" Text
    , _ctfrResourceId    :: Text
    , _ctfrResourceType  :: Text
    } deriving (Eq, Show)

-- | 'ChangeTagsForResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctfrAddTags' @::@ 'NonEmpty' 'Tag'
--
-- * 'ctfrRemoveTagKeys' @::@ 'NonEmpty' 'Text'
--
-- * 'ctfrResourceId' @::@ 'Text'
--
-- * 'ctfrResourceType' @::@ 'Text'
--
changeTagsForResource :: Text -- ^ 'ctfrResourceType'
                      -> Text -- ^ 'ctfrResourceId'
                      -> NonEmpty Tag -- ^ 'ctfrAddTags'
                      -> NonEmpty Text -- ^ 'ctfrRemoveTagKeys'
                      -> ChangeTagsForResource
changeTagsForResource p1 p2 p3 p4 = ChangeTagsForResource
    { _ctfrResourceType  = p1
    , _ctfrResourceId    = p2
    , _ctfrAddTags       = withIso _List1 (const id) p3
    , _ctfrRemoveTagKeys = withIso _List1 (const id) p4
    }

-- | A complex type that contains a list of Tag elements. Each Tag element
-- identifies a tag that you want to add or update for the specified
-- resource.
ctfrAddTags :: Lens' ChangeTagsForResource (NonEmpty Tag)
ctfrAddTags = lens _ctfrAddTags (\s a -> s { _ctfrAddTags = a }) . _List1

-- | A list of Tag keys that you want to remove from the specified resource.
ctfrRemoveTagKeys :: Lens' ChangeTagsForResource (NonEmpty Text)
ctfrRemoveTagKeys =
    lens _ctfrRemoveTagKeys (\s a -> s { _ctfrRemoveTagKeys = a })
        . _List1

-- | The ID of the resource for which you want to add, change, or delete tags.
ctfrResourceId :: Lens' ChangeTagsForResource Text
ctfrResourceId = lens _ctfrResourceId (\s a -> s { _ctfrResourceId = a })

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ctfrResourceType :: Lens' ChangeTagsForResource Text
ctfrResourceType = lens _ctfrResourceType (\s a -> s { _ctfrResourceType = a })

data ChangeTagsForResourceResponse = ChangeTagsForResourceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ChangeTagsForResourceResponse' constructor.
changeTagsForResourceResponse :: ChangeTagsForResourceResponse
changeTagsForResourceResponse = ChangeTagsForResourceResponse

instance ToPath ChangeTagsForResource where
    toPath ChangeTagsForResource{..} = mconcat
        [ "/2013-04-01/tags/"
        , toText _ctfrResourceType
        , "/"
        , toText _ctfrResourceId
        ]

instance ToQuery ChangeTagsForResource where
    toQuery = const mempty

instance ToHeaders ChangeTagsForResource

instance ToXMLRoot ChangeTagsForResource where
    toXMLRoot ChangeTagsForResource{..} = namespace ns "ChangeTagsForResource"
        [ "AddTags"       =@ _ctfrAddTags
        , "RemoveTagKeys" =@ _ctfrRemoveTagKeys
        ]

instance ToXML ChangeTagsForResource

instance AWSRequest ChangeTagsForResource where
    type Sv ChangeTagsForResource = Route53
    type Rs ChangeTagsForResource = ChangeTagsForResourceResponse

    request  = post
    response = nullResponse ChangeTagsForResourceResponse
