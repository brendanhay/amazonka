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

-- Module      : Network.AWS.EMR.AddTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds tags to an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon EMR
-- resource allocation costs. For more information, see <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMRResources>.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddTags.html>
module Network.AWS.EMR.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atResourceId
    , atTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , addTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data AddTags = AddTags
    { _atResourceId :: Text
    , _atTags       :: List "Tags" Tag
    } deriving (Eq, Show)

-- | 'AddTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atResourceId' @::@ 'Text'
--
-- * 'atTags' @::@ ['Tag']
--
addTags :: Text -- ^ 'atResourceId'
        -> AddTags
addTags p1 = AddTags
    { _atResourceId = p1
    , _atTags       = mempty
    }

-- | The Amazon EMR resource identifier to which tags will be added. This value
-- must be a cluster identifier.
--
atResourceId :: Lens' AddTags Text
atResourceId = lens _atResourceId (\s a -> s { _atResourceId = a })

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances. Tags are user-defined key/value pairs that consist of a required
-- key string with a maximum of 128 characters, and an optional value string
-- with a maximum of 256 characters.
--
atTags :: Lens' AddTags [Tag]
atTags = lens _atTags (\s a -> s { _atTags = a }) . _List

data AddTagsResponse = AddTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AddTagsResponse' constructor.
addTagsResponse :: AddTagsResponse
addTagsResponse = AddTagsResponse

instance ToPath AddTags where
    toPath = const "/"

instance ToQuery AddTags where
    toQuery = const mempty

instance ToHeaders AddTags

instance ToJSON AddTags where
    toJSON AddTags{..} = object
        [ "ResourceId" .= _atResourceId
        , "Tags"       .= _atTags
        ]

instance AWSRequest AddTags where
    type Sv AddTags = EMR
    type Rs AddTags = AddTagsResponse

    request  = post "AddTags"
    response = nullResponse AddTagsResponse
