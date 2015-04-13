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

-- Module      : Network.AWS.DataPipeline.AddTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds or modifies tags for the specified pipeline.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_AddTags.html>
module Network.AWS.DataPipeline.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atPipelineId
    , atTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , addTagsResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data AddTags = AddTags
    { _atPipelineId :: Text
    , _atTags       :: List "tags" Tag
    } deriving (Eq, Read, Show)

-- | 'AddTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atPipelineId' @::@ 'Text'
--
-- * 'atTags' @::@ ['Tag']
--
addTags :: Text -- ^ 'atPipelineId'
        -> AddTags
addTags p1 = AddTags
    { _atPipelineId = p1
    , _atTags       = mempty
    }

-- | The ID of the pipeline.
atPipelineId :: Lens' AddTags Text
atPipelineId = lens _atPipelineId (\s a -> s { _atPipelineId = a })

-- | The tags to add, as key/value pairs.
atTags :: Lens' AddTags [Tag]
atTags = lens _atTags (\s a -> s { _atTags = a }) . _List

data AddTagsResponse = AddTagsResponse
    deriving (Eq, Ord, Read, Show, Generic)

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
        [ "pipelineId" .= _atPipelineId
        , "tags"       .= _atTags
        ]

instance AWSRequest AddTags where
    type Sv AddTags = DataPipeline
    type Rs AddTags = AddTagsResponse

    request  = post "AddTags"
    response = nullResponse AddTagsResponse
