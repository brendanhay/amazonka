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

-- Module      : Network.AWS.DataPipeline.RemoveTags
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

-- | Removes existing tags from the specified pipeline.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_RemoveTags.html>
module Network.AWS.DataPipeline.RemoveTags
    (
    -- * Request
      RemoveTags
    -- ** Request constructor
    , removeTags
    -- ** Request lenses
    , rtPipelineId
    , rtTagKeys

    -- * Response
    , RemoveTagsResponse
    -- ** Response constructor
    , removeTagsResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data RemoveTags = RemoveTags
    { _rtPipelineId :: Text
    , _rtTagKeys    :: List "tagKeys" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RemoveTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtPipelineId' @::@ 'Text'
--
-- * 'rtTagKeys' @::@ ['Text']
--
removeTags :: Text -- ^ 'rtPipelineId'
           -> RemoveTags
removeTags p1 = RemoveTags
    { _rtPipelineId = p1
    , _rtTagKeys    = mempty
    }

-- | The ID of the pipeline.
rtPipelineId :: Lens' RemoveTags Text
rtPipelineId = lens _rtPipelineId (\s a -> s { _rtPipelineId = a })

-- | The keys of the tags to remove.
rtTagKeys :: Lens' RemoveTags [Text]
rtTagKeys = lens _rtTagKeys (\s a -> s { _rtTagKeys = a }) . _List

data RemoveTagsResponse = RemoveTagsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'RemoveTagsResponse' constructor.
removeTagsResponse :: RemoveTagsResponse
removeTagsResponse = RemoveTagsResponse

instance ToPath RemoveTags where
    toPath = const "/"

instance ToQuery RemoveTags where
    toQuery = const mempty

instance ToHeaders RemoveTags

instance ToJSON RemoveTags where
    toJSON RemoveTags{..} = object
        [ "pipelineId" .= _rtPipelineId
        , "tagKeys"    .= _rtTagKeys
        ]

instance AWSRequest RemoveTags where
    type Sv RemoveTags = DataPipeline
    type Rs RemoveTags = RemoveTagsResponse

    request  = post "RemoveTags"
    response = nullResponse RemoveTagsResponse
