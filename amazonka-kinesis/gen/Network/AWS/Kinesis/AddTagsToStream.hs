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

-- Module      : Network.AWS.Kinesis.AddTagsToStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or updates tags for the specified Amazon Kinesis stream. Each stream
-- can have up to 10 tags. If tags have already been assigned to the stream,
-- AddTagsToStream overwrites any existing tags that correspond to the
-- specified tag keys.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_AddTagsToStream.html>
module Network.AWS.Kinesis.AddTagsToStream
    (
    -- * Request
      AddTagsToStream
    -- ** Request constructor
    , addTagsToStream
    -- ** Request lenses
    , attsStreamName
    , attsTags

    -- * Response
    , AddTagsToStreamResponse
    -- ** Response constructor
    , addTagsToStreamResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.Types
import qualified GHC.Exts

data AddTagsToStream = AddTagsToStream
    { _attsStreamName :: Text
    , _attsTags       :: Map Text Text
    } deriving (Eq, Show)

-- | 'AddTagsToStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attsStreamName' @::@ 'Text'
--
-- * 'attsTags' @::@ 'HashMap' 'Text' 'Text'
--
addTagsToStream :: Text -- ^ 'attsStreamName'
                -> AddTagsToStream
addTagsToStream p1 = AddTagsToStream
    { _attsStreamName = p1
    , _attsTags       = mempty
    }

-- | The name of the stream.
attsStreamName :: Lens' AddTagsToStream Text
attsStreamName = lens _attsStreamName (\s a -> s { _attsStreamName = a })

-- | The set of key-value pairs to use to create the tags.
attsTags :: Lens' AddTagsToStream (HashMap Text Text)
attsTags = lens _attsTags (\s a -> s { _attsTags = a }) . _Map

data AddTagsToStreamResponse = AddTagsToStreamResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AddTagsToStreamResponse' constructor.
addTagsToStreamResponse :: AddTagsToStreamResponse
addTagsToStreamResponse = AddTagsToStreamResponse

instance ToPath AddTagsToStream where
    toPath = const "/"

instance ToQuery AddTagsToStream where
    toQuery = const mempty

instance ToHeaders AddTagsToStream

instance ToJSON AddTagsToStream where
    toJSON AddTagsToStream{..} = object
        [ "StreamName" .= _attsStreamName
        , "Tags"       .= _attsTags
        ]

json

instance AWSRequest AddTagsToStream where
    type Sv AddTagsToStream = Kinesis
    type Rs AddTagsToStream = AddTagsToStreamResponse

    request  = post "AddTagsToStream"
    response = nullResponse AddTagsToStreamResponse
