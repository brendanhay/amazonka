{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
import Network.AWS.Request
import Network.AWS.Kinesis.Types

data AddTagsToStream = AddTagsToStream
    { _attsStreamName :: Text
    , _attsTags       :: Map Text Text
    } deriving (Eq, Show, Generic)

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
attsTags = lens _attsTags (\s a -> s { _attsTags = a })
    . _Map

instance ToPath AddTagsToStream where
    toPath = const "/"

instance ToQuery AddTagsToStream where
    toQuery = const mempty

instance ToHeaders AddTagsToStream

instance ToBody AddTagsToStream where
    toBody = toBody . encode . _attsStreamName

data AddTagsToStreamResponse = AddTagsToStreamResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AddTagsToStreamResponse' constructor.
addTagsToStreamResponse :: AddTagsToStreamResponse
addTagsToStreamResponse = AddTagsToStreamResponse

instance AWSRequest AddTagsToStream where
    type Sv AddTagsToStream = Kinesis
    type Rs AddTagsToStream = AddTagsToStreamResponse

    request  = post
    response = nullaryResponse AddTagsToStreamResponse
