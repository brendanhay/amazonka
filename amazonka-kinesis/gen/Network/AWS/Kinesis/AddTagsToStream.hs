{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.AddTagsToStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified Amazon Kinesis stream. Each
-- stream can have up to 10 tags.
--
-- If tags have already been assigned to the stream, @AddTagsToStream@
-- overwrites any existing tags that correspond to the specified tag keys.
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

import           Network.AWS.Kinesis.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for @AddTagsToStream@.
--
-- /See:/ 'addTagsToStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attsStreamName'
--
-- * 'attsTags'
data AddTagsToStream = AddTagsToStream'
    { _attsStreamName :: !Text
    , _attsTags       :: !(Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToStream' smart constructor.
addTagsToStream :: Text -> AddTagsToStream
addTagsToStream pStreamName_ =
    AddTagsToStream'
    { _attsStreamName = pStreamName_
    , _attsTags = mempty
    }

-- | The name of the stream.
attsStreamName :: Lens' AddTagsToStream Text
attsStreamName = lens _attsStreamName (\ s a -> s{_attsStreamName = a});

-- | The set of key-value pairs to use to create the tags.
attsTags :: Lens' AddTagsToStream (HashMap Text Text)
attsTags = lens _attsTags (\ s a -> s{_attsTags = a}) . _Map;

instance AWSRequest AddTagsToStream where
        type Sv AddTagsToStream = Kinesis
        type Rs AddTagsToStream = AddTagsToStreamResponse
        request = postJSON "AddTagsToStream"
        response = receiveNull AddTagsToStreamResponse'

instance ToHeaders AddTagsToStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.AddTagsToStream" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTagsToStream where
        toJSON AddTagsToStream'{..}
          = object
              ["StreamName" .= _attsStreamName,
               "Tags" .= _attsTags]

instance ToPath AddTagsToStream where
        toPath = const "/"

instance ToQuery AddTagsToStream where
        toQuery = const mempty

-- | /See:/ 'addTagsToStreamResponse' smart constructor.
data AddTagsToStreamResponse =
    AddTagsToStreamResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToStreamResponse' smart constructor.
addTagsToStreamResponse :: AddTagsToStreamResponse
addTagsToStreamResponse = AddTagsToStreamResponse'
