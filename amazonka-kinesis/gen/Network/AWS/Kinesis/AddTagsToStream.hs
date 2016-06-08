{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.AddTagsToStream
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified Amazon Kinesis stream. Each stream can have up to 10 tags.
--
-- If tags have already been assigned to the stream, 'AddTagsToStream' overwrites any existing tags that correspond to the specified tag keys.
module Network.AWS.Kinesis.AddTagsToStream
    (
    -- * Creating a Request
      addTagsToStream
    , AddTagsToStream
    -- * Request Lenses
    , attsStreamName
    , attsTags

    -- * Destructuring the Response
    , addTagsToStreamResponse
    , AddTagsToStreamResponse
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Kinesis.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for 'AddTagsToStream'.
--
-- /See:/ 'addTagsToStream' smart constructor.
data AddTagsToStream = AddTagsToStream'
    { _attsStreamName :: !Text
    , _attsTags       :: !(Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddTagsToStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attsStreamName'
--
-- * 'attsTags'
addTagsToStream
    :: Text -- ^ 'attsStreamName'
    -> AddTagsToStream
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
        type Rs AddTagsToStream = AddTagsToStreamResponse
        request = postJSON kinesis
        response = receiveNull AddTagsToStreamResponse'

instance Hashable AddTagsToStream

instance NFData AddTagsToStream

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
              (catMaybes
                 [Just ("StreamName" .= _attsStreamName),
                  Just ("Tags" .= _attsTags)])

instance ToPath AddTagsToStream where
        toPath = const "/"

instance ToQuery AddTagsToStream where
        toQuery = const mempty

-- | /See:/ 'addTagsToStreamResponse' smart constructor.
data AddTagsToStreamResponse =
    AddTagsToStreamResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddTagsToStreamResponse' with the minimum fields required to make a request.
--
addTagsToStreamResponse
    :: AddTagsToStreamResponse
addTagsToStreamResponse = AddTagsToStreamResponse'

instance NFData AddTagsToStreamResponse
