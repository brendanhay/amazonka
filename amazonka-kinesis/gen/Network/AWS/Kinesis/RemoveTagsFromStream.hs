{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.RemoveTagsFromStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from the specified Amazon Kinesis stream.
--
-- If you specify a tag that does not exist, it is ignored.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_RemoveTagsFromStream.html>
module Network.AWS.Kinesis.RemoveTagsFromStream
    (
    -- * Request
      RemoveTagsFromStream
    -- ** Request constructor
    , removeTagsFromStream
    -- ** Request lenses
    , rtfsrqStreamName
    , rtfsrqTagKeys

    -- * Response
    , RemoveTagsFromStreamResponse
    -- ** Response constructor
    , removeTagsFromStreamResponse
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for @RemoveTagsFromStream@.
--
-- /See:/ 'removeTagsFromStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfsrqStreamName'
--
-- * 'rtfsrqTagKeys'
data RemoveTagsFromStream = RemoveTagsFromStream'
    { _rtfsrqStreamName :: !Text
    , _rtfsrqTagKeys    :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromStream' smart constructor.
removeTagsFromStream :: Text -> NonEmpty Text -> RemoveTagsFromStream
removeTagsFromStream pStreamName pTagKeys =
    RemoveTagsFromStream'
    { _rtfsrqStreamName = pStreamName
    , _rtfsrqTagKeys = _List1 # pTagKeys
    }

-- | The name of the stream.
rtfsrqStreamName :: Lens' RemoveTagsFromStream Text
rtfsrqStreamName = lens _rtfsrqStreamName (\ s a -> s{_rtfsrqStreamName = a});

-- | A list of tag keys. Each corresponding tag is removed from the stream.
rtfsrqTagKeys :: Lens' RemoveTagsFromStream (NonEmpty Text)
rtfsrqTagKeys = lens _rtfsrqTagKeys (\ s a -> s{_rtfsrqTagKeys = a}) . _List1;

instance AWSRequest RemoveTagsFromStream where
        type Sv RemoveTagsFromStream = Kinesis
        type Rs RemoveTagsFromStream =
             RemoveTagsFromStreamResponse
        request = postJSON
        response = receiveNull RemoveTagsFromStreamResponse'

instance ToHeaders RemoveTagsFromStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.RemoveTagsFromStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTagsFromStream where
        toJSON RemoveTagsFromStream'{..}
          = object
              ["StreamName" .= _rtfsrqStreamName,
               "TagKeys" .= _rtfsrqTagKeys]

instance ToPath RemoveTagsFromStream where
        toPath = const "/"

instance ToQuery RemoveTagsFromStream where
        toQuery = const mempty

-- | /See:/ 'removeTagsFromStreamResponse' smart constructor.
data RemoveTagsFromStreamResponse =
    RemoveTagsFromStreamResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromStreamResponse' smart constructor.
removeTagsFromStreamResponse :: RemoveTagsFromStreamResponse
removeTagsFromStreamResponse = RemoveTagsFromStreamResponse'
