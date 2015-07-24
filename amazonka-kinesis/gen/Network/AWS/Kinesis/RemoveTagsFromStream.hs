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
    , rtfsStreamName
    , rtfsTagKeys

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
-- * 'rtfsStreamName'
--
-- * 'rtfsTagKeys'
data RemoveTagsFromStream = RemoveTagsFromStream'
    { _rtfsStreamName :: !Text
    , _rtfsTagKeys    :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromStream' smart constructor.
removeTagsFromStream :: Text -> NonEmpty Text -> RemoveTagsFromStream
removeTagsFromStream pStreamName_ pTagKeys_ =
    RemoveTagsFromStream'
    { _rtfsStreamName = pStreamName_
    , _rtfsTagKeys = _List1 # pTagKeys_
    }

-- | The name of the stream.
rtfsStreamName :: Lens' RemoveTagsFromStream Text
rtfsStreamName = lens _rtfsStreamName (\ s a -> s{_rtfsStreamName = a});

-- | A list of tag keys. Each corresponding tag is removed from the stream.
rtfsTagKeys :: Lens' RemoveTagsFromStream (NonEmpty Text)
rtfsTagKeys = lens _rtfsTagKeys (\ s a -> s{_rtfsTagKeys = a}) . _List1;

instance AWSRequest RemoveTagsFromStream where
        type Sv RemoveTagsFromStream = Kinesis
        type Rs RemoveTagsFromStream =
             RemoveTagsFromStreamResponse
        request = postJSON "RemoveTagsFromStream"
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
              ["StreamName" .= _rtfsStreamName,
               "TagKeys" .= _rtfsTagKeys]

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
