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
-- Module      : Network.AWS.KinesisVideo.TagStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a stream. A /tag/ is a key-value pair (the value is optional) that you can define and assign to AWS resources. If you specify a tag that already exists, the tag value is replaced with the value that you specify in the request. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
--
-- You must provide either the @StreamName@ or the @StreamARN@ .
--
-- This operation requires permission for the @KinesisVideo:TagStream@ action.
--
-- Kinesis video streams support up to 50 tags.
--
module Network.AWS.KinesisVideo.TagStream
    (
    -- * Creating a Request
      tagStream
    , TagStream
    -- * Request Lenses
    , tsStreamARN
    , tsStreamName
    , tsTags

    -- * Destructuring the Response
    , tagStreamResponse
    , TagStreamResponse
    -- * Response Lenses
    , tsrsResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagStream' smart constructor.
data TagStream = TagStream'
  { _tsStreamARN  :: !(Maybe Text)
  , _tsStreamName :: !(Maybe Text)
  , _tsTags       :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsStreamARN' - The Amazon Resource Name (ARN) of the resource that you want to add the tag or tags to.
--
-- * 'tsStreamName' - The name of the stream that you want to add the tag or tags to.
--
-- * 'tsTags' - A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
tagStream
    :: TagStream
tagStream =
  TagStream' {_tsStreamARN = Nothing, _tsStreamName = Nothing, _tsTags = mempty}


-- | The Amazon Resource Name (ARN) of the resource that you want to add the tag or tags to.
tsStreamARN :: Lens' TagStream (Maybe Text)
tsStreamARN = lens _tsStreamARN (\ s a -> s{_tsStreamARN = a})

-- | The name of the stream that you want to add the tag or tags to.
tsStreamName :: Lens' TagStream (Maybe Text)
tsStreamName = lens _tsStreamName (\ s a -> s{_tsStreamName = a})

-- | A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
tsTags :: Lens' TagStream (HashMap Text Text)
tsTags = lens _tsTags (\ s a -> s{_tsTags = a}) . _Map

instance AWSRequest TagStream where
        type Rs TagStream = TagStreamResponse
        request = postJSON kinesisVideo
        response
          = receiveEmpty
              (\ s h x ->
                 TagStreamResponse' <$> (pure (fromEnum s)))

instance Hashable TagStream where

instance NFData TagStream where

instance ToHeaders TagStream where
        toHeaders = const mempty

instance ToJSON TagStream where
        toJSON TagStream'{..}
          = object
              (catMaybes
                 [("StreamARN" .=) <$> _tsStreamARN,
                  ("StreamName" .=) <$> _tsStreamName,
                  Just ("Tags" .= _tsTags)])

instance ToPath TagStream where
        toPath = const "/tagStream"

instance ToQuery TagStream where
        toQuery = const mempty

-- | /See:/ 'tagStreamResponse' smart constructor.
newtype TagStreamResponse = TagStreamResponse'
  { _tsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsrsResponseStatus' - -- | The response status code.
tagStreamResponse
    :: Int -- ^ 'tsrsResponseStatus'
    -> TagStreamResponse
tagStreamResponse pResponseStatus_ =
  TagStreamResponse' {_tsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
tsrsResponseStatus :: Lens' TagStreamResponse Int
tsrsResponseStatus = lens _tsrsResponseStatus (\ s a -> s{_tsrsResponseStatus = a})

instance NFData TagStreamResponse where
