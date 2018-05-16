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
-- Module      : Network.AWS.KinesisVideoMedia.GetMedia
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to retrieve media content from a Kinesis video stream. In the request, you identify stream name or stream Amazon Resource Name (ARN), and the starting chunk. Kinesis Video Streams then returns a stream of chunks in order by fragment number.
--
--
-- When you put media data (fragments) on a stream, Kinesis Video Streams stores each incoming fragment and related metadata in what is called a "chunk." For more information, see . The @GetMedia@ API returns a stream of these chunks starting from the chunk that you specify in the request.
--
-- The following limits apply when using the @GetMedia@ API:
--
--     * A client can call @GetMedia@ up to five times per second per stream.
--
--     * Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a @GetMedia@ session.
--
--
--
module Network.AWS.KinesisVideoMedia.GetMedia
    (
    -- * Creating a Request
      getMedia
    , GetMedia
    -- * Request Lenses
    , gmStreamARN
    , gmStreamName
    , gmStartSelector

    -- * Destructuring the Response
    , getMediaResponse
    , GetMediaResponse
    -- * Response Lenses
    , gmrsContentType
    , gmrsResponseStatus
    , gmrsPayload
    ) where

import Network.AWS.KinesisVideoMedia.Types
import Network.AWS.KinesisVideoMedia.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMedia' smart constructor.
data GetMedia = GetMedia'
  { _gmStreamARN     :: !(Maybe Text)
  , _gmStreamName    :: !(Maybe Text)
  , _gmStartSelector :: !StartSelector
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMedia' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmStreamARN' - The ARN of the stream from where you want to get the media content. If you don't specify the @streamARN@ , you must specify the @streamName@ .
--
-- * 'gmStreamName' - The Kinesis video stream name from where you want to get the media content. If you don't specify the @streamName@ , you must specify the @streamARN@ .
--
-- * 'gmStartSelector' - Identifies the starting chunk to get from the specified stream.
getMedia
    :: StartSelector -- ^ 'gmStartSelector'
    -> GetMedia
getMedia pStartSelector_ =
  GetMedia'
    { _gmStreamARN = Nothing
    , _gmStreamName = Nothing
    , _gmStartSelector = pStartSelector_
    }


-- | The ARN of the stream from where you want to get the media content. If you don't specify the @streamARN@ , you must specify the @streamName@ .
gmStreamARN :: Lens' GetMedia (Maybe Text)
gmStreamARN = lens _gmStreamARN (\ s a -> s{_gmStreamARN = a})

-- | The Kinesis video stream name from where you want to get the media content. If you don't specify the @streamName@ , you must specify the @streamARN@ .
gmStreamName :: Lens' GetMedia (Maybe Text)
gmStreamName = lens _gmStreamName (\ s a -> s{_gmStreamName = a})

-- | Identifies the starting chunk to get from the specified stream.
gmStartSelector :: Lens' GetMedia StartSelector
gmStartSelector = lens _gmStartSelector (\ s a -> s{_gmStartSelector = a})

instance AWSRequest GetMedia where
        type Rs GetMedia = GetMediaResponse
        request = postJSON kinesisVideoMedia
        response
          = receiveBody
              (\ s h x ->
                 GetMediaResponse' <$>
                   (h .#? "Content-Type") <*> (pure (fromEnum s)) <*>
                     (pure x))

instance Hashable GetMedia where

instance NFData GetMedia where

instance ToHeaders GetMedia where
        toHeaders = const mempty

instance ToJSON GetMedia where
        toJSON GetMedia'{..}
          = object
              (catMaybes
                 [("StreamARN" .=) <$> _gmStreamARN,
                  ("StreamName" .=) <$> _gmStreamName,
                  Just ("StartSelector" .= _gmStartSelector)])

instance ToPath GetMedia where
        toPath = const "/getMedia"

instance ToQuery GetMedia where
        toQuery = const mempty

-- | /See:/ 'getMediaResponse' smart constructor.
data GetMediaResponse = GetMediaResponse'
  { _gmrsContentType    :: !(Maybe Text)
  , _gmrsResponseStatus :: !Int
  , _gmrsPayload        :: !RsBody
  } deriving (Show, Generic)


-- | Creates a value of 'GetMediaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmrsContentType' - The content type of the requested media.
--
-- * 'gmrsResponseStatus' - -- | The response status code.
--
-- * 'gmrsPayload' - The payload Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see . The chunks that Kinesis Video Streams returns in the @GetMedia@ call also include the following additional Matroska (MKV) tags:      * AWS_KINESISVIDEO_CONTINUATION_TOKEN (UTF-8 string) - In the event your @GetMedia@ call terminates, you can use this continuation token in your next request to get the next chunk where the last request terminated.     * AWS_KINESISVIDEO_MILLIS_BEHIND_NOW (UTF-8 string) - Client applications can use this tag value to determine how far behind the chunk returned in the response is from the latest chunk on the stream.      * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.     * AWS_KINESISVIDEO_SERVER_TIMESTAMP - Server time stamp of the fragment.     * AWS_KINESISVIDEO_PRODUCER_TIMESTAMP - Producer time stamp of the fragment. The following tags will be present if an error occurs:     * AWS_KINESISVIDEO_ERROR_CODE - String description of an error that caused GetMedia to stop.     * AWS_KINESISVIDEO_ERROR_ID: Integer code of the error. The error codes are as follows:     * 3002 - Error writing to the stream     * 4000 - Requested fragment is not found     * 4500 - Access denied for the stream's KMS key     * 4501 - Stream's KMS key is disabled     * 4502 - Validation error on the Stream's KMS key     * 4503 - KMS key specified in the stream is unavailable     * 4504 - Invalid usage of the KMS key specified in the stream     * 4505 - Invalid state of the KMS key specified in the stream     * 4506 - Unable to find the KMS key specified in the stream     * 5000 - Internal error
getMediaResponse
    :: Int -- ^ 'gmrsResponseStatus'
    -> RsBody -- ^ 'gmrsPayload'
    -> GetMediaResponse
getMediaResponse pResponseStatus_ pPayload_ =
  GetMediaResponse'
    { _gmrsContentType = Nothing
    , _gmrsResponseStatus = pResponseStatus_
    , _gmrsPayload = pPayload_
    }


-- | The content type of the requested media.
gmrsContentType :: Lens' GetMediaResponse (Maybe Text)
gmrsContentType = lens _gmrsContentType (\ s a -> s{_gmrsContentType = a})

-- | -- | The response status code.
gmrsResponseStatus :: Lens' GetMediaResponse Int
gmrsResponseStatus = lens _gmrsResponseStatus (\ s a -> s{_gmrsResponseStatus = a})

-- | The payload Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see . The chunks that Kinesis Video Streams returns in the @GetMedia@ call also include the following additional Matroska (MKV) tags:      * AWS_KINESISVIDEO_CONTINUATION_TOKEN (UTF-8 string) - In the event your @GetMedia@ call terminates, you can use this continuation token in your next request to get the next chunk where the last request terminated.     * AWS_KINESISVIDEO_MILLIS_BEHIND_NOW (UTF-8 string) - Client applications can use this tag value to determine how far behind the chunk returned in the response is from the latest chunk on the stream.      * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.     * AWS_KINESISVIDEO_SERVER_TIMESTAMP - Server time stamp of the fragment.     * AWS_KINESISVIDEO_PRODUCER_TIMESTAMP - Producer time stamp of the fragment. The following tags will be present if an error occurs:     * AWS_KINESISVIDEO_ERROR_CODE - String description of an error that caused GetMedia to stop.     * AWS_KINESISVIDEO_ERROR_ID: Integer code of the error. The error codes are as follows:     * 3002 - Error writing to the stream     * 4000 - Requested fragment is not found     * 4500 - Access denied for the stream's KMS key     * 4501 - Stream's KMS key is disabled     * 4502 - Validation error on the Stream's KMS key     * 4503 - KMS key specified in the stream is unavailable     * 4504 - Invalid usage of the KMS key specified in the stream     * 4505 - Invalid state of the KMS key specified in the stream     * 4506 - Unable to find the KMS key specified in the stream     * 5000 - Internal error
gmrsPayload :: Lens' GetMediaResponse RsBody
gmrsPayload = lens _gmrsPayload (\ s a -> s{_gmrsPayload = a})
