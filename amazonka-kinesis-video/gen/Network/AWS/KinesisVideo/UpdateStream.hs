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
-- Module      : Network.AWS.KinesisVideo.UpdateStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates stream metadata, such as the device name and media type.
--
--
-- You must provide the stream name or the Amazon Resource Name (ARN) of the stream.
--
-- To make sure that you have the latest version of the stream before updating it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the @DescribeStream@ API.
--
-- @UpdateStream@ is an asynchronous operation, and takes time to complete.
--
module Network.AWS.KinesisVideo.UpdateStream
    (
    -- * Creating a Request
      updateStream
    , UpdateStream
    -- * Request Lenses
    , uMediaType
    , uStreamARN
    , uDeviceName
    , uStreamName
    , uCurrentVersion

    -- * Destructuring the Response
    , updateStreamResponse
    , UpdateStreamResponse
    -- * Response Lenses
    , usrsResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStream' smart constructor.
data UpdateStream = UpdateStream'
  { _uMediaType      :: !(Maybe Text)
  , _uStreamARN      :: !(Maybe Text)
  , _uDeviceName     :: !(Maybe Text)
  , _uStreamName     :: !(Maybe Text)
  , _uCurrentVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uMediaType' - The stream's media type. Use @MediaType@ to specify the type of content that the stream contains to the consumers of the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.sietf.org/html/rfc6838#section-4.2 Naming Requirements> . To play video on the console, you must specify the correct video type. For example, if the video in the stream is H.264, specify @video/h264@ as the @MediaType@ .
--
-- * 'uStreamARN' - The ARN of the stream whose metadata you want to update.
--
-- * 'uDeviceName' - The name of the device that is writing to the stream.
--
-- * 'uStreamName' - The name of the stream whose metadata you want to update. The stream name is an identifier for the stream, and must be unique for each account and region.
--
-- * 'uCurrentVersion' - The version of the stream whose metadata you want to update.
updateStream
    :: Text -- ^ 'uCurrentVersion'
    -> UpdateStream
updateStream pCurrentVersion_ =
  UpdateStream'
    { _uMediaType = Nothing
    , _uStreamARN = Nothing
    , _uDeviceName = Nothing
    , _uStreamName = Nothing
    , _uCurrentVersion = pCurrentVersion_
    }


-- | The stream's media type. Use @MediaType@ to specify the type of content that the stream contains to the consumers of the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.sietf.org/html/rfc6838#section-4.2 Naming Requirements> . To play video on the console, you must specify the correct video type. For example, if the video in the stream is H.264, specify @video/h264@ as the @MediaType@ .
uMediaType :: Lens' UpdateStream (Maybe Text)
uMediaType = lens _uMediaType (\ s a -> s{_uMediaType = a})

-- | The ARN of the stream whose metadata you want to update.
uStreamARN :: Lens' UpdateStream (Maybe Text)
uStreamARN = lens _uStreamARN (\ s a -> s{_uStreamARN = a})

-- | The name of the device that is writing to the stream.
uDeviceName :: Lens' UpdateStream (Maybe Text)
uDeviceName = lens _uDeviceName (\ s a -> s{_uDeviceName = a})

-- | The name of the stream whose metadata you want to update. The stream name is an identifier for the stream, and must be unique for each account and region.
uStreamName :: Lens' UpdateStream (Maybe Text)
uStreamName = lens _uStreamName (\ s a -> s{_uStreamName = a})

-- | The version of the stream whose metadata you want to update.
uCurrentVersion :: Lens' UpdateStream Text
uCurrentVersion = lens _uCurrentVersion (\ s a -> s{_uCurrentVersion = a})

instance AWSRequest UpdateStream where
        type Rs UpdateStream = UpdateStreamResponse
        request = postJSON kinesisVideo
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateStreamResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateStream where

instance NFData UpdateStream where

instance ToHeaders UpdateStream where
        toHeaders = const mempty

instance ToJSON UpdateStream where
        toJSON UpdateStream'{..}
          = object
              (catMaybes
                 [("MediaType" .=) <$> _uMediaType,
                  ("StreamARN" .=) <$> _uStreamARN,
                  ("DeviceName" .=) <$> _uDeviceName,
                  ("StreamName" .=) <$> _uStreamName,
                  Just ("CurrentVersion" .= _uCurrentVersion)])

instance ToPath UpdateStream where
        toPath = const "/updateStream"

instance ToQuery UpdateStream where
        toQuery = const mempty

-- | /See:/ 'updateStreamResponse' smart constructor.
newtype UpdateStreamResponse = UpdateStreamResponse'
  { _usrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateStreamResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateStreamResponse
updateStreamResponse pResponseStatus_ =
  UpdateStreamResponse' {_usrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateStreamResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateStreamResponse where
