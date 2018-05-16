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
-- Module      : Network.AWS.KinesisVideo.CreateStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Kinesis video stream.
--
--
-- When you create a new stream, Kinesis Video Streams assigns it a version number. When you change the stream's metadata, Kinesis Video Streams updates the version.
--
-- @CreateStream@ is an asynchronous operation.
--
-- For information about how the service works, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/how-it-works.html How it Works> .
--
-- You must have permissions for the @KinesisVideo:CreateStream@ action.
--
module Network.AWS.KinesisVideo.CreateStream
    (
    -- * Creating a Request
      createStream
    , CreateStream
    -- * Request Lenses
    , csMediaType
    , csDataRetentionInHours
    , csKMSKeyId
    , csDeviceName
    , csStreamName

    -- * Destructuring the Response
    , createStreamResponse
    , CreateStreamResponse
    -- * Response Lenses
    , csrsStreamARN
    , csrsResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStream' smart constructor.
data CreateStream = CreateStream'
  { _csMediaType            :: !(Maybe Text)
  , _csDataRetentionInHours :: !(Maybe Nat)
  , _csKMSKeyId             :: !(Maybe Text)
  , _csDeviceName           :: !(Maybe Text)
  , _csStreamName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csMediaType' - The media type of the stream. Consumers of the stream can use this information when processing the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> for guidelines. To play video on the console, the media must be H.264 encoded, and you need to specify this video type in this parameter as @video/h264@ .  This parameter is optional; the default value is @null@ (or empty in JSON).
--
-- * 'csDataRetentionInHours' - The number of hours that you want to retain the data in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream. The default value is 0, indicating that the stream does not persist data.
--
-- * 'csKMSKeyId' - The ID of the AWS Key Management Service (AWS KMS) key that you want Kinesis Video Streams to use to encrypt stream data. If no key ID is specified, the default, Kinesis Video-managed key (@aws/kinesisvideo@ ) is used. For more information, see <http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey> .
--
-- * 'csDeviceName' - The name of the device that is writing to the stream.
--
-- * 'csStreamName' - A name for the stream that you are creating. The stream name is an identifier for the stream, and must be unique for each account and region.
createStream
    :: Text -- ^ 'csStreamName'
    -> CreateStream
createStream pStreamName_ =
  CreateStream'
    { _csMediaType = Nothing
    , _csDataRetentionInHours = Nothing
    , _csKMSKeyId = Nothing
    , _csDeviceName = Nothing
    , _csStreamName = pStreamName_
    }


-- | The media type of the stream. Consumers of the stream can use this information when processing the stream. For more information about media types, see <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types> . If you choose to specify the @MediaType@ , see <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements> for guidelines. To play video on the console, the media must be H.264 encoded, and you need to specify this video type in this parameter as @video/h264@ .  This parameter is optional; the default value is @null@ (or empty in JSON).
csMediaType :: Lens' CreateStream (Maybe Text)
csMediaType = lens _csMediaType (\ s a -> s{_csMediaType = a})

-- | The number of hours that you want to retain the data in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream. The default value is 0, indicating that the stream does not persist data.
csDataRetentionInHours :: Lens' CreateStream (Maybe Natural)
csDataRetentionInHours = lens _csDataRetentionInHours (\ s a -> s{_csDataRetentionInHours = a}) . mapping _Nat

-- | The ID of the AWS Key Management Service (AWS KMS) key that you want Kinesis Video Streams to use to encrypt stream data. If no key ID is specified, the default, Kinesis Video-managed key (@aws/kinesisvideo@ ) is used. For more information, see <http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey> .
csKMSKeyId :: Lens' CreateStream (Maybe Text)
csKMSKeyId = lens _csKMSKeyId (\ s a -> s{_csKMSKeyId = a})

-- | The name of the device that is writing to the stream.
csDeviceName :: Lens' CreateStream (Maybe Text)
csDeviceName = lens _csDeviceName (\ s a -> s{_csDeviceName = a})

-- | A name for the stream that you are creating. The stream name is an identifier for the stream, and must be unique for each account and region.
csStreamName :: Lens' CreateStream Text
csStreamName = lens _csStreamName (\ s a -> s{_csStreamName = a})

instance AWSRequest CreateStream where
        type Rs CreateStream = CreateStreamResponse
        request = postJSON kinesisVideo
        response
          = receiveJSON
              (\ s h x ->
                 CreateStreamResponse' <$>
                   (x .?> "StreamARN") <*> (pure (fromEnum s)))

instance Hashable CreateStream where

instance NFData CreateStream where

instance ToHeaders CreateStream where
        toHeaders = const mempty

instance ToJSON CreateStream where
        toJSON CreateStream'{..}
          = object
              (catMaybes
                 [("MediaType" .=) <$> _csMediaType,
                  ("DataRetentionInHours" .=) <$>
                    _csDataRetentionInHours,
                  ("KmsKeyId" .=) <$> _csKMSKeyId,
                  ("DeviceName" .=) <$> _csDeviceName,
                  Just ("StreamName" .= _csStreamName)])

instance ToPath CreateStream where
        toPath = const "/createStream"

instance ToQuery CreateStream where
        toQuery = const mempty

-- | /See:/ 'createStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  { _csrsStreamARN      :: !(Maybe Text)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsStreamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createStreamResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateStreamResponse
createStreamResponse pResponseStatus_ =
  CreateStreamResponse'
    {_csrsStreamARN = Nothing, _csrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the stream.
csrsStreamARN :: Lens' CreateStreamResponse (Maybe Text)
csrsStreamARN = lens _csrsStreamARN (\ s a -> s{_csrsStreamARN = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateStreamResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateStreamResponse where
