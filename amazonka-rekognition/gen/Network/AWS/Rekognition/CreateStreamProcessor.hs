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
-- Module      : Network.AWS.Rekognition.CreateStreamProcessor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Rekognition stream processor that you can use to detect and recognize faces in a streaming video.
--
--
-- Rekognition Video is a consumer of live video from Amazon Kinesis Video Streams. Rekognition Video sends analysis results to Amazon Kinesis Data Streams.
--
-- You provide as input a Kinesis video stream (@Input@ ) and a Kinesis data stream (@Output@ ) stream. You also specify the face recognition criteria in @Settings@ . For example, the collection containing faces that you want to recognize. Use @Name@ to assign an identifier for the stream processor. You use @Name@ to manage the stream processor. For example, you can start processing the source video by calling with the @Name@ field.
--
-- After you have finished analyzing a streaming video, use to stop processing. You can delete the stream processor by calling .
--
module Network.AWS.Rekognition.CreateStreamProcessor
    (
    -- * Creating a Request
      createStreamProcessor
    , CreateStreamProcessor
    -- * Request Lenses
    , cspInput
    , cspOutput
    , cspName
    , cspSettings
    , cspRoleARN

    -- * Destructuring the Response
    , createStreamProcessorResponse
    , CreateStreamProcessorResponse
    -- * Response Lenses
    , csprsStreamProcessorARN
    , csprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStreamProcessor' smart constructor.
data CreateStreamProcessor = CreateStreamProcessor'
  { _cspInput    :: !StreamProcessorInput
  , _cspOutput   :: !StreamProcessorOutput
  , _cspName     :: !Text
  , _cspSettings :: !StreamProcessorSettings
  , _cspRoleARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cspInput' - Kinesis video stream stream that provides the source streaming video. If you are using the AWS CLI, the parameter name is @StreamProcessorInput@ .
--
-- * 'cspOutput' - Kinesis data stream stream to which Rekognition Video puts the analysis results. If you are using the AWS CLI, the parameter name is @StreamProcessorOutput@ .
--
-- * 'cspName' - An identifier you assign to the stream processor. You can use @Name@ to manage the stream processor. For example, you can get the current status of the stream processor by calling . @Name@ is idempotent.
--
-- * 'cspSettings' - Face recognition input parameters to be used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
--
-- * 'cspRoleARN' - ARN of the IAM role that allows access to the stream processor.
createStreamProcessor
    :: StreamProcessorInput -- ^ 'cspInput'
    -> StreamProcessorOutput -- ^ 'cspOutput'
    -> Text -- ^ 'cspName'
    -> StreamProcessorSettings -- ^ 'cspSettings'
    -> Text -- ^ 'cspRoleARN'
    -> CreateStreamProcessor
createStreamProcessor pInput_ pOutput_ pName_ pSettings_ pRoleARN_ =
  CreateStreamProcessor'
    { _cspInput = pInput_
    , _cspOutput = pOutput_
    , _cspName = pName_
    , _cspSettings = pSettings_
    , _cspRoleARN = pRoleARN_
    }


-- | Kinesis video stream stream that provides the source streaming video. If you are using the AWS CLI, the parameter name is @StreamProcessorInput@ .
cspInput :: Lens' CreateStreamProcessor StreamProcessorInput
cspInput = lens _cspInput (\ s a -> s{_cspInput = a})

-- | Kinesis data stream stream to which Rekognition Video puts the analysis results. If you are using the AWS CLI, the parameter name is @StreamProcessorOutput@ .
cspOutput :: Lens' CreateStreamProcessor StreamProcessorOutput
cspOutput = lens _cspOutput (\ s a -> s{_cspOutput = a})

-- | An identifier you assign to the stream processor. You can use @Name@ to manage the stream processor. For example, you can get the current status of the stream processor by calling . @Name@ is idempotent.
cspName :: Lens' CreateStreamProcessor Text
cspName = lens _cspName (\ s a -> s{_cspName = a})

-- | Face recognition input parameters to be used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
cspSettings :: Lens' CreateStreamProcessor StreamProcessorSettings
cspSettings = lens _cspSettings (\ s a -> s{_cspSettings = a})

-- | ARN of the IAM role that allows access to the stream processor.
cspRoleARN :: Lens' CreateStreamProcessor Text
cspRoleARN = lens _cspRoleARN (\ s a -> s{_cspRoleARN = a})

instance AWSRequest CreateStreamProcessor where
        type Rs CreateStreamProcessor =
             CreateStreamProcessorResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 CreateStreamProcessorResponse' <$>
                   (x .?> "StreamProcessorArn") <*> (pure (fromEnum s)))

instance Hashable CreateStreamProcessor where

instance NFData CreateStreamProcessor where

instance ToHeaders CreateStreamProcessor where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.CreateStreamProcessor" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateStreamProcessor where
        toJSON CreateStreamProcessor'{..}
          = object
              (catMaybes
                 [Just ("Input" .= _cspInput),
                  Just ("Output" .= _cspOutput),
                  Just ("Name" .= _cspName),
                  Just ("Settings" .= _cspSettings),
                  Just ("RoleArn" .= _cspRoleARN)])

instance ToPath CreateStreamProcessor where
        toPath = const "/"

instance ToQuery CreateStreamProcessor where
        toQuery = const mempty

-- | /See:/ 'createStreamProcessorResponse' smart constructor.
data CreateStreamProcessorResponse = CreateStreamProcessorResponse'
  { _csprsStreamProcessorARN :: !(Maybe Text)
  , _csprsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamProcessorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csprsStreamProcessorARN' - ARN for the newly create stream processor.
--
-- * 'csprsResponseStatus' - -- | The response status code.
createStreamProcessorResponse
    :: Int -- ^ 'csprsResponseStatus'
    -> CreateStreamProcessorResponse
createStreamProcessorResponse pResponseStatus_ =
  CreateStreamProcessorResponse'
    { _csprsStreamProcessorARN = Nothing
    , _csprsResponseStatus = pResponseStatus_
    }


-- | ARN for the newly create stream processor.
csprsStreamProcessorARN :: Lens' CreateStreamProcessorResponse (Maybe Text)
csprsStreamProcessorARN = lens _csprsStreamProcessorARN (\ s a -> s{_csprsStreamProcessorARN = a})

-- | -- | The response status code.
csprsResponseStatus :: Lens' CreateStreamProcessorResponse Int
csprsResponseStatus = lens _csprsResponseStatus (\ s a -> s{_csprsResponseStatus = a})

instance NFData CreateStreamProcessorResponse where
