{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectModerationLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects unsafe content in a specified JPEG or PNG format image. Use @DetectModerationLabels@ to moderate images depending on your requirements. For example, you might want to filter images that contain nudity, but not images containing suggestive content.
--
--
-- To filter images, use the labels returned by @DetectModerationLabels@ to determine which types of content are appropriate.
--
-- For information about moderation labels, see Detecting Unsafe Content in the Amazon Rekognition Developer Guide.
--
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
module Network.AWS.Rekognition.DetectModerationLabels
  ( -- * Creating a Request
    detectModerationLabels,
    DetectModerationLabels,

    -- * Request Lenses
    dmlHumanLoopConfig,
    dmlMinConfidence,
    dmlImage,

    -- * Destructuring the Response
    detectModerationLabelsResponse,
    DetectModerationLabelsResponse,

    -- * Response Lenses
    dmlrsHumanLoopActivationOutput,
    dmlrsModerationModelVersion,
    dmlrsModerationLabels,
    dmlrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectModerationLabels' smart constructor.
data DetectModerationLabels = DetectModerationLabels'
  { _dmlHumanLoopConfig ::
      !(Maybe HumanLoopConfig),
    _dmlMinConfidence :: !(Maybe Double),
    _dmlImage :: !Image
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectModerationLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmlHumanLoopConfig' - Sets up the configuration for human evaluation, including the FlowDefinition the image will be sent to.
--
-- * 'dmlMinConfidence' - Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
--
-- * 'dmlImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.  If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
detectModerationLabels ::
  -- | 'dmlImage'
  Image ->
  DetectModerationLabels
detectModerationLabels pImage_ =
  DetectModerationLabels'
    { _dmlHumanLoopConfig = Nothing,
      _dmlMinConfidence = Nothing,
      _dmlImage = pImage_
    }

-- | Sets up the configuration for human evaluation, including the FlowDefinition the image will be sent to.
dmlHumanLoopConfig :: Lens' DetectModerationLabels (Maybe HumanLoopConfig)
dmlHumanLoopConfig = lens _dmlHumanLoopConfig (\s a -> s {_dmlHumanLoopConfig = a})

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
dmlMinConfidence :: Lens' DetectModerationLabels (Maybe Double)
dmlMinConfidence = lens _dmlMinConfidence (\s a -> s {_dmlMinConfidence = a})

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.  If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
dmlImage :: Lens' DetectModerationLabels Image
dmlImage = lens _dmlImage (\s a -> s {_dmlImage = a})

instance AWSRequest DetectModerationLabels where
  type Rs DetectModerationLabels = DetectModerationLabelsResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          DetectModerationLabelsResponse'
            <$> (x .?> "HumanLoopActivationOutput")
            <*> (x .?> "ModerationModelVersion")
            <*> (x .?> "ModerationLabels" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DetectModerationLabels

instance NFData DetectModerationLabels

instance ToHeaders DetectModerationLabels where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.DetectModerationLabels" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DetectModerationLabels where
  toJSON DetectModerationLabels' {..} =
    object
      ( catMaybes
          [ ("HumanLoopConfig" .=) <$> _dmlHumanLoopConfig,
            ("MinConfidence" .=) <$> _dmlMinConfidence,
            Just ("Image" .= _dmlImage)
          ]
      )

instance ToPath DetectModerationLabels where
  toPath = const "/"

instance ToQuery DetectModerationLabels where
  toQuery = const mempty

-- | /See:/ 'detectModerationLabelsResponse' smart constructor.
data DetectModerationLabelsResponse = DetectModerationLabelsResponse'
  { _dmlrsHumanLoopActivationOutput ::
      !( Maybe
           HumanLoopActivationOutput
       ),
    _dmlrsModerationModelVersion ::
      !(Maybe Text),
    _dmlrsModerationLabels ::
      !(Maybe [ModerationLabel]),
    _dmlrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectModerationLabelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmlrsHumanLoopActivationOutput' - Shows the results of the human in the loop evaluation.
--
-- * 'dmlrsModerationModelVersion' - Version number of the moderation detection model that was used to detect unsafe content.
--
-- * 'dmlrsModerationLabels' - Array of detected Moderation labels and the time, in milliseconds from the start of the video, they were detected.
--
-- * 'dmlrsResponseStatus' - -- | The response status code.
detectModerationLabelsResponse ::
  -- | 'dmlrsResponseStatus'
  Int ->
  DetectModerationLabelsResponse
detectModerationLabelsResponse pResponseStatus_ =
  DetectModerationLabelsResponse'
    { _dmlrsHumanLoopActivationOutput =
        Nothing,
      _dmlrsModerationModelVersion = Nothing,
      _dmlrsModerationLabels = Nothing,
      _dmlrsResponseStatus = pResponseStatus_
    }

-- | Shows the results of the human in the loop evaluation.
dmlrsHumanLoopActivationOutput :: Lens' DetectModerationLabelsResponse (Maybe HumanLoopActivationOutput)
dmlrsHumanLoopActivationOutput = lens _dmlrsHumanLoopActivationOutput (\s a -> s {_dmlrsHumanLoopActivationOutput = a})

-- | Version number of the moderation detection model that was used to detect unsafe content.
dmlrsModerationModelVersion :: Lens' DetectModerationLabelsResponse (Maybe Text)
dmlrsModerationModelVersion = lens _dmlrsModerationModelVersion (\s a -> s {_dmlrsModerationModelVersion = a})

-- | Array of detected Moderation labels and the time, in milliseconds from the start of the video, they were detected.
dmlrsModerationLabels :: Lens' DetectModerationLabelsResponse [ModerationLabel]
dmlrsModerationLabels = lens _dmlrsModerationLabels (\s a -> s {_dmlrsModerationLabels = a}) . _Default . _Coerce

-- | -- | The response status code.
dmlrsResponseStatus :: Lens' DetectModerationLabelsResponse Int
dmlrsResponseStatus = lens _dmlrsResponseStatus (\s a -> s {_dmlrsResponseStatus = a})

instance NFData DetectModerationLabelsResponse
