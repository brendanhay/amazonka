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
-- Module      : Network.AWS.Rekognition.DetectCustomLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects custom labels in a supplied image by using an Amazon Rekognition Custom Labels model.
--
--
-- You specify which version of a model version to use by using the @ProjectVersionArn@ input parameter.
--
-- You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
--
-- For each object that the model version detects on an image, the API returns a (@CustomLabel@ ) object in an array (@CustomLabels@ ). Each @CustomLabel@ object provides the label name (@Name@ ), the level of confidence that the image contains the object (@Confidence@ ), and object location information, if it exists, for the label on the image (@Geometry@ ).
--
-- During training model calculates a threshold value that determines if a prediction for a label is true. By default, @DetectCustomLabels@ doesn't return labels whose confidence value is below the model's calculated threshold value. To filter labels that are returned, specify a value for @MinConfidence@ that is higher than the model's calculated threshold. You can get the model's calculated threshold from the model's training results shown in the Amazon Rekognition Custom Labels console. To get all labels, regardless of confidence, specify a @MinConfidence@ value of 0.
--
-- You can also add the @MaxResults@ parameter to limit the number of labels returned.
--
-- This is a stateless API operation. That is, the operation does not persist any data.
--
-- This operation requires permissions to perform the @rekognition:DetectCustomLabels@ action.
module Network.AWS.Rekognition.DetectCustomLabels
  ( -- * Creating a Request
    detectCustomLabels,
    DetectCustomLabels,

    -- * Request Lenses
    dclMinConfidence,
    dclMaxResults,
    dclProjectVersionARN,
    dclImage,

    -- * Destructuring the Response
    detectCustomLabelsResponse,
    DetectCustomLabelsResponse,

    -- * Response Lenses
    dclrsCustomLabels,
    dclrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectCustomLabels' smart constructor.
data DetectCustomLabels = DetectCustomLabels'
  { _dclMinConfidence ::
      !(Maybe Double),
    _dclMaxResults :: !(Maybe Nat),
    _dclProjectVersionARN :: !Text,
    _dclImage :: !Image
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectCustomLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dclMinConfidence' - Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence lower than this specified value. If you specify a value of 0, all labels are return, regardless of the default thresholds that the model version applies.
--
-- * 'dclMaxResults' - Maximum number of results you want the service to return in the response. The service returns the specified number of highest confidence labels ranked from highest confidence to lowest.
--
-- * 'dclProjectVersionARN' - The ARN of the model version that you want to use.
--
-- * 'dclImage' - Undocumented member.
detectCustomLabels ::
  -- | 'dclProjectVersionARN'
  Text ->
  -- | 'dclImage'
  Image ->
  DetectCustomLabels
detectCustomLabels pProjectVersionARN_ pImage_ =
  DetectCustomLabels'
    { _dclMinConfidence = Nothing,
      _dclMaxResults = Nothing,
      _dclProjectVersionARN = pProjectVersionARN_,
      _dclImage = pImage_
    }

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence lower than this specified value. If you specify a value of 0, all labels are return, regardless of the default thresholds that the model version applies.
dclMinConfidence :: Lens' DetectCustomLabels (Maybe Double)
dclMinConfidence = lens _dclMinConfidence (\s a -> s {_dclMinConfidence = a})

-- | Maximum number of results you want the service to return in the response. The service returns the specified number of highest confidence labels ranked from highest confidence to lowest.
dclMaxResults :: Lens' DetectCustomLabels (Maybe Natural)
dclMaxResults = lens _dclMaxResults (\s a -> s {_dclMaxResults = a}) . mapping _Nat

-- | The ARN of the model version that you want to use.
dclProjectVersionARN :: Lens' DetectCustomLabels Text
dclProjectVersionARN = lens _dclProjectVersionARN (\s a -> s {_dclProjectVersionARN = a})

-- | Undocumented member.
dclImage :: Lens' DetectCustomLabels Image
dclImage = lens _dclImage (\s a -> s {_dclImage = a})

instance AWSRequest DetectCustomLabels where
  type Rs DetectCustomLabels = DetectCustomLabelsResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          DetectCustomLabelsResponse'
            <$> (x .?> "CustomLabels" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DetectCustomLabels

instance NFData DetectCustomLabels

instance ToHeaders DetectCustomLabels where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.DetectCustomLabels" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DetectCustomLabels where
  toJSON DetectCustomLabels' {..} =
    object
      ( catMaybes
          [ ("MinConfidence" .=) <$> _dclMinConfidence,
            ("MaxResults" .=) <$> _dclMaxResults,
            Just ("ProjectVersionArn" .= _dclProjectVersionARN),
            Just ("Image" .= _dclImage)
          ]
      )

instance ToPath DetectCustomLabels where
  toPath = const "/"

instance ToQuery DetectCustomLabels where
  toQuery = const mempty

-- | /See:/ 'detectCustomLabelsResponse' smart constructor.
data DetectCustomLabelsResponse = DetectCustomLabelsResponse'
  { _dclrsCustomLabels ::
      !(Maybe [CustomLabel]),
    _dclrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectCustomLabelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dclrsCustomLabels' - An array of custom labels detected in the input image.
--
-- * 'dclrsResponseStatus' - -- | The response status code.
detectCustomLabelsResponse ::
  -- | 'dclrsResponseStatus'
  Int ->
  DetectCustomLabelsResponse
detectCustomLabelsResponse pResponseStatus_ =
  DetectCustomLabelsResponse'
    { _dclrsCustomLabels = Nothing,
      _dclrsResponseStatus = pResponseStatus_
    }

-- | An array of custom labels detected in the input image.
dclrsCustomLabels :: Lens' DetectCustomLabelsResponse [CustomLabel]
dclrsCustomLabels = lens _dclrsCustomLabels (\s a -> s {_dclrsCustomLabels = a}) . _Default . _Coerce

-- | -- | The response status code.
dclrsResponseStatus :: Lens' DetectCustomLabelsResponse Int
dclrsResponseStatus = lens _dclrsResponseStatus (\s a -> s {_dclrsResponseStatus = a})

instance NFData DetectCustomLabelsResponse
