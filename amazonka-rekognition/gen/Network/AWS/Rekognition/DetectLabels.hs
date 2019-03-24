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
-- Module      : Network.AWS.Rekognition.DetectLabels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects instances of real-world entities within an image (JPEG or PNG) provided as input. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; and concepts like landscape, evening, and nature.
--
--
-- For an example, see Analyzing Images Stored in an Amazon S3 Bucket in the Amazon Rekognition Developer Guide.
--
-- You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
--
-- For each object, scene, and concept the API returns one or more labels. Each label provides the object name, and the level of confidence that the image contains the object. For example, suppose the input image has a lighthouse, the sea, and a rock. The response includes all three labels, one for each object.
--
-- @{Name: lighthouse, Confidence: 98.4629}@
--
-- @{Name: rock,Confidence: 79.2097}@
--
-- @{Name: sea,Confidence: 75.061}@
--
-- In the preceding example, the operation returns one label for each of the three objects. The operation can also return multiple labels for the same object in the image. For example, if the input image shows a flower (for example, a tulip), the operation might return the following three labels.
--
-- @{Name: flower,Confidence: 99.0562}@
--
-- @{Name: plant,Confidence: 99.0562}@
--
-- @{Name: tulip,Confidence: 99.0562}@
--
-- In this example, the detection algorithm more precisely identifies the flower as a tulip.
--
-- In response, the API returns an array of labels. In addition, the response also includes the orientation correction. Optionally, you can specify @MinConfidence@ to control the confidence threshold for the labels returned. The default is 55%. You can also add the @MaxLabels@ parameter to limit the number of labels returned.
--
-- @DetectLabels@ returns bounding boxes for instances of common object labels in an array of 'Instance' objects. An @Instance@ object contains a 'BoundingBox' object, for the location of the label on the image. It also includes the confidence by which the bounding box was detected.
--
-- @DetectLabels@ also returns a hierarchical taxonomy of detected labels. For example, a detected car might be assigned the label /car/ . The label /car/ has two parent labels: /Vehicle/ (its parent) and /Transportation/ (its grandparent). The response returns the entire list of ancestors for a label. Each ancestor is a unique label in the response. In the previous example, /Car/ , /Vehicle/ , and /Transportation/ are returned as unique labels in the response.
--
-- This is a stateless API operation. That is, the operation does not persist any data.
--
-- This operation requires permissions to perform the @rekognition:DetectLabels@ action.
--
module Network.AWS.Rekognition.DetectLabels
    (
    -- * Creating a Request
      detectLabels
    , DetectLabels
    -- * Request Lenses
    , dlMinConfidence
    , dlMaxLabels
    , dlImage

    -- * Destructuring the Response
    , detectLabelsResponse
    , DetectLabelsResponse
    -- * Response Lenses
    , dlrsLabels
    , dlrsOrientationCorrection
    , dlrsLabelModelVersion
    , dlrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectLabels' smart constructor.
data DetectLabels = DetectLabels'
  { _dlMinConfidence :: !(Maybe Double)
  , _dlMaxLabels     :: !(Maybe Nat)
  , _dlImage         :: !Image
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlMinConfidence' - Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with confidence lower than this specified value. If @MinConfidence@ is not specified, the operation returns labels with a confidence values greater than or equal to 55 percent.
--
-- * 'dlMaxLabels' - Maximum number of labels you want the service to return in the response. The service returns the specified number of highest confidence labels.
--
-- * 'dlImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. Images stored in an S3 Bucket do not need to be base64-encoded. If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
detectLabels
    :: Image -- ^ 'dlImage'
    -> DetectLabels
detectLabels pImage_ =
  DetectLabels'
    {_dlMinConfidence = Nothing, _dlMaxLabels = Nothing, _dlImage = pImage_}


-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with confidence lower than this specified value. If @MinConfidence@ is not specified, the operation returns labels with a confidence values greater than or equal to 55 percent.
dlMinConfidence :: Lens' DetectLabels (Maybe Double)
dlMinConfidence = lens _dlMinConfidence (\ s a -> s{_dlMinConfidence = a})

-- | Maximum number of labels you want the service to return in the response. The service returns the specified number of highest confidence labels.
dlMaxLabels :: Lens' DetectLabels (Maybe Natural)
dlMaxLabels = lens _dlMaxLabels (\ s a -> s{_dlMaxLabels = a}) . mapping _Nat

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. Images stored in an S3 Bucket do not need to be base64-encoded. If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
dlImage :: Lens' DetectLabels Image
dlImage = lens _dlImage (\ s a -> s{_dlImage = a})

instance AWSRequest DetectLabels where
        type Rs DetectLabels = DetectLabelsResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 DetectLabelsResponse' <$>
                   (x .?> "Labels" .!@ mempty) <*>
                     (x .?> "OrientationCorrection")
                     <*> (x .?> "LabelModelVersion")
                     <*> (pure (fromEnum s)))

instance Hashable DetectLabels where

instance NFData DetectLabels where

instance ToHeaders DetectLabels where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.DetectLabels" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectLabels where
        toJSON DetectLabels'{..}
          = object
              (catMaybes
                 [("MinConfidence" .=) <$> _dlMinConfidence,
                  ("MaxLabels" .=) <$> _dlMaxLabels,
                  Just ("Image" .= _dlImage)])

instance ToPath DetectLabels where
        toPath = const "/"

instance ToQuery DetectLabels where
        toQuery = const mempty

-- | /See:/ 'detectLabelsResponse' smart constructor.
data DetectLabelsResponse = DetectLabelsResponse'
  { _dlrsLabels                :: !(Maybe [Label])
  , _dlrsOrientationCorrection :: !(Maybe OrientationCorrection)
  , _dlrsLabelModelVersion     :: !(Maybe Text)
  , _dlrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectLabelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsLabels' - An array of labels for the real-world objects detected.
--
-- * 'dlrsOrientationCorrection' - The value of @OrientationCorrection@ is always null. If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction. The bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata. Amazon Rekognition doesn
