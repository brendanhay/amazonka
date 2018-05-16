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
-- Module      : Network.AWS.Rekognition.IndexFaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects faces in the input image and adds them to the specified collection.
--
--
-- Amazon Rekognition does not save the actual faces detected. Instead, the underlying detection algorithm first detects the faces in the input image, and for each face extracts facial features into a feature vector, and stores it in the back-end database. Amazon Rekognition uses feature vectors when performing face match and search operations using the and operations.
--
-- If you are using version 1.0 of the face detection model, @IndexFaces@ indexes the 15 largest faces in the input image. Later versions of the face detection model index the 100 largest faces in the input image. To determine which version of the model you are using, check the the value of @FaceModelVersion@ in the response from @IndexFaces@ . For more information, see 'face-detection-model' .
--
-- If you provide the optional @ExternalImageID@ for the input image you provided, Amazon Rekognition associates this ID with all faces that it detects. When you call the operation, the response returns the external ID. You can use this external image ID to create a client-side index to associate the faces with each image. You can then use the index to find all faces in an image.
--
-- In response, the operation returns an array of metadata for all detected faces. This includes, the bounding box of the detected face, confidence value (indicating the bounding box contains a face), a face ID assigned by the service for each face that is detected and stored, and an image ID assigned by the service for the input image. If you request all facial attributes (using the @detectionAttributes@ parameter, Amazon Rekognition returns detailed facial attributes such as facial landmarks (for example, location of eye and mount) and other facial attributes such gender. If you provide the same image, specify the same collection, and use the same external ID in the @IndexFaces@ operation, Amazon Rekognition doesn't save duplicate face metadata.
--
-- The input image is passed either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
--
-- This operation requires permissions to perform the @rekognition:IndexFaces@ action.
--
module Network.AWS.Rekognition.IndexFaces
    (
    -- * Creating a Request
      indexFaces
    , IndexFaces
    -- * Request Lenses
    , ifExternalImageId
    , ifDetectionAttributes
    , ifCollectionId
    , ifImage

    -- * Destructuring the Response
    , indexFacesResponse
    , IndexFacesResponse
    -- * Response Lenses
    , ifrsFaceModelVersion
    , ifrsFaceRecords
    , ifrsOrientationCorrection
    , ifrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'indexFaces' smart constructor.
data IndexFaces = IndexFaces'
  { _ifExternalImageId     :: !(Maybe Text)
  , _ifDetectionAttributes :: !(Maybe [Attribute])
  , _ifCollectionId        :: !Text
  , _ifImage               :: !Image
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IndexFaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifExternalImageId' - ID you want to assign to all the faces detected in the image.
--
-- * 'ifDetectionAttributes' - An array of facial attributes that you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned but the operation will take longer to complete. If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
--
-- * 'ifCollectionId' - The ID of an existing collection to which you want to add the faces that are detected in the input images.
--
-- * 'ifImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
indexFaces
    :: Text -- ^ 'ifCollectionId'
    -> Image -- ^ 'ifImage'
    -> IndexFaces
indexFaces pCollectionId_ pImage_ =
  IndexFaces'
    { _ifExternalImageId = Nothing
    , _ifDetectionAttributes = Nothing
    , _ifCollectionId = pCollectionId_
    , _ifImage = pImage_
    }


-- | ID you want to assign to all the faces detected in the image.
ifExternalImageId :: Lens' IndexFaces (Maybe Text)
ifExternalImageId = lens _ifExternalImageId (\ s a -> s{_ifExternalImageId = a})

-- | An array of facial attributes that you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned but the operation will take longer to complete. If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
ifDetectionAttributes :: Lens' IndexFaces [Attribute]
ifDetectionAttributes = lens _ifDetectionAttributes (\ s a -> s{_ifDetectionAttributes = a}) . _Default . _Coerce

-- | The ID of an existing collection to which you want to add the faces that are detected in the input images.
ifCollectionId :: Lens' IndexFaces Text
ifCollectionId = lens _ifCollectionId (\ s a -> s{_ifCollectionId = a})

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
ifImage :: Lens' IndexFaces Image
ifImage = lens _ifImage (\ s a -> s{_ifImage = a})

instance AWSRequest IndexFaces where
        type Rs IndexFaces = IndexFacesResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 IndexFacesResponse' <$>
                   (x .?> "FaceModelVersion") <*>
                     (x .?> "FaceRecords" .!@ mempty)
                     <*> (x .?> "OrientationCorrection")
                     <*> (pure (fromEnum s)))

instance Hashable IndexFaces where

instance NFData IndexFaces where

instance ToHeaders IndexFaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.IndexFaces" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON IndexFaces where
        toJSON IndexFaces'{..}
          = object
              (catMaybes
                 [("ExternalImageId" .=) <$> _ifExternalImageId,
                  ("DetectionAttributes" .=) <$>
                    _ifDetectionAttributes,
                  Just ("CollectionId" .= _ifCollectionId),
                  Just ("Image" .= _ifImage)])

instance ToPath IndexFaces where
        toPath = const "/"

instance ToQuery IndexFaces where
        toQuery = const mempty

-- | /See:/ 'indexFacesResponse' smart constructor.
data IndexFacesResponse = IndexFacesResponse'
  { _ifrsFaceModelVersion      :: !(Maybe Text)
  , _ifrsFaceRecords           :: !(Maybe [FaceRecord])
  , _ifrsOrientationCorrection :: !(Maybe OrientationCorrection)
  , _ifrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IndexFacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifrsFaceModelVersion' - Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- * 'ifrsFaceRecords' - An array of faces detected and added to the collection. For more information, see 'collections-index-faces' .
--
-- * 'ifrsOrientationCorrection' - The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct image orientation. The bounding box coordinates returned in @FaceRecords@ represent face locations before the image orientation is corrected.
--
-- * 'ifrsResponseStatus' - -- | The response status code.
indexFacesResponse
    :: Int -- ^ 'ifrsResponseStatus'
    -> IndexFacesResponse
indexFacesResponse pResponseStatus_ =
  IndexFacesResponse'
    { _ifrsFaceModelVersion = Nothing
    , _ifrsFaceRecords = Nothing
    , _ifrsOrientationCorrection = Nothing
    , _ifrsResponseStatus = pResponseStatus_
    }


-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
ifrsFaceModelVersion :: Lens' IndexFacesResponse (Maybe Text)
ifrsFaceModelVersion = lens _ifrsFaceModelVersion (\ s a -> s{_ifrsFaceModelVersion = a})

-- | An array of faces detected and added to the collection. For more information, see 'collections-index-faces' .
ifrsFaceRecords :: Lens' IndexFacesResponse [FaceRecord]
ifrsFaceRecords = lens _ifrsFaceRecords (\ s a -> s{_ifrsFaceRecords = a}) . _Default . _Coerce

-- | The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct image orientation. The bounding box coordinates returned in @FaceRecords@ represent face locations before the image orientation is corrected.
ifrsOrientationCorrection :: Lens' IndexFacesResponse (Maybe OrientationCorrection)
ifrsOrientationCorrection = lens _ifrsOrientationCorrection (\ s a -> s{_ifrsOrientationCorrection = a})

-- | -- | The response status code.
ifrsResponseStatus :: Lens' IndexFacesResponse Int
ifrsResponseStatus = lens _ifrsResponseStatus (\ s a -> s{_ifrsResponseStatus = a})

instance NFData IndexFacesResponse where
