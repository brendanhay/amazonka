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
-- Module      : Network.AWS.Rekognition.IndexFaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects faces in the input image and adds them to the specified collection.
--
--
-- Amazon Rekognition doesn't save the actual faces that are detected. Instead, the underlying detection algorithm first detects the faces in the input image. For each face, the algorithm extracts facial features into a feature vector, and stores it in the backend database. Amazon Rekognition uses feature vectors when it performs face match and search operations using the 'SearchFaces' and 'SearchFacesByImage' operations.
--
-- For more information, see Adding Faces to a Collection in the Amazon Rekognition Developer Guide.
--
-- To get the number of faces in a collection, call 'DescribeCollection' .
--
-- If you're using version 1.0 of the face detection model, @IndexFaces@ indexes the 15 largest faces in the input image. Later versions of the face detection model index the 100 largest faces in the input image.
--
-- If you're using version 4 or later of the face model, image orientation information is not returned in the @OrientationCorrection@ field.
--
-- To determine which version of the model you're using, call 'DescribeCollection' and supply the collection ID. You can also get the model version from the value of @FaceModelVersion@ in the response from @IndexFaces@
--
-- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
--
-- If you provide the optional @ExternalImageId@ for the input image you provided, Amazon Rekognition associates this ID with all faces that it detects. When you call the 'ListFaces' operation, the response returns the external ID. You can use this external image ID to create a client-side index to associate the faces with each image. You can then use the index to find all faces in an image.
--
-- You can specify the maximum number of faces to index with the @MaxFaces@ input parameter. This is useful when you want to index the largest faces in an image and don't want to index smaller faces, such as those belonging to people standing in the background.
--
-- The @QualityFilter@ input parameter allows you to filter out detected faces that don’t meet a required quality bar. The quality bar is based on a variety of common use cases. By default, @IndexFaces@ chooses the quality bar that's used to filter faces. You can also explicitly choose the quality bar. Use @QualityFilter@ , to set the quality bar by specifying @LOW@ , @MEDIUM@ , or @HIGH@ . If you do not want to filter detected faces, specify @NONE@ .
--
-- Information about faces detected in an image, but not indexed, is returned in an array of 'UnindexedFace' objects, @UnindexedFaces@ . Faces aren't indexed for reasons such as:
--
--     * The number of faces detected exceeds the value of the @MaxFaces@ request parameter.
--
--     * The face is too small compared to the image dimensions.
--
--     * The face is too blurry.
--
--     * The image is too dark.
--
--     * The face has an extreme pose.
--
--     * The face doesn’t have enough detail to be suitable for face search.
--
--
--
-- In response, the @IndexFaces@ operation returns an array of metadata for all detected faces, @FaceRecords@ . This includes:
--
--     * The bounding box, @BoundingBox@ , of the detected face.
--
--     * A confidence value, @Confidence@ , which indicates the confidence that the bounding box contains a face.
--
--     * A face ID, @FaceId@ , assigned by the service for each face that's detected and stored.
--
--     * An image ID, @ImageId@ , assigned by the service for the input image.
--
--
--
-- If you request all facial attributes (by using the @detectionAttributes@ parameter), Amazon Rekognition returns detailed facial attributes, such as facial landmarks (for example, location of eye and mouth) and other facial attributes. If you provide the same image, specify the same collection, and use the same external ID in the @IndexFaces@ operation, Amazon Rekognition doesn't save duplicate face metadata.
--
--
--
-- The input image is passed either as base64-encoded image bytes, or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes isn't supported. The image must be formatted as a PNG or JPEG file.
--
-- This operation requires permissions to perform the @rekognition:IndexFaces@ action.
module Network.AWS.Rekognition.IndexFaces
  ( -- * Creating a Request
    indexFaces,
    IndexFaces,

    -- * Request Lenses
    ifExternalImageId,
    ifQualityFilter,
    ifMaxFaces,
    ifDetectionAttributes,
    ifCollectionId,
    ifImage,

    -- * Destructuring the Response
    indexFacesResponse,
    IndexFacesResponse,

    -- * Response Lenses
    ifrsFaceModelVersion,
    ifrsFaceRecords,
    ifrsOrientationCorrection,
    ifrsUnindexedFaces,
    ifrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'indexFaces' smart constructor.
data IndexFaces = IndexFaces'
  { _ifExternalImageId :: !(Maybe Text),
    _ifQualityFilter :: !(Maybe QualityFilter),
    _ifMaxFaces :: !(Maybe Nat),
    _ifDetectionAttributes :: !(Maybe [Attribute]),
    _ifCollectionId :: !Text,
    _ifImage :: !Image
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IndexFaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifExternalImageId' - The ID you want to assign to all the faces detected in the image.
--
-- * 'ifQualityFilter' - A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't indexed. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The default value is @AUTO@ . The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed.  To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
--
-- * 'ifMaxFaces' - The maximum number of faces to index. The value of @MaxFaces@ must be greater than or equal to 1. @IndexFaces@ returns no more than 100 detected faces in an image, even if you specify a larger value for @MaxFaces@ . If @IndexFaces@ detects more faces than the value of @MaxFaces@ , the faces with the lowest quality are filtered out first. If there are still more faces than the value of @MaxFaces@ , the faces with the smallest bounding boxes are filtered out (up to the number that's needed to satisfy the value of @MaxFaces@ ). Information about the unindexed faces is available in the @UnindexedFaces@ array.  The faces that are returned by @IndexFaces@ are sorted by the largest face bounding box size to the smallest size, in descending order. @MaxFaces@ can be used with a collection associated with any version of the face model.
--
-- * 'ifDetectionAttributes' - An array of facial attributes that you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ , and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned, but the operation takes longer to complete. If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
--
-- * 'ifCollectionId' - The ID of an existing collection to which you want to add the faces that are detected in the input images.
--
-- * 'ifImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes isn't supported.  If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
indexFaces ::
  -- | 'ifCollectionId'
  Text ->
  -- | 'ifImage'
  Image ->
  IndexFaces
indexFaces pCollectionId_ pImage_ =
  IndexFaces'
    { _ifExternalImageId = Nothing,
      _ifQualityFilter = Nothing,
      _ifMaxFaces = Nothing,
      _ifDetectionAttributes = Nothing,
      _ifCollectionId = pCollectionId_,
      _ifImage = pImage_
    }

-- | The ID you want to assign to all the faces detected in the image.
ifExternalImageId :: Lens' IndexFaces (Maybe Text)
ifExternalImageId = lens _ifExternalImageId (\s a -> s {_ifExternalImageId = a})

-- | A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't indexed. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The default value is @AUTO@ . The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed.  To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
ifQualityFilter :: Lens' IndexFaces (Maybe QualityFilter)
ifQualityFilter = lens _ifQualityFilter (\s a -> s {_ifQualityFilter = a})

-- | The maximum number of faces to index. The value of @MaxFaces@ must be greater than or equal to 1. @IndexFaces@ returns no more than 100 detected faces in an image, even if you specify a larger value for @MaxFaces@ . If @IndexFaces@ detects more faces than the value of @MaxFaces@ , the faces with the lowest quality are filtered out first. If there are still more faces than the value of @MaxFaces@ , the faces with the smallest bounding boxes are filtered out (up to the number that's needed to satisfy the value of @MaxFaces@ ). Information about the unindexed faces is available in the @UnindexedFaces@ array.  The faces that are returned by @IndexFaces@ are sorted by the largest face bounding box size to the smallest size, in descending order. @MaxFaces@ can be used with a collection associated with any version of the face model.
ifMaxFaces :: Lens' IndexFaces (Maybe Natural)
ifMaxFaces = lens _ifMaxFaces (\s a -> s {_ifMaxFaces = a}) . mapping _Nat

-- | An array of facial attributes that you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ , and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned, but the operation takes longer to complete. If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
ifDetectionAttributes :: Lens' IndexFaces [Attribute]
ifDetectionAttributes = lens _ifDetectionAttributes (\s a -> s {_ifDetectionAttributes = a}) . _Default . _Coerce

-- | The ID of an existing collection to which you want to add the faces that are detected in the input images.
ifCollectionId :: Lens' IndexFaces Text
ifCollectionId = lens _ifCollectionId (\s a -> s {_ifCollectionId = a})

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes isn't supported.  If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
ifImage :: Lens' IndexFaces Image
ifImage = lens _ifImage (\s a -> s {_ifImage = a})

instance AWSRequest IndexFaces where
  type Rs IndexFaces = IndexFacesResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          IndexFacesResponse'
            <$> (x .?> "FaceModelVersion")
            <*> (x .?> "FaceRecords" .!@ mempty)
            <*> (x .?> "OrientationCorrection")
            <*> (x .?> "UnindexedFaces" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable IndexFaces

instance NFData IndexFaces

instance ToHeaders IndexFaces where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("RekognitionService.IndexFaces" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON IndexFaces where
  toJSON IndexFaces' {..} =
    object
      ( catMaybes
          [ ("ExternalImageId" .=) <$> _ifExternalImageId,
            ("QualityFilter" .=) <$> _ifQualityFilter,
            ("MaxFaces" .=) <$> _ifMaxFaces,
            ("DetectionAttributes" .=) <$> _ifDetectionAttributes,
            Just ("CollectionId" .= _ifCollectionId),
            Just ("Image" .= _ifImage)
          ]
      )

instance ToPath IndexFaces where
  toPath = const "/"

instance ToQuery IndexFaces where
  toQuery = const mempty

-- | /See:/ 'indexFacesResponse' smart constructor.
data IndexFacesResponse = IndexFacesResponse'
  { _ifrsFaceModelVersion ::
      !(Maybe Text),
    _ifrsFaceRecords :: !(Maybe [FaceRecord]),
    _ifrsOrientationCorrection ::
      !(Maybe OrientationCorrection),
    _ifrsUnindexedFaces :: !(Maybe [UnindexedFace]),
    _ifrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IndexFacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifrsFaceModelVersion' - The version number of the face detection model that's associated with the input collection (@CollectionId@ ).
--
-- * 'ifrsFaceRecords' - An array of faces detected and added to the collection. For more information, see Searching Faces in a Collection in the Amazon Rekognition Developer Guide.
--
-- * 'ifrsOrientationCorrection' - If your collection is associated with a face detection model that's later than version 3.0, the value of @OrientationCorrection@ is always null and no orientation information is returned. If your collection is associated with a face detection model that's version 3.0 or earlier, the following applies:     * If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction - the bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata. The value of @OrientationCorrection@ is null.     * If the image doesn't contain orientation information in its Exif metadata, Amazon Rekognition returns an estimated orientation (ROTATE_0, ROTATE_90, ROTATE_180, ROTATE_270). Amazon Rekognition doesn’t perform image correction for images. The bounding box coordinates aren't translated and represent the object locations before the image is rotated. Bounding box information is returned in the @FaceRecords@ array. You can get the version of the face detection model by calling 'DescribeCollection' .
--
-- * 'ifrsUnindexedFaces' - An array of faces that were detected in the image but weren't indexed. They weren't indexed because the quality filter identified them as low quality, or the @MaxFaces@ request parameter filtered them out. To use the quality filter, you specify the @QualityFilter@ request parameter.
--
-- * 'ifrsResponseStatus' - -- | The response status code.
indexFacesResponse ::
  -- | 'ifrsResponseStatus'
  Int ->
  IndexFacesResponse
indexFacesResponse pResponseStatus_ =
  IndexFacesResponse'
    { _ifrsFaceModelVersion = Nothing,
      _ifrsFaceRecords = Nothing,
      _ifrsOrientationCorrection = Nothing,
      _ifrsUnindexedFaces = Nothing,
      _ifrsResponseStatus = pResponseStatus_
    }

-- | The version number of the face detection model that's associated with the input collection (@CollectionId@ ).
ifrsFaceModelVersion :: Lens' IndexFacesResponse (Maybe Text)
ifrsFaceModelVersion = lens _ifrsFaceModelVersion (\s a -> s {_ifrsFaceModelVersion = a})

-- | An array of faces detected and added to the collection. For more information, see Searching Faces in a Collection in the Amazon Rekognition Developer Guide.
ifrsFaceRecords :: Lens' IndexFacesResponse [FaceRecord]
ifrsFaceRecords = lens _ifrsFaceRecords (\s a -> s {_ifrsFaceRecords = a}) . _Default . _Coerce

-- | If your collection is associated with a face detection model that's later than version 3.0, the value of @OrientationCorrection@ is always null and no orientation information is returned. If your collection is associated with a face detection model that's version 3.0 or earlier, the following applies:     * If the input image is in .jpeg format, it might contain exchangeable image file format (Exif) metadata that includes the image's orientation. Amazon Rekognition uses this orientation information to perform image correction - the bounding box coordinates are translated to represent object locations after the orientation information in the Exif metadata is used to correct the image orientation. Images in .png format don't contain Exif metadata. The value of @OrientationCorrection@ is null.     * If the image doesn't contain orientation information in its Exif metadata, Amazon Rekognition returns an estimated orientation (ROTATE_0, ROTATE_90, ROTATE_180, ROTATE_270). Amazon Rekognition doesn’t perform image correction for images. The bounding box coordinates aren't translated and represent the object locations before the image is rotated. Bounding box information is returned in the @FaceRecords@ array. You can get the version of the face detection model by calling 'DescribeCollection' .
ifrsOrientationCorrection :: Lens' IndexFacesResponse (Maybe OrientationCorrection)
ifrsOrientationCorrection = lens _ifrsOrientationCorrection (\s a -> s {_ifrsOrientationCorrection = a})

-- | An array of faces that were detected in the image but weren't indexed. They weren't indexed because the quality filter identified them as low quality, or the @MaxFaces@ request parameter filtered them out. To use the quality filter, you specify the @QualityFilter@ request parameter.
ifrsUnindexedFaces :: Lens' IndexFacesResponse [UnindexedFace]
ifrsUnindexedFaces = lens _ifrsUnindexedFaces (\s a -> s {_ifrsUnindexedFaces = a}) . _Default . _Coerce

-- | -- | The response status code.
ifrsResponseStatus :: Lens' IndexFacesResponse Int
ifrsResponseStatus = lens _ifrsResponseStatus (\s a -> s {_ifrsResponseStatus = a})

instance NFData IndexFacesResponse
