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
-- Module      : Network.AWS.Rekognition.SearchFacesByImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given input image, first detects the largest face in the image, and then searches the specified collection for matching faces. The operation compares the features of the input face with faces in the specified collection.
--
--
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
--
-- The response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match found. Along with the metadata, the response also includes a @similarity@ indicating how similar the face is to the input face. In the response, the operation also returns the bounding box (and a confidence level that the bounding box contains a face) of the face that Amazon Rekognition used for the input image.
--
-- For an example, see 'search-face-with-image-procedure' .
--
-- This operation requires permissions to perform the @rekognition:SearchFacesByImage@ action.
--
module Network.AWS.Rekognition.SearchFacesByImage
    (
    -- * Creating a Request
      searchFacesByImage
    , SearchFacesByImage
    -- * Request Lenses
    , sfbiFaceMatchThreshold
    , sfbiMaxFaces
    , sfbiCollectionId
    , sfbiImage

    -- * Destructuring the Response
    , searchFacesByImageResponse
    , SearchFacesByImageResponse
    -- * Response Lenses
    , sfbirsFaceMatches
    , sfbirsFaceModelVersion
    , sfbirsSearchedFaceBoundingBox
    , sfbirsSearchedFaceConfidence
    , sfbirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchFacesByImage' smart constructor.
data SearchFacesByImage = SearchFacesByImage'
  { _sfbiFaceMatchThreshold :: !(Maybe Double)
  , _sfbiMaxFaces           :: !(Maybe Nat)
  , _sfbiCollectionId       :: !Text
  , _sfbiImage              :: !Image
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchFacesByImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfbiFaceMatchThreshold' - (Optional) Specifies the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%.
--
-- * 'sfbiMaxFaces' - Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
--
-- * 'sfbiCollectionId' - ID of the collection to search.
--
-- * 'sfbiImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
searchFacesByImage
    :: Text -- ^ 'sfbiCollectionId'
    -> Image -- ^ 'sfbiImage'
    -> SearchFacesByImage
searchFacesByImage pCollectionId_ pImage_ =
  SearchFacesByImage'
    { _sfbiFaceMatchThreshold = Nothing
    , _sfbiMaxFaces = Nothing
    , _sfbiCollectionId = pCollectionId_
    , _sfbiImage = pImage_
    }


-- | (Optional) Specifies the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%.
sfbiFaceMatchThreshold :: Lens' SearchFacesByImage (Maybe Double)
sfbiFaceMatchThreshold = lens _sfbiFaceMatchThreshold (\ s a -> s{_sfbiFaceMatchThreshold = a})

-- | Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
sfbiMaxFaces :: Lens' SearchFacesByImage (Maybe Natural)
sfbiMaxFaces = lens _sfbiMaxFaces (\ s a -> s{_sfbiMaxFaces = a}) . mapping _Nat

-- | ID of the collection to search.
sfbiCollectionId :: Lens' SearchFacesByImage Text
sfbiCollectionId = lens _sfbiCollectionId (\ s a -> s{_sfbiCollectionId = a})

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
sfbiImage :: Lens' SearchFacesByImage Image
sfbiImage = lens _sfbiImage (\ s a -> s{_sfbiImage = a})

instance AWSRequest SearchFacesByImage where
        type Rs SearchFacesByImage =
             SearchFacesByImageResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 SearchFacesByImageResponse' <$>
                   (x .?> "FaceMatches" .!@ mempty) <*>
                     (x .?> "FaceModelVersion")
                     <*> (x .?> "SearchedFaceBoundingBox")
                     <*> (x .?> "SearchedFaceConfidence")
                     <*> (pure (fromEnum s)))

instance Hashable SearchFacesByImage where

instance NFData SearchFacesByImage where

instance ToHeaders SearchFacesByImage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.SearchFacesByImage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchFacesByImage where
        toJSON SearchFacesByImage'{..}
          = object
              (catMaybes
                 [("FaceMatchThreshold" .=) <$>
                    _sfbiFaceMatchThreshold,
                  ("MaxFaces" .=) <$> _sfbiMaxFaces,
                  Just ("CollectionId" .= _sfbiCollectionId),
                  Just ("Image" .= _sfbiImage)])

instance ToPath SearchFacesByImage where
        toPath = const "/"

instance ToQuery SearchFacesByImage where
        toQuery = const mempty

-- | /See:/ 'searchFacesByImageResponse' smart constructor.
data SearchFacesByImageResponse = SearchFacesByImageResponse'
  { _sfbirsFaceMatches             :: !(Maybe [FaceMatch])
  , _sfbirsFaceModelVersion        :: !(Maybe Text)
  , _sfbirsSearchedFaceBoundingBox :: !(Maybe BoundingBox)
  , _sfbirsSearchedFaceConfidence  :: !(Maybe Double)
  , _sfbirsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchFacesByImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfbirsFaceMatches' - An array of faces that match the input face, along with the confidence in the match.
--
-- * 'sfbirsFaceModelVersion' - Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- * 'sfbirsSearchedFaceBoundingBox' - The bounding box around the face in the input image that Amazon Rekognition used for the search.
--
-- * 'sfbirsSearchedFaceConfidence' - The level of confidence that the @searchedFaceBoundingBox@ , contains a face.
--
-- * 'sfbirsResponseStatus' - -- | The response status code.
searchFacesByImageResponse
    :: Int -- ^ 'sfbirsResponseStatus'
    -> SearchFacesByImageResponse
searchFacesByImageResponse pResponseStatus_ =
  SearchFacesByImageResponse'
    { _sfbirsFaceMatches = Nothing
    , _sfbirsFaceModelVersion = Nothing
    , _sfbirsSearchedFaceBoundingBox = Nothing
    , _sfbirsSearchedFaceConfidence = Nothing
    , _sfbirsResponseStatus = pResponseStatus_
    }


-- | An array of faces that match the input face, along with the confidence in the match.
sfbirsFaceMatches :: Lens' SearchFacesByImageResponse [FaceMatch]
sfbirsFaceMatches = lens _sfbirsFaceMatches (\ s a -> s{_sfbirsFaceMatches = a}) . _Default . _Coerce

-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
sfbirsFaceModelVersion :: Lens' SearchFacesByImageResponse (Maybe Text)
sfbirsFaceModelVersion = lens _sfbirsFaceModelVersion (\ s a -> s{_sfbirsFaceModelVersion = a})

-- | The bounding box around the face in the input image that Amazon Rekognition used for the search.
sfbirsSearchedFaceBoundingBox :: Lens' SearchFacesByImageResponse (Maybe BoundingBox)
sfbirsSearchedFaceBoundingBox = lens _sfbirsSearchedFaceBoundingBox (\ s a -> s{_sfbirsSearchedFaceBoundingBox = a})

-- | The level of confidence that the @searchedFaceBoundingBox@ , contains a face.
sfbirsSearchedFaceConfidence :: Lens' SearchFacesByImageResponse (Maybe Double)
sfbirsSearchedFaceConfidence = lens _sfbirsSearchedFaceConfidence (\ s a -> s{_sfbirsSearchedFaceConfidence = a})

-- | -- | The response status code.
sfbirsResponseStatus :: Lens' SearchFacesByImageResponse Int
sfbirsResponseStatus = lens _sfbirsResponseStatus (\ s a -> s{_sfbirsResponseStatus = a})

instance NFData SearchFacesByImageResponse where
