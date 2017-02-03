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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects faces in the input image and adds them to the specified collection.
--
--
-- Amazon Rekognition does not save the actual faces detected. Instead, the underlying detection algorithm first detects the faces in the input image, and for each face extracts facial features into a feature vector, and stores it in the back-end database. Amazon Rekognition uses feature vectors when performing face match and search operations using the and operations.
--
-- If you provide the optional @externalImageID@ for the input image you provided, Amazon Rekognition associates this ID with all faces that it detects. When you call the operation, the response returns the external ID. You can use this external image ID to create a client-side index to associate the faces with each image. You can then use the index to find all faces in an image.
--
-- In response, the operation returns an array of metadata for all detected faces. This includes, the bounding box of the detected face, confidence value (indicating the bounding box contains a face), a face ID assigned by the service for each face that is detected and stored, and an image ID assigned by the service for the input image If you request all facial attributes (using the @detectionAttributes@ parameter, Amazon Rekognition returns detailed facial attributes such as facial landmarks (for example, location of eye and mount) and other facial attributes such gender. If you provide the same image, specify the same collection, and use the same external ID in the @IndexFaces@ operation, Amazon Rekognition doesn't save duplicate face metadata.
--
-- For an example, see 'example2' .
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
    , ifrsFaceRecords
    , ifrsOrientationCorrection
    , ifrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Rekognition.Types
import           Network.AWS.Rekognition.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'indexFaces' smart constructor.
data IndexFaces = IndexFaces'
    { _ifExternalImageId     :: !(Maybe Text)
    , _ifDetectionAttributes :: !(Maybe [Attribute])
    , _ifCollectionId        :: !Text
    , _ifImage               :: !Image
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IndexFaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifExternalImageId' - ID you want to assign to all the faces detected in the image.
--
-- * 'ifDetectionAttributes' - (Optional) Returns detailed attributes of indexed faces. By default, the operation returns a subset of the facial attributes.  For example, you can specify the value as, ["ALL"] or ["DEFAULT"]. If you provide both, ["ALL", "DEFAULT"], Amazon Rekognition uses the logical AND operator to determine which attributes to return (in this case, it is all attributes). If you specify all attributes, the service performs additional detection, in addition to the default.
--
-- * 'ifCollectionId' - ID of an existing collection to which you want to add the faces that are detected in the input images.
--
-- * 'ifImage' - Undocumented member.
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
ifExternalImageId = lens _ifExternalImageId (\ s a -> s{_ifExternalImageId = a});

-- | (Optional) Returns detailed attributes of indexed faces. By default, the operation returns a subset of the facial attributes.  For example, you can specify the value as, ["ALL"] or ["DEFAULT"]. If you provide both, ["ALL", "DEFAULT"], Amazon Rekognition uses the logical AND operator to determine which attributes to return (in this case, it is all attributes). If you specify all attributes, the service performs additional detection, in addition to the default.
ifDetectionAttributes :: Lens' IndexFaces [Attribute]
ifDetectionAttributes = lens _ifDetectionAttributes (\ s a -> s{_ifDetectionAttributes = a}) . _Default . _Coerce;

-- | ID of an existing collection to which you want to add the faces that are detected in the input images.
ifCollectionId :: Lens' IndexFaces Text
ifCollectionId = lens _ifCollectionId (\ s a -> s{_ifCollectionId = a});

-- | Undocumented member.
ifImage :: Lens' IndexFaces Image
ifImage = lens _ifImage (\ s a -> s{_ifImage = a});

instance AWSRequest IndexFaces where
        type Rs IndexFaces = IndexFacesResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 IndexFacesResponse' <$>
                   (x .?> "FaceRecords" .!@ mempty) <*>
                     (x .?> "OrientationCorrection")
                     <*> (pure (fromEnum s)))

instance Hashable IndexFaces

instance NFData IndexFaces

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
    { _ifrsFaceRecords           :: !(Maybe [FaceRecord])
    , _ifrsOrientationCorrection :: !(Maybe OrientationCorrection)
    , _ifrsResponseStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IndexFacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifrsFaceRecords' - An array of faces detected and added to the collection. For more information, see 'howitworks-index-faces' .
--
-- * 'ifrsOrientationCorrection' - The algorithm detects the image orientation. If it detects that the image was rotated, it returns the degree of rotation. You can use this value to correct the orientation and also appropriately analyze the bounding box coordinates that are returned.
--
-- * 'ifrsResponseStatus' - -- | The response status code.
indexFacesResponse
    :: Int -- ^ 'ifrsResponseStatus'
    -> IndexFacesResponse
indexFacesResponse pResponseStatus_ =
    IndexFacesResponse'
    { _ifrsFaceRecords = Nothing
    , _ifrsOrientationCorrection = Nothing
    , _ifrsResponseStatus = pResponseStatus_
    }

-- | An array of faces detected and added to the collection. For more information, see 'howitworks-index-faces' .
ifrsFaceRecords :: Lens' IndexFacesResponse [FaceRecord]
ifrsFaceRecords = lens _ifrsFaceRecords (\ s a -> s{_ifrsFaceRecords = a}) . _Default . _Coerce;

-- | The algorithm detects the image orientation. If it detects that the image was rotated, it returns the degree of rotation. You can use this value to correct the orientation and also appropriately analyze the bounding box coordinates that are returned.
ifrsOrientationCorrection :: Lens' IndexFacesResponse (Maybe OrientationCorrection)
ifrsOrientationCorrection = lens _ifrsOrientationCorrection (\ s a -> s{_ifrsOrientationCorrection = a});

-- | -- | The response status code.
ifrsResponseStatus :: Lens' IndexFacesResponse Int
ifrsResponseStatus = lens _ifrsResponseStatus (\ s a -> s{_ifrsResponseStatus = a});

instance NFData IndexFacesResponse
