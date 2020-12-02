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
-- Module      : Network.AWS.Rekognition.DeleteFaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes faces from a collection. You specify a collection ID and an array of face IDs to remove from the collection.
--
--
-- This operation requires permissions to perform the @rekognition:DeleteFaces@ action.
--
module Network.AWS.Rekognition.DeleteFaces
    (
    -- * Creating a Request
      deleteFaces
    , DeleteFaces
    -- * Request Lenses
    , dfCollectionId
    , dfFaceIds

    -- * Destructuring the Response
    , deleteFacesResponse
    , DeleteFacesResponse
    -- * Response Lenses
    , dfsrsDeletedFaces
    , dfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFaces' smart constructor.
data DeleteFaces = DeleteFaces'
  { _dfCollectionId :: !Text
  , _dfFaceIds      :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfCollectionId' - Collection from which to remove the specific faces.
--
-- * 'dfFaceIds' - An array of face IDs to delete.
deleteFaces
    :: Text -- ^ 'dfCollectionId'
    -> NonEmpty Text -- ^ 'dfFaceIds'
    -> DeleteFaces
deleteFaces pCollectionId_ pFaceIds_ =
  DeleteFaces'
    {_dfCollectionId = pCollectionId_, _dfFaceIds = _List1 # pFaceIds_}


-- | Collection from which to remove the specific faces.
dfCollectionId :: Lens' DeleteFaces Text
dfCollectionId = lens _dfCollectionId (\ s a -> s{_dfCollectionId = a})

-- | An array of face IDs to delete.
dfFaceIds :: Lens' DeleteFaces (NonEmpty Text)
dfFaceIds = lens _dfFaceIds (\ s a -> s{_dfFaceIds = a}) . _List1

instance AWSRequest DeleteFaces where
        type Rs DeleteFaces = DeleteFacesResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 DeleteFacesResponse' <$>
                   (x .?> "DeletedFaces") <*> (pure (fromEnum s)))

instance Hashable DeleteFaces where

instance NFData DeleteFaces where

instance ToHeaders DeleteFaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.DeleteFaces" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteFaces where
        toJSON DeleteFaces'{..}
          = object
              (catMaybes
                 [Just ("CollectionId" .= _dfCollectionId),
                  Just ("FaceIds" .= _dfFaceIds)])

instance ToPath DeleteFaces where
        toPath = const "/"

instance ToQuery DeleteFaces where
        toQuery = const mempty

-- | /See:/ 'deleteFacesResponse' smart constructor.
data DeleteFacesResponse = DeleteFacesResponse'
  { _dfsrsDeletedFaces   :: !(Maybe (List1 Text))
  , _dfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrsDeletedFaces' - An array of strings (face IDs) of the faces that were deleted.
--
-- * 'dfsrsResponseStatus' - -- | The response status code.
deleteFacesResponse
    :: Int -- ^ 'dfsrsResponseStatus'
    -> DeleteFacesResponse
deleteFacesResponse pResponseStatus_ =
  DeleteFacesResponse'
    {_dfsrsDeletedFaces = Nothing, _dfsrsResponseStatus = pResponseStatus_}


-- | An array of strings (face IDs) of the faces that were deleted.
dfsrsDeletedFaces :: Lens' DeleteFacesResponse (Maybe (NonEmpty Text))
dfsrsDeletedFaces = lens _dfsrsDeletedFaces (\ s a -> s{_dfsrsDeletedFaces = a}) . mapping _List1

-- | -- | The response status code.
dfsrsResponseStatus :: Lens' DeleteFacesResponse Int
dfsrsResponseStatus = lens _dfsrsResponseStatus (\ s a -> s{_dfsrsResponseStatus = a})

instance NFData DeleteFacesResponse where
