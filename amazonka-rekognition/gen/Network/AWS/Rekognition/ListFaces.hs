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
-- Module      : Network.AWS.Rekognition.ListFaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata for faces in the specified collection. This metadata includes information such as the bounding box coordinates, the confidence (that the bounding box contains a face), and face ID. For an example, see 'list-faces-in-collection-procedure' .
--
--
-- This operation requires permissions to perform the @rekognition:ListFaces@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListFaces
    (
    -- * Creating a Request
      listFaces
    , ListFaces
    -- * Request Lenses
    , lfNextToken
    , lfMaxResults
    , lfCollectionId

    -- * Destructuring the Response
    , listFacesResponse
    , ListFacesResponse
    -- * Response Lenses
    , lfrsFaceModelVersion
    , lfrsNextToken
    , lfrsFaces
    , lfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFaces' smart constructor.
data ListFaces = ListFaces'
  { _lfNextToken    :: !(Maybe Text)
  , _lfMaxResults   :: !(Maybe Nat)
  , _lfCollectionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfNextToken' - If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
--
-- * 'lfMaxResults' - Maximum number of faces to return.
--
-- * 'lfCollectionId' - ID of the collection from which to list the faces.
listFaces
    :: Text -- ^ 'lfCollectionId'
    -> ListFaces
listFaces pCollectionId_ =
  ListFaces'
    { _lfNextToken = Nothing
    , _lfMaxResults = Nothing
    , _lfCollectionId = pCollectionId_
    }


-- | If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
lfNextToken :: Lens' ListFaces (Maybe Text)
lfNextToken = lens _lfNextToken (\ s a -> s{_lfNextToken = a})

-- | Maximum number of faces to return.
lfMaxResults :: Lens' ListFaces (Maybe Natural)
lfMaxResults = lens _lfMaxResults (\ s a -> s{_lfMaxResults = a}) . mapping _Nat

-- | ID of the collection from which to list the faces.
lfCollectionId :: Lens' ListFaces Text
lfCollectionId = lens _lfCollectionId (\ s a -> s{_lfCollectionId = a})

instance AWSPager ListFaces where
        page rq rs
          | stop (rs ^. lfrsNextToken) = Nothing
          | stop (rs ^. lfrsFaces) = Nothing
          | otherwise =
            Just $ rq & lfNextToken .~ rs ^. lfrsNextToken

instance AWSRequest ListFaces where
        type Rs ListFaces = ListFacesResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 ListFacesResponse' <$>
                   (x .?> "FaceModelVersion") <*> (x .?> "NextToken")
                     <*> (x .?> "Faces" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListFaces where

instance NFData ListFaces where

instance ToHeaders ListFaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.ListFaces" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListFaces where
        toJSON ListFaces'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lfNextToken,
                  ("MaxResults" .=) <$> _lfMaxResults,
                  Just ("CollectionId" .= _lfCollectionId)])

instance ToPath ListFaces where
        toPath = const "/"

instance ToQuery ListFaces where
        toQuery = const mempty

-- | /See:/ 'listFacesResponse' smart constructor.
data ListFacesResponse = ListFacesResponse'
  { _lfrsFaceModelVersion :: !(Maybe Text)
  , _lfrsNextToken        :: !(Maybe Text)
  , _lfrsFaces            :: !(Maybe [Face])
  , _lfrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsFaceModelVersion' - Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- * 'lfrsNextToken' - If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
--
-- * 'lfrsFaces' - An array of @Face@ objects.
--
-- * 'lfrsResponseStatus' - -- | The response status code.
listFacesResponse
    :: Int -- ^ 'lfrsResponseStatus'
    -> ListFacesResponse
listFacesResponse pResponseStatus_ =
  ListFacesResponse'
    { _lfrsFaceModelVersion = Nothing
    , _lfrsNextToken = Nothing
    , _lfrsFaces = Nothing
    , _lfrsResponseStatus = pResponseStatus_
    }


-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
lfrsFaceModelVersion :: Lens' ListFacesResponse (Maybe Text)
lfrsFaceModelVersion = lens _lfrsFaceModelVersion (\ s a -> s{_lfrsFaceModelVersion = a})

-- | If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
lfrsNextToken :: Lens' ListFacesResponse (Maybe Text)
lfrsNextToken = lens _lfrsNextToken (\ s a -> s{_lfrsNextToken = a})

-- | An array of @Face@ objects.
lfrsFaces :: Lens' ListFacesResponse [Face]
lfrsFaces = lens _lfrsFaces (\ s a -> s{_lfrsFaces = a}) . _Default . _Coerce

-- | -- | The response status code.
lfrsResponseStatus :: Lens' ListFacesResponse Int
lfrsResponseStatus = lens _lfrsResponseStatus (\ s a -> s{_lfrsResponseStatus = a})

instance NFData ListFacesResponse where
