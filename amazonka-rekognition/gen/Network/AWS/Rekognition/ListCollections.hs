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
-- Module      : Network.AWS.Rekognition.ListCollections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of collection IDs in your account. If the result is truncated, the response also provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
--
--
-- For an example, see 'list-collection-procedure' .
--
-- This operation requires permissions to perform the @rekognition:ListCollections@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListCollections
    (
    -- * Creating a Request
      listCollections
    , ListCollections
    -- * Request Lenses
    , lcNextToken
    , lcMaxResults

    -- * Destructuring the Response
    , listCollectionsResponse
    , ListCollectionsResponse
    -- * Response Lenses
    , lcrsCollectionIds
    , lcrsNextToken
    , lcrsFaceModelVersions
    , lcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCollections' smart constructor.
data ListCollections = ListCollections'
  { _lcNextToken  :: !(Maybe Text)
  , _lcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCollections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - Pagination token from the previous response.
--
-- * 'lcMaxResults' - Maximum number of collection IDs to return.
listCollections
    :: ListCollections
listCollections =
  ListCollections' {_lcNextToken = Nothing, _lcMaxResults = Nothing}


-- | Pagination token from the previous response.
lcNextToken :: Lens' ListCollections (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | Maximum number of collection IDs to return.
lcMaxResults :: Lens' ListCollections (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

instance AWSPager ListCollections where
        page rq rs
          | stop (rs ^. lcrsNextToken) = Nothing
          | stop (rs ^. lcrsCollectionIds) = Nothing
          | stop (rs ^. lcrsFaceModelVersions) = Nothing
          | otherwise =
            Just $ rq & lcNextToken .~ rs ^. lcrsNextToken

instance AWSRequest ListCollections where
        type Rs ListCollections = ListCollectionsResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 ListCollectionsResponse' <$>
                   (x .?> "CollectionIds" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "FaceModelVersions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListCollections where

instance NFData ListCollections where

instance ToHeaders ListCollections where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.ListCollections" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCollections where
        toJSON ListCollections'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lcNextToken,
                  ("MaxResults" .=) <$> _lcMaxResults])

instance ToPath ListCollections where
        toPath = const "/"

instance ToQuery ListCollections where
        toQuery = const mempty

-- | /See:/ 'listCollectionsResponse' smart constructor.
data ListCollectionsResponse = ListCollectionsResponse'
  { _lcrsCollectionIds     :: !(Maybe [Text])
  , _lcrsNextToken         :: !(Maybe Text)
  , _lcrsFaceModelVersions :: !(Maybe [Text])
  , _lcrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCollectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsCollectionIds' - An array of collection IDs.
--
-- * 'lcrsNextToken' - If the result is truncated, the response provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
--
-- * 'lcrsFaceModelVersions' - Version numbers of the face detection models associated with the collections in the array @CollectionIds@ . For example, the value of @FaceModelVersions[2]@ is the version number for the face detection model used by the collection in @CollectionId[2]@ .
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listCollectionsResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListCollectionsResponse
listCollectionsResponse pResponseStatus_ =
  ListCollectionsResponse'
    { _lcrsCollectionIds = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsFaceModelVersions = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | An array of collection IDs.
lcrsCollectionIds :: Lens' ListCollectionsResponse [Text]
lcrsCollectionIds = lens _lcrsCollectionIds (\ s a -> s{_lcrsCollectionIds = a}) . _Default . _Coerce

-- | If the result is truncated, the response provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
lcrsNextToken :: Lens' ListCollectionsResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | Version numbers of the face detection models associated with the collections in the array @CollectionIds@ . For example, the value of @FaceModelVersions[2]@ is the version number for the face detection model used by the collection in @CollectionId[2]@ .
lcrsFaceModelVersions :: Lens' ListCollectionsResponse [Text]
lcrsFaceModelVersions = lens _lcrsFaceModelVersions (\ s a -> s{_lcrsFaceModelVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListCollectionsResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListCollectionsResponse where
