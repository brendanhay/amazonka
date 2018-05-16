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
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists versions for the specified application.
--
--
module Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
    (
    -- * Creating a Request
      listApplicationVersions
    , ListApplicationVersions
    -- * Request Lenses
    , lavNextToken
    , lavMaxItems
    , lavApplicationId

    -- * Destructuring the Response
    , listApplicationVersionsResponse
    , ListApplicationVersionsResponse
    -- * Response Lenses
    , lavrsVersions
    , lavrsNextToken
    , lavrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'listApplicationVersions' smart constructor.
data ListApplicationVersions = ListApplicationVersions'
  { _lavNextToken     :: !(Maybe Text)
  , _lavMaxItems      :: !(Maybe Nat)
  , _lavApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApplicationVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lavNextToken' - A token to specify where to start paginating.
--
-- * 'lavMaxItems' - The total number of items to return.
--
-- * 'lavApplicationId' - The ID of the application to get.
listApplicationVersions
    :: Text -- ^ 'lavApplicationId'
    -> ListApplicationVersions
listApplicationVersions pApplicationId_ =
  ListApplicationVersions'
    { _lavNextToken = Nothing
    , _lavMaxItems = Nothing
    , _lavApplicationId = pApplicationId_
    }


-- | A token to specify where to start paginating.
lavNextToken :: Lens' ListApplicationVersions (Maybe Text)
lavNextToken = lens _lavNextToken (\ s a -> s{_lavNextToken = a})

-- | The total number of items to return.
lavMaxItems :: Lens' ListApplicationVersions (Maybe Natural)
lavMaxItems = lens _lavMaxItems (\ s a -> s{_lavMaxItems = a}) . mapping _Nat

-- | The ID of the application to get.
lavApplicationId :: Lens' ListApplicationVersions Text
lavApplicationId = lens _lavApplicationId (\ s a -> s{_lavApplicationId = a})

instance AWSRequest ListApplicationVersions where
        type Rs ListApplicationVersions =
             ListApplicationVersionsResponse
        request = get serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 ListApplicationVersionsResponse' <$>
                   (x .?> "versions" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListApplicationVersions where

instance NFData ListApplicationVersions where

instance ToHeaders ListApplicationVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListApplicationVersions where
        toPath ListApplicationVersions'{..}
          = mconcat
              ["/applications/", toBS _lavApplicationId,
               "/versions"]

instance ToQuery ListApplicationVersions where
        toQuery ListApplicationVersions'{..}
          = mconcat
              ["nextToken" =: _lavNextToken,
               "maxItems" =: _lavMaxItems]

-- | /See:/ 'listApplicationVersionsResponse' smart constructor.
data ListApplicationVersionsResponse = ListApplicationVersionsResponse'
  { _lavrsVersions       :: !(Maybe [VersionSummary])
  , _lavrsNextToken      :: !(Maybe Text)
  , _lavrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApplicationVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lavrsVersions' - Array of version summaries for the application.
--
-- * 'lavrsNextToken' - The token to request the next page of results.
--
-- * 'lavrsResponseStatus' - -- | The response status code.
listApplicationVersionsResponse
    :: Int -- ^ 'lavrsResponseStatus'
    -> ListApplicationVersionsResponse
listApplicationVersionsResponse pResponseStatus_ =
  ListApplicationVersionsResponse'
    { _lavrsVersions = Nothing
    , _lavrsNextToken = Nothing
    , _lavrsResponseStatus = pResponseStatus_
    }


-- | Array of version summaries for the application.
lavrsVersions :: Lens' ListApplicationVersionsResponse [VersionSummary]
lavrsVersions = lens _lavrsVersions (\ s a -> s{_lavrsVersions = a}) . _Default . _Coerce

-- | The token to request the next page of results.
lavrsNextToken :: Lens' ListApplicationVersionsResponse (Maybe Text)
lavrsNextToken = lens _lavrsNextToken (\ s a -> s{_lavrsNextToken = a})

-- | -- | The response status code.
lavrsResponseStatus :: Lens' ListApplicationVersionsResponse Int
lavrsResponseStatus = lens _lavrsResponseStatus (\ s a -> s{_lavrsResponseStatus = a})

instance NFData ListApplicationVersionsResponse where
