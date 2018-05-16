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
-- Module      : Network.AWS.MigrationHub.ListDiscoveredResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists discovered resources associated with the given @MigrationTask@ .
--
--
module Network.AWS.MigrationHub.ListDiscoveredResources
    (
    -- * Creating a Request
      listDiscoveredResources
    , ListDiscoveredResources
    -- * Request Lenses
    , ldrNextToken
    , ldrMaxResults
    , ldrProgressUpdateStream
    , ldrMigrationTaskName

    -- * Destructuring the Response
    , listDiscoveredResourcesResponse
    , ListDiscoveredResourcesResponse
    -- * Response Lenses
    , ldrrsDiscoveredResourceList
    , ldrrsNextToken
    , ldrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { _ldrNextToken            :: !(Maybe Text)
  , _ldrMaxResults           :: !(Maybe Nat)
  , _ldrProgressUpdateStream :: !Text
  , _ldrMigrationTaskName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDiscoveredResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrNextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- * 'ldrMaxResults' - The maximum number of results returned per page.
--
-- * 'ldrProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'ldrMigrationTaskName' - The name of the MigrationTask.
listDiscoveredResources
    :: Text -- ^ 'ldrProgressUpdateStream'
    -> Text -- ^ 'ldrMigrationTaskName'
    -> ListDiscoveredResources
listDiscoveredResources pProgressUpdateStream_ pMigrationTaskName_ =
  ListDiscoveredResources'
    { _ldrNextToken = Nothing
    , _ldrMaxResults = Nothing
    , _ldrProgressUpdateStream = pProgressUpdateStream_
    , _ldrMigrationTaskName = pMigrationTaskName_
    }


-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
ldrNextToken :: Lens' ListDiscoveredResources (Maybe Text)
ldrNextToken = lens _ldrNextToken (\ s a -> s{_ldrNextToken = a})

-- | The maximum number of results returned per page.
ldrMaxResults :: Lens' ListDiscoveredResources (Maybe Natural)
ldrMaxResults = lens _ldrMaxResults (\ s a -> s{_ldrMaxResults = a}) . mapping _Nat

-- | The name of the ProgressUpdateStream.
ldrProgressUpdateStream :: Lens' ListDiscoveredResources Text
ldrProgressUpdateStream = lens _ldrProgressUpdateStream (\ s a -> s{_ldrProgressUpdateStream = a})

-- | The name of the MigrationTask.
ldrMigrationTaskName :: Lens' ListDiscoveredResources Text
ldrMigrationTaskName = lens _ldrMigrationTaskName (\ s a -> s{_ldrMigrationTaskName = a})

instance AWSRequest ListDiscoveredResources where
        type Rs ListDiscoveredResources =
             ListDiscoveredResourcesResponse
        request = postJSON migrationHub
        response
          = receiveJSON
              (\ s h x ->
                 ListDiscoveredResourcesResponse' <$>
                   (x .?> "DiscoveredResourceList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListDiscoveredResources where

instance NFData ListDiscoveredResources where

instance ToHeaders ListDiscoveredResources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.ListDiscoveredResources" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDiscoveredResources where
        toJSON ListDiscoveredResources'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ldrNextToken,
                  ("MaxResults" .=) <$> _ldrMaxResults,
                  Just
                    ("ProgressUpdateStream" .= _ldrProgressUpdateStream),
                  Just ("MigrationTaskName" .= _ldrMigrationTaskName)])

instance ToPath ListDiscoveredResources where
        toPath = const "/"

instance ToQuery ListDiscoveredResources where
        toQuery = const mempty

-- | /See:/ 'listDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { _ldrrsDiscoveredResourceList :: !(Maybe [DiscoveredResource])
  , _ldrrsNextToken              :: !(Maybe Text)
  , _ldrrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDiscoveredResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrrsDiscoveredResourceList' - Returned list of discovered resources associated with the given MigrationTask.
--
-- * 'ldrrsNextToken' - If there are more discovered resources than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- * 'ldrrsResponseStatus' - -- | The response status code.
listDiscoveredResourcesResponse
    :: Int -- ^ 'ldrrsResponseStatus'
    -> ListDiscoveredResourcesResponse
listDiscoveredResourcesResponse pResponseStatus_ =
  ListDiscoveredResourcesResponse'
    { _ldrrsDiscoveredResourceList = Nothing
    , _ldrrsNextToken = Nothing
    , _ldrrsResponseStatus = pResponseStatus_
    }


-- | Returned list of discovered resources associated with the given MigrationTask.
ldrrsDiscoveredResourceList :: Lens' ListDiscoveredResourcesResponse [DiscoveredResource]
ldrrsDiscoveredResourceList = lens _ldrrsDiscoveredResourceList (\ s a -> s{_ldrrsDiscoveredResourceList = a}) . _Default . _Coerce

-- | If there are more discovered resources than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
ldrrsNextToken :: Lens' ListDiscoveredResourcesResponse (Maybe Text)
ldrrsNextToken = lens _ldrrsNextToken (\ s a -> s{_ldrrsNextToken = a})

-- | -- | The response status code.
ldrrsResponseStatus :: Lens' ListDiscoveredResourcesResponse Int
ldrrsResponseStatus = lens _ldrrsResponseStatus (\ s a -> s{_ldrrsResponseStatus = a})

instance NFData ListDiscoveredResourcesResponse where
