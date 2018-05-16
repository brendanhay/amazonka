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
-- Module      : Network.AWS.MigrationHub.ListCreatedArtifacts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the created artifacts attached to a given migration task in an update stream. This API has the following traits:
--
--
--     * Gets the list of the created artifacts while migration is taking place.
--
--     * Shows the artifacts created by the migration tool that was associated by the @AssociateCreatedArtifact@ API.
--
--     * Lists created artifacts in a paginated interface.
--
--
--
module Network.AWS.MigrationHub.ListCreatedArtifacts
    (
    -- * Creating a Request
      listCreatedArtifacts
    , ListCreatedArtifacts
    -- * Request Lenses
    , lcaNextToken
    , lcaMaxResults
    , lcaProgressUpdateStream
    , lcaMigrationTaskName

    -- * Destructuring the Response
    , listCreatedArtifactsResponse
    , ListCreatedArtifactsResponse
    -- * Response Lenses
    , lcarsNextToken
    , lcarsCreatedArtifactList
    , lcarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCreatedArtifacts' smart constructor.
data ListCreatedArtifacts = ListCreatedArtifacts'
  { _lcaNextToken            :: !(Maybe Text)
  , _lcaMaxResults           :: !(Maybe Nat)
  , _lcaProgressUpdateStream :: !Text
  , _lcaMigrationTaskName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCreatedArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcaNextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- * 'lcaMaxResults' - Maximum number of results to be returned per page.
--
-- * 'lcaProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'lcaMigrationTaskName' - Unique identifier that references the migration task.
listCreatedArtifacts
    :: Text -- ^ 'lcaProgressUpdateStream'
    -> Text -- ^ 'lcaMigrationTaskName'
    -> ListCreatedArtifacts
listCreatedArtifacts pProgressUpdateStream_ pMigrationTaskName_ =
  ListCreatedArtifacts'
    { _lcaNextToken = Nothing
    , _lcaMaxResults = Nothing
    , _lcaProgressUpdateStream = pProgressUpdateStream_
    , _lcaMigrationTaskName = pMigrationTaskName_
    }


-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
lcaNextToken :: Lens' ListCreatedArtifacts (Maybe Text)
lcaNextToken = lens _lcaNextToken (\ s a -> s{_lcaNextToken = a})

-- | Maximum number of results to be returned per page.
lcaMaxResults :: Lens' ListCreatedArtifacts (Maybe Natural)
lcaMaxResults = lens _lcaMaxResults (\ s a -> s{_lcaMaxResults = a}) . mapping _Nat

-- | The name of the ProgressUpdateStream.
lcaProgressUpdateStream :: Lens' ListCreatedArtifacts Text
lcaProgressUpdateStream = lens _lcaProgressUpdateStream (\ s a -> s{_lcaProgressUpdateStream = a})

-- | Unique identifier that references the migration task.
lcaMigrationTaskName :: Lens' ListCreatedArtifacts Text
lcaMigrationTaskName = lens _lcaMigrationTaskName (\ s a -> s{_lcaMigrationTaskName = a})

instance AWSRequest ListCreatedArtifacts where
        type Rs ListCreatedArtifacts =
             ListCreatedArtifactsResponse
        request = postJSON migrationHub
        response
          = receiveJSON
              (\ s h x ->
                 ListCreatedArtifactsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "CreatedArtifactList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListCreatedArtifacts where

instance NFData ListCreatedArtifacts where

instance ToHeaders ListCreatedArtifacts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.ListCreatedArtifacts" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCreatedArtifacts where
        toJSON ListCreatedArtifacts'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lcaNextToken,
                  ("MaxResults" .=) <$> _lcaMaxResults,
                  Just
                    ("ProgressUpdateStream" .= _lcaProgressUpdateStream),
                  Just ("MigrationTaskName" .= _lcaMigrationTaskName)])

instance ToPath ListCreatedArtifacts where
        toPath = const "/"

instance ToQuery ListCreatedArtifacts where
        toQuery = const mempty

-- | /See:/ 'listCreatedArtifactsResponse' smart constructor.
data ListCreatedArtifactsResponse = ListCreatedArtifactsResponse'
  { _lcarsNextToken           :: !(Maybe Text)
  , _lcarsCreatedArtifactList :: !(Maybe [CreatedArtifact])
  , _lcarsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCreatedArtifactsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcarsNextToken' - If there are more created artifacts than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- * 'lcarsCreatedArtifactList' - List of created artifacts up to the maximum number of results specified in the request.
--
-- * 'lcarsResponseStatus' - -- | The response status code.
listCreatedArtifactsResponse
    :: Int -- ^ 'lcarsResponseStatus'
    -> ListCreatedArtifactsResponse
listCreatedArtifactsResponse pResponseStatus_ =
  ListCreatedArtifactsResponse'
    { _lcarsNextToken = Nothing
    , _lcarsCreatedArtifactList = Nothing
    , _lcarsResponseStatus = pResponseStatus_
    }


-- | If there are more created artifacts than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
lcarsNextToken :: Lens' ListCreatedArtifactsResponse (Maybe Text)
lcarsNextToken = lens _lcarsNextToken (\ s a -> s{_lcarsNextToken = a})

-- | List of created artifacts up to the maximum number of results specified in the request.
lcarsCreatedArtifactList :: Lens' ListCreatedArtifactsResponse [CreatedArtifact]
lcarsCreatedArtifactList = lens _lcarsCreatedArtifactList (\ s a -> s{_lcarsCreatedArtifactList = a}) . _Default . _Coerce

-- | -- | The response status code.
lcarsResponseStatus :: Lens' ListCreatedArtifactsResponse Int
lcarsResponseStatus = lens _lcarsResponseStatus (\ s a -> s{_lcarsResponseStatus = a})

instance NFData ListCreatedArtifactsResponse where
