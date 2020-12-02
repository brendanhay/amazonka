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
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of applications nested in the containing application.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplicationDependencies
  ( -- * Creating a Request
    listApplicationDependencies,
    ListApplicationDependencies,

    -- * Request Lenses
    ladSemanticVersion,
    ladNextToken,
    ladMaxItems,
    ladApplicationId,

    -- * Destructuring the Response
    listApplicationDependenciesResponse,
    ListApplicationDependenciesResponse,

    -- * Response Lenses
    ladrsDependencies,
    ladrsNextToken,
    ladrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'listApplicationDependencies' smart constructor.
data ListApplicationDependencies = ListApplicationDependencies'
  { _ladSemanticVersion ::
      !(Maybe Text),
    _ladNextToken :: !(Maybe Text),
    _ladMaxItems :: !(Maybe Nat),
    _ladApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApplicationDependencies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ladSemanticVersion' - The semantic version of the application to get.
--
-- * 'ladNextToken' - A token to specify where to start paginating.
--
-- * 'ladMaxItems' - The total number of items to return.
--
-- * 'ladApplicationId' - The Amazon Resource Name (ARN) of the application.
listApplicationDependencies ::
  -- | 'ladApplicationId'
  Text ->
  ListApplicationDependencies
listApplicationDependencies pApplicationId_ =
  ListApplicationDependencies'
    { _ladSemanticVersion = Nothing,
      _ladNextToken = Nothing,
      _ladMaxItems = Nothing,
      _ladApplicationId = pApplicationId_
    }

-- | The semantic version of the application to get.
ladSemanticVersion :: Lens' ListApplicationDependencies (Maybe Text)
ladSemanticVersion = lens _ladSemanticVersion (\s a -> s {_ladSemanticVersion = a})

-- | A token to specify where to start paginating.
ladNextToken :: Lens' ListApplicationDependencies (Maybe Text)
ladNextToken = lens _ladNextToken (\s a -> s {_ladNextToken = a})

-- | The total number of items to return.
ladMaxItems :: Lens' ListApplicationDependencies (Maybe Natural)
ladMaxItems = lens _ladMaxItems (\s a -> s {_ladMaxItems = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the application.
ladApplicationId :: Lens' ListApplicationDependencies Text
ladApplicationId = lens _ladApplicationId (\s a -> s {_ladApplicationId = a})

instance AWSPager ListApplicationDependencies where
  page rq rs
    | stop (rs ^. ladrsNextToken) = Nothing
    | stop (rs ^. ladrsDependencies) = Nothing
    | otherwise = Just $ rq & ladNextToken .~ rs ^. ladrsNextToken

instance AWSRequest ListApplicationDependencies where
  type
    Rs ListApplicationDependencies =
      ListApplicationDependenciesResponse
  request = get serverlessApplicationRepository
  response =
    receiveJSON
      ( \s h x ->
          ListApplicationDependenciesResponse'
            <$> (x .?> "dependencies" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListApplicationDependencies

instance NFData ListApplicationDependencies

instance ToHeaders ListApplicationDependencies where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListApplicationDependencies where
  toPath ListApplicationDependencies' {..} =
    mconcat
      ["/applications/", toBS _ladApplicationId, "/dependencies"]

instance ToQuery ListApplicationDependencies where
  toQuery ListApplicationDependencies' {..} =
    mconcat
      [ "semanticVersion" =: _ladSemanticVersion,
        "nextToken" =: _ladNextToken,
        "maxItems" =: _ladMaxItems
      ]

-- | /See:/ 'listApplicationDependenciesResponse' smart constructor.
data ListApplicationDependenciesResponse = ListApplicationDependenciesResponse'
  { _ladrsDependencies ::
      !( Maybe
           [ApplicationDependencySummary]
       ),
    _ladrsNextToken ::
      !(Maybe Text),
    _ladrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApplicationDependenciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ladrsDependencies' - An array of application summaries nested in the application.
--
-- * 'ladrsNextToken' - The token to request the next page of results.
--
-- * 'ladrsResponseStatus' - -- | The response status code.
listApplicationDependenciesResponse ::
  -- | 'ladrsResponseStatus'
  Int ->
  ListApplicationDependenciesResponse
listApplicationDependenciesResponse pResponseStatus_ =
  ListApplicationDependenciesResponse'
    { _ladrsDependencies =
        Nothing,
      _ladrsNextToken = Nothing,
      _ladrsResponseStatus = pResponseStatus_
    }

-- | An array of application summaries nested in the application.
ladrsDependencies :: Lens' ListApplicationDependenciesResponse [ApplicationDependencySummary]
ladrsDependencies = lens _ladrsDependencies (\s a -> s {_ladrsDependencies = a}) . _Default . _Coerce

-- | The token to request the next page of results.
ladrsNextToken :: Lens' ListApplicationDependenciesResponse (Maybe Text)
ladrsNextToken = lens _ladrsNextToken (\s a -> s {_ladrsNextToken = a})

-- | -- | The response status code.
ladrsResponseStatus :: Lens' ListApplicationDependenciesResponse Int
ladrsResponseStatus = lens _ladrsResponseStatus (\s a -> s {_ladrsResponseStatus = a})

instance NFData ListApplicationDependenciesResponse
