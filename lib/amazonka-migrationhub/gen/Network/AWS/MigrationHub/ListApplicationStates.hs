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
-- Module      : Network.AWS.MigrationHub.ListApplicationStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the migration statuses for your applications. If you use the optional @ApplicationIds@ parameter, only the migration statuses for those applications will be returned.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListApplicationStates
  ( -- * Creating a Request
    listApplicationStates,
    ListApplicationStates,

    -- * Request Lenses
    lasNextToken,
    lasApplicationIds,
    lasMaxResults,

    -- * Destructuring the Response
    listApplicationStatesResponse,
    ListApplicationStatesResponse,

    -- * Response Lenses
    lasrsApplicationStateList,
    lasrsNextToken,
    lasrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listApplicationStates' smart constructor.
data ListApplicationStates = ListApplicationStates'
  { _lasNextToken ::
      !(Maybe Text),
    _lasApplicationIds :: !(Maybe (List1 Text)),
    _lasMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApplicationStates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasNextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- * 'lasApplicationIds' - The configurationIds from the Application Discovery Service that uniquely identifies your applications.
--
-- * 'lasMaxResults' - Maximum number of results to be returned per page.
listApplicationStates ::
  ListApplicationStates
listApplicationStates =
  ListApplicationStates'
    { _lasNextToken = Nothing,
      _lasApplicationIds = Nothing,
      _lasMaxResults = Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
lasNextToken :: Lens' ListApplicationStates (Maybe Text)
lasNextToken = lens _lasNextToken (\s a -> s {_lasNextToken = a})

-- | The configurationIds from the Application Discovery Service that uniquely identifies your applications.
lasApplicationIds :: Lens' ListApplicationStates (Maybe (NonEmpty Text))
lasApplicationIds = lens _lasApplicationIds (\s a -> s {_lasApplicationIds = a}) . mapping _List1

-- | Maximum number of results to be returned per page.
lasMaxResults :: Lens' ListApplicationStates (Maybe Natural)
lasMaxResults = lens _lasMaxResults (\s a -> s {_lasMaxResults = a}) . mapping _Nat

instance AWSPager ListApplicationStates where
  page rq rs
    | stop (rs ^. lasrsNextToken) = Nothing
    | stop (rs ^. lasrsApplicationStateList) = Nothing
    | otherwise = Just $ rq & lasNextToken .~ rs ^. lasrsNextToken

instance AWSRequest ListApplicationStates where
  type Rs ListApplicationStates = ListApplicationStatesResponse
  request = postJSON migrationHub
  response =
    receiveJSON
      ( \s h x ->
          ListApplicationStatesResponse'
            <$> (x .?> "ApplicationStateList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListApplicationStates

instance NFData ListApplicationStates

instance ToHeaders ListApplicationStates where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSMigrationHub.ListApplicationStates" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListApplicationStates where
  toJSON ListApplicationStates' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lasNextToken,
            ("ApplicationIds" .=) <$> _lasApplicationIds,
            ("MaxResults" .=) <$> _lasMaxResults
          ]
      )

instance ToPath ListApplicationStates where
  toPath = const "/"

instance ToQuery ListApplicationStates where
  toQuery = const mempty

-- | /See:/ 'listApplicationStatesResponse' smart constructor.
data ListApplicationStatesResponse = ListApplicationStatesResponse'
  { _lasrsApplicationStateList ::
      !(Maybe [ApplicationState]),
    _lasrsNextToken ::
      !(Maybe Text),
    _lasrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApplicationStatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasrsApplicationStateList' - A list of Applications that exist in Application Discovery Service.
--
-- * 'lasrsNextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- * 'lasrsResponseStatus' - -- | The response status code.
listApplicationStatesResponse ::
  -- | 'lasrsResponseStatus'
  Int ->
  ListApplicationStatesResponse
listApplicationStatesResponse pResponseStatus_ =
  ListApplicationStatesResponse'
    { _lasrsApplicationStateList =
        Nothing,
      _lasrsNextToken = Nothing,
      _lasrsResponseStatus = pResponseStatus_
    }

-- | A list of Applications that exist in Application Discovery Service.
lasrsApplicationStateList :: Lens' ListApplicationStatesResponse [ApplicationState]
lasrsApplicationStateList = lens _lasrsApplicationStateList (\s a -> s {_lasrsApplicationStateList = a}) . _Default . _Coerce

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
lasrsNextToken :: Lens' ListApplicationStatesResponse (Maybe Text)
lasrsNextToken = lens _lasrsNextToken (\s a -> s {_lasrsNextToken = a})

-- | -- | The response status code.
lasrsResponseStatus :: Lens' ListApplicationStatesResponse Int
lasrsResponseStatus = lens _lasrsResponseStatus (\s a -> s {_lasrsResponseStatus = a})

instance NFData ListApplicationStatesResponse
