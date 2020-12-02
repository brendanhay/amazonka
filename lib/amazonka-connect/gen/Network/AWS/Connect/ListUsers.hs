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
-- Module      : Network.AWS.Connect.ListUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the users for the specified Amazon Connect instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUsers
  ( -- * Creating a Request
    listUsers,
    ListUsers,

    -- * Request Lenses
    luNextToken,
    luMaxResults,
    luInstanceId,

    -- * Destructuring the Response
    listUsersResponse,
    ListUsersResponse,

    -- * Response Lenses
    lursNextToken,
    lursUserSummaryList,
    lursResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUsers' smart constructor.
data ListUsers = ListUsers'
  { _luNextToken :: !(Maybe Text),
    _luMaxResults :: !(Maybe Nat),
    _luInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'luMaxResults' - The maximimum number of results to return per page.
--
-- * 'luInstanceId' - The identifier of the Amazon Connect instance.
listUsers ::
  -- | 'luInstanceId'
  Text ->
  ListUsers
listUsers pInstanceId_ =
  ListUsers'
    { _luNextToken = Nothing,
      _luMaxResults = Nothing,
      _luInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
luNextToken :: Lens' ListUsers (Maybe Text)
luNextToken = lens _luNextToken (\s a -> s {_luNextToken = a})

-- | The maximimum number of results to return per page.
luMaxResults :: Lens' ListUsers (Maybe Natural)
luMaxResults = lens _luMaxResults (\s a -> s {_luMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
luInstanceId :: Lens' ListUsers Text
luInstanceId = lens _luInstanceId (\s a -> s {_luInstanceId = a})

instance AWSPager ListUsers where
  page rq rs
    | stop (rs ^. lursNextToken) = Nothing
    | stop (rs ^. lursUserSummaryList) = Nothing
    | otherwise = Just $ rq & luNextToken .~ rs ^. lursNextToken

instance AWSRequest ListUsers where
  type Rs ListUsers = ListUsersResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListUsersResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "UserSummaryList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListUsers

instance NFData ListUsers

instance ToHeaders ListUsers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListUsers where
  toPath ListUsers' {..} =
    mconcat ["/users-summary/", toBS _luInstanceId]

instance ToQuery ListUsers where
  toQuery ListUsers' {..} =
    mconcat
      ["nextToken" =: _luNextToken, "maxResults" =: _luMaxResults]

-- | /See:/ 'listUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { _lursNextToken ::
      !(Maybe Text),
    _lursUserSummaryList :: !(Maybe [UserSummary]),
    _lursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'lursUserSummaryList' - Information about the users.
--
-- * 'lursResponseStatus' - -- | The response status code.
listUsersResponse ::
  -- | 'lursResponseStatus'
  Int ->
  ListUsersResponse
listUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { _lursNextToken = Nothing,
      _lursUserSummaryList = Nothing,
      _lursResponseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
lursNextToken :: Lens' ListUsersResponse (Maybe Text)
lursNextToken = lens _lursNextToken (\s a -> s {_lursNextToken = a})

-- | Information about the users.
lursUserSummaryList :: Lens' ListUsersResponse [UserSummary]
lursUserSummaryList = lens _lursUserSummaryList (\s a -> s {_lursUserSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
lursResponseStatus :: Lens' ListUsersResponse Int
lursResponseStatus = lens _lursResponseStatus (\s a -> s {_lursResponseStatus = a})

instance NFData ListUsersResponse
