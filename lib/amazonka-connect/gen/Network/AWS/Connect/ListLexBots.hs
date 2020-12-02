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
-- Module      : Network.AWS.Connect.ListLexBots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the Amazon Lex bots currently associated with the instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLexBots
  ( -- * Creating a Request
    listLexBots,
    ListLexBots,

    -- * Request Lenses
    llbNextToken,
    llbMaxResults,
    llbInstanceId,

    -- * Destructuring the Response
    listLexBotsResponse,
    ListLexBotsResponse,

    -- * Response Lenses
    llbrsNextToken,
    llbrsLexBots,
    llbrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listLexBots' smart constructor.
data ListLexBots = ListLexBots'
  { _llbNextToken :: !(Maybe Text),
    _llbMaxResults :: !(Maybe Nat),
    _llbInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListLexBots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llbNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'llbMaxResults' - The maximimum number of results to return per page.
--
-- * 'llbInstanceId' - The identifier of the Amazon Connect instance.
listLexBots ::
  -- | 'llbInstanceId'
  Text ->
  ListLexBots
listLexBots pInstanceId_ =
  ListLexBots'
    { _llbNextToken = Nothing,
      _llbMaxResults = Nothing,
      _llbInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
llbNextToken :: Lens' ListLexBots (Maybe Text)
llbNextToken = lens _llbNextToken (\s a -> s {_llbNextToken = a})

-- | The maximimum number of results to return per page.
llbMaxResults :: Lens' ListLexBots (Maybe Natural)
llbMaxResults = lens _llbMaxResults (\s a -> s {_llbMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
llbInstanceId :: Lens' ListLexBots Text
llbInstanceId = lens _llbInstanceId (\s a -> s {_llbInstanceId = a})

instance AWSPager ListLexBots where
  page rq rs
    | stop (rs ^. llbrsNextToken) = Nothing
    | stop (rs ^. llbrsLexBots) = Nothing
    | otherwise = Just $ rq & llbNextToken .~ rs ^. llbrsNextToken

instance AWSRequest ListLexBots where
  type Rs ListLexBots = ListLexBotsResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListLexBotsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "LexBots" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListLexBots

instance NFData ListLexBots

instance ToHeaders ListLexBots where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListLexBots where
  toPath ListLexBots' {..} =
    mconcat ["/instance/", toBS _llbInstanceId, "/lex-bots"]

instance ToQuery ListLexBots where
  toQuery ListLexBots' {..} =
    mconcat
      ["nextToken" =: _llbNextToken, "maxResults" =: _llbMaxResults]

-- | /See:/ 'listLexBotsResponse' smart constructor.
data ListLexBotsResponse = ListLexBotsResponse'
  { _llbrsNextToken ::
      !(Maybe Text),
    _llbrsLexBots :: !(Maybe [LexBot]),
    _llbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListLexBotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llbrsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'llbrsLexBots' - The the names and regions of the Amazon Lex bots associated with the specified instance.
--
-- * 'llbrsResponseStatus' - -- | The response status code.
listLexBotsResponse ::
  -- | 'llbrsResponseStatus'
  Int ->
  ListLexBotsResponse
listLexBotsResponse pResponseStatus_ =
  ListLexBotsResponse'
    { _llbrsNextToken = Nothing,
      _llbrsLexBots = Nothing,
      _llbrsResponseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
llbrsNextToken :: Lens' ListLexBotsResponse (Maybe Text)
llbrsNextToken = lens _llbrsNextToken (\s a -> s {_llbrsNextToken = a})

-- | The the names and regions of the Amazon Lex bots associated with the specified instance.
llbrsLexBots :: Lens' ListLexBotsResponse [LexBot]
llbrsLexBots = lens _llbrsLexBots (\s a -> s {_llbrsLexBots = a}) . _Default . _Coerce

-- | -- | The response status code.
llbrsResponseStatus :: Lens' ListLexBotsResponse Int
llbrsResponseStatus = lens _llbrsResponseStatus (\s a -> s {_llbrsResponseStatus = a})

instance NFData ListLexBotsResponse
