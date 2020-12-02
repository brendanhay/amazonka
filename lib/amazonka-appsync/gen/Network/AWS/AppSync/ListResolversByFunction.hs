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
-- Module      : Network.AWS.AppSync.ListResolversByFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the resolvers that are associated with a specific function.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolversByFunction
  ( -- * Creating a Request
    listResolversByFunction,
    ListResolversByFunction,

    -- * Request Lenses
    lrbfNextToken,
    lrbfMaxResults,
    lrbfApiId,
    lrbfFunctionId,

    -- * Destructuring the Response
    listResolversByFunctionResponse,
    ListResolversByFunctionResponse,

    -- * Response Lenses
    lrbfrsNextToken,
    lrbfrsResolvers,
    lrbfrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listResolversByFunction' smart constructor.
data ListResolversByFunction = ListResolversByFunction'
  { _lrbfNextToken ::
      !(Maybe Text),
    _lrbfMaxResults :: !(Maybe Nat),
    _lrbfApiId :: !Text,
    _lrbfFunctionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListResolversByFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrbfNextToken' - An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
--
-- * 'lrbfMaxResults' - The maximum number of results you want the request to return.
--
-- * 'lrbfApiId' - The API ID.
--
-- * 'lrbfFunctionId' - The Function ID.
listResolversByFunction ::
  -- | 'lrbfApiId'
  Text ->
  -- | 'lrbfFunctionId'
  Text ->
  ListResolversByFunction
listResolversByFunction pApiId_ pFunctionId_ =
  ListResolversByFunction'
    { _lrbfNextToken = Nothing,
      _lrbfMaxResults = Nothing,
      _lrbfApiId = pApiId_,
      _lrbfFunctionId = pFunctionId_
    }

-- | An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
lrbfNextToken :: Lens' ListResolversByFunction (Maybe Text)
lrbfNextToken = lens _lrbfNextToken (\s a -> s {_lrbfNextToken = a})

-- | The maximum number of results you want the request to return.
lrbfMaxResults :: Lens' ListResolversByFunction (Maybe Natural)
lrbfMaxResults = lens _lrbfMaxResults (\s a -> s {_lrbfMaxResults = a}) . mapping _Nat

-- | The API ID.
lrbfApiId :: Lens' ListResolversByFunction Text
lrbfApiId = lens _lrbfApiId (\s a -> s {_lrbfApiId = a})

-- | The Function ID.
lrbfFunctionId :: Lens' ListResolversByFunction Text
lrbfFunctionId = lens _lrbfFunctionId (\s a -> s {_lrbfFunctionId = a})

instance AWSPager ListResolversByFunction where
  page rq rs
    | stop (rs ^. lrbfrsNextToken) = Nothing
    | stop (rs ^. lrbfrsResolvers) = Nothing
    | otherwise = Just $ rq & lrbfNextToken .~ rs ^. lrbfrsNextToken

instance AWSRequest ListResolversByFunction where
  type Rs ListResolversByFunction = ListResolversByFunctionResponse
  request = get appSync
  response =
    receiveJSON
      ( \s h x ->
          ListResolversByFunctionResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "resolvers" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListResolversByFunction

instance NFData ListResolversByFunction

instance ToHeaders ListResolversByFunction where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListResolversByFunction where
  toPath ListResolversByFunction' {..} =
    mconcat
      [ "/v1/apis/",
        toBS _lrbfApiId,
        "/functions/",
        toBS _lrbfFunctionId,
        "/resolvers"
      ]

instance ToQuery ListResolversByFunction where
  toQuery ListResolversByFunction' {..} =
    mconcat
      ["nextToken" =: _lrbfNextToken, "maxResults" =: _lrbfMaxResults]

-- | /See:/ 'listResolversByFunctionResponse' smart constructor.
data ListResolversByFunctionResponse = ListResolversByFunctionResponse'
  { _lrbfrsNextToken ::
      !(Maybe Text),
    _lrbfrsResolvers ::
      !(Maybe [Resolver]),
    _lrbfrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListResolversByFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrbfrsNextToken' - An identifier that can be used to return the next set of items in the list.
--
-- * 'lrbfrsResolvers' - The list of resolvers.
--
-- * 'lrbfrsResponseStatus' - -- | The response status code.
listResolversByFunctionResponse ::
  -- | 'lrbfrsResponseStatus'
  Int ->
  ListResolversByFunctionResponse
listResolversByFunctionResponse pResponseStatus_ =
  ListResolversByFunctionResponse'
    { _lrbfrsNextToken = Nothing,
      _lrbfrsResolvers = Nothing,
      _lrbfrsResponseStatus = pResponseStatus_
    }

-- | An identifier that can be used to return the next set of items in the list.
lrbfrsNextToken :: Lens' ListResolversByFunctionResponse (Maybe Text)
lrbfrsNextToken = lens _lrbfrsNextToken (\s a -> s {_lrbfrsNextToken = a})

-- | The list of resolvers.
lrbfrsResolvers :: Lens' ListResolversByFunctionResponse [Resolver]
lrbfrsResolvers = lens _lrbfrsResolvers (\s a -> s {_lrbfrsResolvers = a}) . _Default . _Coerce

-- | -- | The response status code.
lrbfrsResponseStatus :: Lens' ListResolversByFunctionResponse Int
lrbfrsResponseStatus = lens _lrbfrsResponseStatus (\s a -> s {_lrbfrsResponseStatus = a})

instance NFData ListResolversByFunctionResponse
