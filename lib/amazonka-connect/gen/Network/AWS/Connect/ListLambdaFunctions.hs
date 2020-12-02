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
-- Module      : Network.AWS.Connect.ListLambdaFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the Lambda functions that show up in the drop-down options in the relevant contact flow blocks.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLambdaFunctions
  ( -- * Creating a Request
    listLambdaFunctions,
    ListLambdaFunctions,

    -- * Request Lenses
    llfNextToken,
    llfMaxResults,
    llfInstanceId,

    -- * Destructuring the Response
    listLambdaFunctionsResponse,
    ListLambdaFunctionsResponse,

    -- * Response Lenses
    llfrsLambdaFunctions,
    llfrsNextToken,
    llfrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listLambdaFunctions' smart constructor.
data ListLambdaFunctions = ListLambdaFunctions'
  { _llfNextToken ::
      !(Maybe Text),
    _llfMaxResults :: !(Maybe Nat),
    _llfInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListLambdaFunctions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llfNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'llfMaxResults' - The maximimum number of results to return per page.
--
-- * 'llfInstanceId' - The identifier of the Amazon Connect instance.
listLambdaFunctions ::
  -- | 'llfInstanceId'
  Text ->
  ListLambdaFunctions
listLambdaFunctions pInstanceId_ =
  ListLambdaFunctions'
    { _llfNextToken = Nothing,
      _llfMaxResults = Nothing,
      _llfInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
llfNextToken :: Lens' ListLambdaFunctions (Maybe Text)
llfNextToken = lens _llfNextToken (\s a -> s {_llfNextToken = a})

-- | The maximimum number of results to return per page.
llfMaxResults :: Lens' ListLambdaFunctions (Maybe Natural)
llfMaxResults = lens _llfMaxResults (\s a -> s {_llfMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
llfInstanceId :: Lens' ListLambdaFunctions Text
llfInstanceId = lens _llfInstanceId (\s a -> s {_llfInstanceId = a})

instance AWSPager ListLambdaFunctions where
  page rq rs
    | stop (rs ^. llfrsNextToken) = Nothing
    | stop (rs ^. llfrsLambdaFunctions) = Nothing
    | otherwise = Just $ rq & llfNextToken .~ rs ^. llfrsNextToken

instance AWSRequest ListLambdaFunctions where
  type Rs ListLambdaFunctions = ListLambdaFunctionsResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListLambdaFunctionsResponse'
            <$> (x .?> "LambdaFunctions" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListLambdaFunctions

instance NFData ListLambdaFunctions

instance ToHeaders ListLambdaFunctions where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListLambdaFunctions where
  toPath ListLambdaFunctions' {..} =
    mconcat ["/instance/", toBS _llfInstanceId, "/lambda-functions"]

instance ToQuery ListLambdaFunctions where
  toQuery ListLambdaFunctions' {..} =
    mconcat
      ["nextToken" =: _llfNextToken, "maxResults" =: _llfMaxResults]

-- | /See:/ 'listLambdaFunctionsResponse' smart constructor.
data ListLambdaFunctionsResponse = ListLambdaFunctionsResponse'
  { _llfrsLambdaFunctions ::
      !(Maybe [Text]),
    _llfrsNextToken :: !(Maybe Text),
    _llfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListLambdaFunctionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llfrsLambdaFunctions' - The Lambdafunction ARNs associated with the specified instance.
--
-- * 'llfrsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'llfrsResponseStatus' - -- | The response status code.
listLambdaFunctionsResponse ::
  -- | 'llfrsResponseStatus'
  Int ->
  ListLambdaFunctionsResponse
listLambdaFunctionsResponse pResponseStatus_ =
  ListLambdaFunctionsResponse'
    { _llfrsLambdaFunctions = Nothing,
      _llfrsNextToken = Nothing,
      _llfrsResponseStatus = pResponseStatus_
    }

-- | The Lambdafunction ARNs associated with the specified instance.
llfrsLambdaFunctions :: Lens' ListLambdaFunctionsResponse [Text]
llfrsLambdaFunctions = lens _llfrsLambdaFunctions (\s a -> s {_llfrsLambdaFunctions = a}) . _Default . _Coerce

-- | If there are additional results, this is the token for the next set of results.
llfrsNextToken :: Lens' ListLambdaFunctionsResponse (Maybe Text)
llfrsNextToken = lens _llfrsNextToken (\s a -> s {_llfrsNextToken = a})

-- | -- | The response status code.
llfrsResponseStatus :: Lens' ListLambdaFunctionsResponse Int
llfrsResponseStatus = lens _llfrsResponseStatus (\s a -> s {_llfrsResponseStatus = a})

instance NFData ListLambdaFunctionsResponse
