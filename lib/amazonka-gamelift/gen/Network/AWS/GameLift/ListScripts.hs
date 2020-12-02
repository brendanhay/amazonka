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
-- Module      : Network.AWS.GameLift.ListScripts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves script records for all Realtime scripts that are associated with the AWS account in use.
--
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- __Related operations__
--
--     * 'CreateScript'
--
--     * 'ListScripts'
--
--     * 'DescribeScript'
--
--     * 'UpdateScript'
--
--     * 'DeleteScript'
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListScripts
  ( -- * Creating a Request
    listScripts,
    ListScripts,

    -- * Request Lenses
    lsNextToken,
    lsLimit,

    -- * Destructuring the Response
    listScriptsResponse,
    ListScriptsResponse,

    -- * Response Lenses
    lsrsScripts,
    lsrsNextToken,
    lsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listScripts' smart constructor.
data ListScripts = ListScripts'
  { _lsNextToken :: !(Maybe Text),
    _lsLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListScripts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsNextToken' - A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- * 'lsLimit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
listScripts ::
  ListScripts
listScripts =
  ListScripts' {_lsNextToken = Nothing, _lsLimit = Nothing}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
lsNextToken :: Lens' ListScripts (Maybe Text)
lsNextToken = lens _lsNextToken (\s a -> s {_lsNextToken = a})

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
lsLimit :: Lens' ListScripts (Maybe Natural)
lsLimit = lens _lsLimit (\s a -> s {_lsLimit = a}) . mapping _Nat

instance AWSPager ListScripts where
  page rq rs
    | stop (rs ^. lsrsNextToken) = Nothing
    | stop (rs ^. lsrsScripts) = Nothing
    | otherwise = Just $ rq & lsNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListScripts where
  type Rs ListScripts = ListScriptsResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          ListScriptsResponse'
            <$> (x .?> "Scripts" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListScripts

instance NFData ListScripts

instance ToHeaders ListScripts where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.ListScripts" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListScripts where
  toJSON ListScripts' {..} =
    object
      ( catMaybes
          [("NextToken" .=) <$> _lsNextToken, ("Limit" .=) <$> _lsLimit]
      )

instance ToPath ListScripts where
  toPath = const "/"

instance ToQuery ListScripts where
  toQuery = const mempty

-- | /See:/ 'listScriptsResponse' smart constructor.
data ListScriptsResponse = ListScriptsResponse'
  { _lsrsScripts ::
      !(Maybe [Script]),
    _lsrsNextToken :: !(Maybe Text),
    _lsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListScriptsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsScripts' - A set of properties describing the requested script.
--
-- * 'lsrsNextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listScriptsResponse ::
  -- | 'lsrsResponseStatus'
  Int ->
  ListScriptsResponse
listScriptsResponse pResponseStatus_ =
  ListScriptsResponse'
    { _lsrsScripts = Nothing,
      _lsrsNextToken = Nothing,
      _lsrsResponseStatus = pResponseStatus_
    }

-- | A set of properties describing the requested script.
lsrsScripts :: Lens' ListScriptsResponse [Script]
lsrsScripts = lens _lsrsScripts (\s a -> s {_lsrsScripts = a}) . _Default . _Coerce

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
lsrsNextToken :: Lens' ListScriptsResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\s a -> s {_lsrsNextToken = a})

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListScriptsResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\s a -> s {_lsrsResponseStatus = a})

instance NFData ListScriptsResponse
