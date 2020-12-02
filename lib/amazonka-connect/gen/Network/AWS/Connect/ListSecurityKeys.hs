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
-- Module      : Network.AWS.Connect.ListSecurityKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all security keys associated with the instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListSecurityKeys
  ( -- * Creating a Request
    listSecurityKeys,
    ListSecurityKeys,

    -- * Request Lenses
    lskNextToken,
    lskMaxResults,
    lskInstanceId,

    -- * Destructuring the Response
    listSecurityKeysResponse,
    ListSecurityKeysResponse,

    -- * Response Lenses
    lskrsNextToken,
    lskrsSecurityKeys,
    lskrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSecurityKeys' smart constructor.
data ListSecurityKeys = ListSecurityKeys'
  { _lskNextToken ::
      !(Maybe Text),
    _lskMaxResults :: !(Maybe Nat),
    _lskInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSecurityKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lskNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'lskMaxResults' - The maximimum number of results to return per page.
--
-- * 'lskInstanceId' - The identifier of the Amazon Connect instance.
listSecurityKeys ::
  -- | 'lskInstanceId'
  Text ->
  ListSecurityKeys
listSecurityKeys pInstanceId_ =
  ListSecurityKeys'
    { _lskNextToken = Nothing,
      _lskMaxResults = Nothing,
      _lskInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
lskNextToken :: Lens' ListSecurityKeys (Maybe Text)
lskNextToken = lens _lskNextToken (\s a -> s {_lskNextToken = a})

-- | The maximimum number of results to return per page.
lskMaxResults :: Lens' ListSecurityKeys (Maybe Natural)
lskMaxResults = lens _lskMaxResults (\s a -> s {_lskMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
lskInstanceId :: Lens' ListSecurityKeys Text
lskInstanceId = lens _lskInstanceId (\s a -> s {_lskInstanceId = a})

instance AWSPager ListSecurityKeys where
  page rq rs
    | stop (rs ^. lskrsNextToken) = Nothing
    | stop (rs ^. lskrsSecurityKeys) = Nothing
    | otherwise = Just $ rq & lskNextToken .~ rs ^. lskrsNextToken

instance AWSRequest ListSecurityKeys where
  type Rs ListSecurityKeys = ListSecurityKeysResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListSecurityKeysResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "SecurityKeys" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListSecurityKeys

instance NFData ListSecurityKeys

instance ToHeaders ListSecurityKeys where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListSecurityKeys where
  toPath ListSecurityKeys' {..} =
    mconcat ["/instance/", toBS _lskInstanceId, "/security-keys"]

instance ToQuery ListSecurityKeys where
  toQuery ListSecurityKeys' {..} =
    mconcat
      ["nextToken" =: _lskNextToken, "maxResults" =: _lskMaxResults]

-- | /See:/ 'listSecurityKeysResponse' smart constructor.
data ListSecurityKeysResponse = ListSecurityKeysResponse'
  { _lskrsNextToken ::
      !(Maybe Text),
    _lskrsSecurityKeys ::
      !(Maybe [SecurityKey]),
    _lskrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSecurityKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lskrsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'lskrsSecurityKeys' - The security keys.
--
-- * 'lskrsResponseStatus' - -- | The response status code.
listSecurityKeysResponse ::
  -- | 'lskrsResponseStatus'
  Int ->
  ListSecurityKeysResponse
listSecurityKeysResponse pResponseStatus_ =
  ListSecurityKeysResponse'
    { _lskrsNextToken = Nothing,
      _lskrsSecurityKeys = Nothing,
      _lskrsResponseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
lskrsNextToken :: Lens' ListSecurityKeysResponse (Maybe Text)
lskrsNextToken = lens _lskrsNextToken (\s a -> s {_lskrsNextToken = a})

-- | The security keys.
lskrsSecurityKeys :: Lens' ListSecurityKeysResponse [SecurityKey]
lskrsSecurityKeys = lens _lskrsSecurityKeys (\s a -> s {_lskrsSecurityKeys = a}) . _Default . _Coerce

-- | -- | The response status code.
lskrsResponseStatus :: Lens' ListSecurityKeysResponse Int
lskrsResponseStatus = lens _lskrsResponseStatus (\s a -> s {_lskrsResponseStatus = a})

instance NFData ListSecurityKeysResponse
