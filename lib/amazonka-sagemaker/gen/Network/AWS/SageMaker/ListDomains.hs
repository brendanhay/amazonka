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
-- Module      : Network.AWS.SageMaker.ListDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the domains.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListDomains
  ( -- * Creating a Request
    listDomains,
    ListDomains,

    -- * Request Lenses
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the Response
    listDomainsResponse,
    ListDomainsResponse,

    -- * Response Lenses
    ldrsNextToken,
    ldrsDomains,
    ldrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listDomains' smart constructor.
data ListDomains = ListDomains'
  { _ldNextToken :: !(Maybe Text),
    _ldMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDomains' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldNextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- * 'ldMaxResults' - Returns a list up to a specified limit.
listDomains ::
  ListDomains
listDomains =
  ListDomains' {_ldNextToken = Nothing, _ldMaxResults = Nothing}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
ldNextToken :: Lens' ListDomains (Maybe Text)
ldNextToken = lens _ldNextToken (\s a -> s {_ldNextToken = a})

-- | Returns a list up to a specified limit.
ldMaxResults :: Lens' ListDomains (Maybe Natural)
ldMaxResults = lens _ldMaxResults (\s a -> s {_ldMaxResults = a}) . mapping _Nat

instance AWSPager ListDomains where
  page rq rs
    | stop (rs ^. ldrsNextToken) = Nothing
    | stop (rs ^. ldrsDomains) = Nothing
    | otherwise = Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDomains where
  type Rs ListDomains = ListDomainsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Domains" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListDomains

instance NFData ListDomains

instance ToHeaders ListDomains where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListDomains" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListDomains where
  toJSON ListDomains' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _ldNextToken,
            ("MaxResults" .=) <$> _ldMaxResults
          ]
      )

instance ToPath ListDomains where
  toPath = const "/"

instance ToQuery ListDomains where
  toQuery = const mempty

-- | /See:/ 'listDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { _ldrsNextToken ::
      !(Maybe Text),
    _ldrsDomains :: !(Maybe [DomainDetails]),
    _ldrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDomainsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- * 'ldrsDomains' - The list of domains.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDomainsResponse ::
  -- | 'ldrsResponseStatus'
  Int ->
  ListDomainsResponse
listDomainsResponse pResponseStatus_ =
  ListDomainsResponse'
    { _ldrsNextToken = Nothing,
      _ldrsDomains = Nothing,
      _ldrsResponseStatus = pResponseStatus_
    }

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
ldrsNextToken :: Lens' ListDomainsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\s a -> s {_ldrsNextToken = a})

-- | The list of domains.
ldrsDomains :: Lens' ListDomainsResponse [DomainDetails]
ldrsDomains = lens _ldrsDomains (\s a -> s {_ldrsDomains = a}) . _Default . _Coerce

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDomainsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\s a -> s {_ldrsResponseStatus = a})

instance NFData ListDomainsResponse
