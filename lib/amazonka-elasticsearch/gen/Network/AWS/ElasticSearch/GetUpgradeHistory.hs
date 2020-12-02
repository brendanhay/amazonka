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
-- Module      : Network.AWS.ElasticSearch.GetUpgradeHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the complete history of the last 10 upgrades that were performed on the domain.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.GetUpgradeHistory
  ( -- * Creating a Request
    getUpgradeHistory,
    GetUpgradeHistory,

    -- * Request Lenses
    guhNextToken,
    guhMaxResults,
    guhDomainName,

    -- * Destructuring the Response
    getUpgradeHistoryResponse,
    GetUpgradeHistoryResponse,

    -- * Response Lenses
    guhrsNextToken,
    guhrsUpgradeHistories,
    guhrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'GetUpgradeHistory' @ operation.
--
--
--
-- /See:/ 'getUpgradeHistory' smart constructor.
data GetUpgradeHistory = GetUpgradeHistory'
  { _guhNextToken ::
      !(Maybe Text),
    _guhMaxResults :: !(Maybe Int),
    _guhDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetUpgradeHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guhNextToken' - Undocumented member.
--
-- * 'guhMaxResults' - Undocumented member.
--
-- * 'guhDomainName' - Undocumented member.
getUpgradeHistory ::
  -- | 'guhDomainName'
  Text ->
  GetUpgradeHistory
getUpgradeHistory pDomainName_ =
  GetUpgradeHistory'
    { _guhNextToken = Nothing,
      _guhMaxResults = Nothing,
      _guhDomainName = pDomainName_
    }

-- | Undocumented member.
guhNextToken :: Lens' GetUpgradeHistory (Maybe Text)
guhNextToken = lens _guhNextToken (\s a -> s {_guhNextToken = a})

-- | Undocumented member.
guhMaxResults :: Lens' GetUpgradeHistory (Maybe Int)
guhMaxResults = lens _guhMaxResults (\s a -> s {_guhMaxResults = a})

-- | Undocumented member.
guhDomainName :: Lens' GetUpgradeHistory Text
guhDomainName = lens _guhDomainName (\s a -> s {_guhDomainName = a})

instance AWSPager GetUpgradeHistory where
  page rq rs
    | stop (rs ^. guhrsNextToken) = Nothing
    | stop (rs ^. guhrsUpgradeHistories) = Nothing
    | otherwise = Just $ rq & guhNextToken .~ rs ^. guhrsNextToken

instance AWSRequest GetUpgradeHistory where
  type Rs GetUpgradeHistory = GetUpgradeHistoryResponse
  request = get elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          GetUpgradeHistoryResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "UpgradeHistories" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetUpgradeHistory

instance NFData GetUpgradeHistory

instance ToHeaders GetUpgradeHistory where
  toHeaders = const mempty

instance ToPath GetUpgradeHistory where
  toPath GetUpgradeHistory' {..} =
    mconcat
      ["/2015-01-01/es/upgradeDomain/", toBS _guhDomainName, "/history"]

instance ToQuery GetUpgradeHistory where
  toQuery GetUpgradeHistory' {..} =
    mconcat
      ["nextToken" =: _guhNextToken, "maxResults" =: _guhMaxResults]

-- | Container for response returned by @'GetUpgradeHistory' @ operation.
--
--
--
-- /See:/ 'getUpgradeHistoryResponse' smart constructor.
data GetUpgradeHistoryResponse = GetUpgradeHistoryResponse'
  { _guhrsNextToken ::
      !(Maybe Text),
    _guhrsUpgradeHistories ::
      !(Maybe [UpgradeHistory]),
    _guhrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetUpgradeHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guhrsNextToken' - Pagination token that needs to be supplied to the next call to get the next page of results
--
-- * 'guhrsUpgradeHistories' - A list of @'UpgradeHistory' @ objects corresponding to each Upgrade or Upgrade Eligibility Check performed on a domain returned as part of @'GetUpgradeHistoryResponse' @ object.
--
-- * 'guhrsResponseStatus' - -- | The response status code.
getUpgradeHistoryResponse ::
  -- | 'guhrsResponseStatus'
  Int ->
  GetUpgradeHistoryResponse
getUpgradeHistoryResponse pResponseStatus_ =
  GetUpgradeHistoryResponse'
    { _guhrsNextToken = Nothing,
      _guhrsUpgradeHistories = Nothing,
      _guhrsResponseStatus = pResponseStatus_
    }

-- | Pagination token that needs to be supplied to the next call to get the next page of results
guhrsNextToken :: Lens' GetUpgradeHistoryResponse (Maybe Text)
guhrsNextToken = lens _guhrsNextToken (\s a -> s {_guhrsNextToken = a})

-- | A list of @'UpgradeHistory' @ objects corresponding to each Upgrade or Upgrade Eligibility Check performed on a domain returned as part of @'GetUpgradeHistoryResponse' @ object.
guhrsUpgradeHistories :: Lens' GetUpgradeHistoryResponse [UpgradeHistory]
guhrsUpgradeHistories = lens _guhrsUpgradeHistories (\s a -> s {_guhrsUpgradeHistories = a}) . _Default . _Coerce

-- | -- | The response status code.
guhrsResponseStatus :: Lens' GetUpgradeHistoryResponse Int
guhrsResponseStatus = lens _guhrsResponseStatus (\s a -> s {_guhrsResponseStatus = a})

instance NFData GetUpgradeHistoryResponse
