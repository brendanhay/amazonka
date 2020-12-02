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
-- Module      : Network.AWS.EC2.DescribeManagedPrefixLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your managed prefix lists and any AWS-managed prefix lists.
--
--
-- To view the entries for your prefix list, use 'GetManagedPrefixListEntries' .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeManagedPrefixLists
  ( -- * Creating a Request
    describeManagedPrefixLists,
    DescribeManagedPrefixLists,

    -- * Request Lenses
    dmplsFilters,
    dmplsPrefixListIds,
    dmplsNextToken,
    dmplsDryRun,
    dmplsMaxResults,

    -- * Destructuring the Response
    describeManagedPrefixListsResponse,
    DescribeManagedPrefixListsResponse,

    -- * Response Lenses
    dmplsrsNextToken,
    dmplsrsPrefixLists,
    dmplsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeManagedPrefixLists' smart constructor.
data DescribeManagedPrefixLists = DescribeManagedPrefixLists'
  { _dmplsFilters ::
      !(Maybe [Filter]),
    _dmplsPrefixListIds ::
      !(Maybe [Text]),
    _dmplsNextToken :: !(Maybe Text),
    _dmplsDryRun :: !(Maybe Bool),
    _dmplsMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeManagedPrefixLists' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmplsFilters' - One or more filters.     * @owner-id@ - The ID of the prefix list owner.     * @prefix-list-id@ - The ID of the prefix list.     * @prefix-list-name@ - The name of the prefix list.
--
-- * 'dmplsPrefixListIds' - One or more prefix list IDs.
--
-- * 'dmplsNextToken' - The token for the next page of results.
--
-- * 'dmplsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dmplsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeManagedPrefixLists ::
  DescribeManagedPrefixLists
describeManagedPrefixLists =
  DescribeManagedPrefixLists'
    { _dmplsFilters = Nothing,
      _dmplsPrefixListIds = Nothing,
      _dmplsNextToken = Nothing,
      _dmplsDryRun = Nothing,
      _dmplsMaxResults = Nothing
    }

-- | One or more filters.     * @owner-id@ - The ID of the prefix list owner.     * @prefix-list-id@ - The ID of the prefix list.     * @prefix-list-name@ - The name of the prefix list.
dmplsFilters :: Lens' DescribeManagedPrefixLists [Filter]
dmplsFilters = lens _dmplsFilters (\s a -> s {_dmplsFilters = a}) . _Default . _Coerce

-- | One or more prefix list IDs.
dmplsPrefixListIds :: Lens' DescribeManagedPrefixLists [Text]
dmplsPrefixListIds = lens _dmplsPrefixListIds (\s a -> s {_dmplsPrefixListIds = a}) . _Default . _Coerce

-- | The token for the next page of results.
dmplsNextToken :: Lens' DescribeManagedPrefixLists (Maybe Text)
dmplsNextToken = lens _dmplsNextToken (\s a -> s {_dmplsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dmplsDryRun :: Lens' DescribeManagedPrefixLists (Maybe Bool)
dmplsDryRun = lens _dmplsDryRun (\s a -> s {_dmplsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dmplsMaxResults :: Lens' DescribeManagedPrefixLists (Maybe Natural)
dmplsMaxResults = lens _dmplsMaxResults (\s a -> s {_dmplsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeManagedPrefixLists where
  page rq rs
    | stop (rs ^. dmplsrsNextToken) = Nothing
    | stop (rs ^. dmplsrsPrefixLists) = Nothing
    | otherwise = Just $ rq & dmplsNextToken .~ rs ^. dmplsrsNextToken

instance AWSRequest DescribeManagedPrefixLists where
  type
    Rs DescribeManagedPrefixLists =
      DescribeManagedPrefixListsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeManagedPrefixListsResponse'
            <$> (x .@? "nextToken")
            <*> (x .@? "prefixListSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeManagedPrefixLists

instance NFData DescribeManagedPrefixLists

instance ToHeaders DescribeManagedPrefixLists where
  toHeaders = const mempty

instance ToPath DescribeManagedPrefixLists where
  toPath = const "/"

instance ToQuery DescribeManagedPrefixLists where
  toQuery DescribeManagedPrefixLists' {..} =
    mconcat
      [ "Action" =: ("DescribeManagedPrefixLists" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dmplsFilters),
        toQuery (toQueryList "PrefixListId" <$> _dmplsPrefixListIds),
        "NextToken" =: _dmplsNextToken,
        "DryRun" =: _dmplsDryRun,
        "MaxResults" =: _dmplsMaxResults
      ]

-- | /See:/ 'describeManagedPrefixListsResponse' smart constructor.
data DescribeManagedPrefixListsResponse = DescribeManagedPrefixListsResponse'
  { _dmplsrsNextToken ::
      !(Maybe Text),
    _dmplsrsPrefixLists ::
      !( Maybe
           [ManagedPrefixList]
       ),
    _dmplsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeManagedPrefixListsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmplsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dmplsrsPrefixLists' - Information about the prefix lists.
--
-- * 'dmplsrsResponseStatus' - -- | The response status code.
describeManagedPrefixListsResponse ::
  -- | 'dmplsrsResponseStatus'
  Int ->
  DescribeManagedPrefixListsResponse
describeManagedPrefixListsResponse pResponseStatus_ =
  DescribeManagedPrefixListsResponse'
    { _dmplsrsNextToken = Nothing,
      _dmplsrsPrefixLists = Nothing,
      _dmplsrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dmplsrsNextToken :: Lens' DescribeManagedPrefixListsResponse (Maybe Text)
dmplsrsNextToken = lens _dmplsrsNextToken (\s a -> s {_dmplsrsNextToken = a})

-- | Information about the prefix lists.
dmplsrsPrefixLists :: Lens' DescribeManagedPrefixListsResponse [ManagedPrefixList]
dmplsrsPrefixLists = lens _dmplsrsPrefixLists (\s a -> s {_dmplsrsPrefixLists = a}) . _Default . _Coerce

-- | -- | The response status code.
dmplsrsResponseStatus :: Lens' DescribeManagedPrefixListsResponse Int
dmplsrsResponseStatus = lens _dmplsrsResponseStatus (\s a -> s {_dmplsrsResponseStatus = a})

instance NFData DescribeManagedPrefixListsResponse
