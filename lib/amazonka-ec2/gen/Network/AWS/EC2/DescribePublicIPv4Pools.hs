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
-- Module      : Network.AWS.EC2.DescribePublicIPv4Pools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified IPv4 address pools.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePublicIPv4Pools
  ( -- * Creating a Request
    describePublicIPv4Pools,
    DescribePublicIPv4Pools,

    -- * Request Lenses
    dpipPoolIds,
    dpipFilters,
    dpipNextToken,
    dpipMaxResults,

    -- * Destructuring the Response
    describePublicIPv4PoolsResponse,
    DescribePublicIPv4PoolsResponse,

    -- * Response Lenses
    dpiprsPublicIPv4Pools,
    dpiprsNextToken,
    dpiprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePublicIPv4Pools' smart constructor.
data DescribePublicIPv4Pools = DescribePublicIPv4Pools'
  { _dpipPoolIds ::
      !(Maybe [Text]),
    _dpipFilters :: !(Maybe [Filter]),
    _dpipNextToken :: !(Maybe Text),
    _dpipMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePublicIPv4Pools' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpipPoolIds' - The IDs of the address pools.
--
-- * 'dpipFilters' - One or more filters.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
-- * 'dpipNextToken' - The token for the next page of results.
--
-- * 'dpipMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describePublicIPv4Pools ::
  DescribePublicIPv4Pools
describePublicIPv4Pools =
  DescribePublicIPv4Pools'
    { _dpipPoolIds = Nothing,
      _dpipFilters = Nothing,
      _dpipNextToken = Nothing,
      _dpipMaxResults = Nothing
    }

-- | The IDs of the address pools.
dpipPoolIds :: Lens' DescribePublicIPv4Pools [Text]
dpipPoolIds = lens _dpipPoolIds (\s a -> s {_dpipPoolIds = a}) . _Default . _Coerce

-- | One or more filters.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
dpipFilters :: Lens' DescribePublicIPv4Pools [Filter]
dpipFilters = lens _dpipFilters (\s a -> s {_dpipFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dpipNextToken :: Lens' DescribePublicIPv4Pools (Maybe Text)
dpipNextToken = lens _dpipNextToken (\s a -> s {_dpipNextToken = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dpipMaxResults :: Lens' DescribePublicIPv4Pools (Maybe Natural)
dpipMaxResults = lens _dpipMaxResults (\s a -> s {_dpipMaxResults = a}) . mapping _Nat

instance AWSPager DescribePublicIPv4Pools where
  page rq rs
    | stop (rs ^. dpiprsNextToken) = Nothing
    | stop (rs ^. dpiprsPublicIPv4Pools) = Nothing
    | otherwise = Just $ rq & dpipNextToken .~ rs ^. dpiprsNextToken

instance AWSRequest DescribePublicIPv4Pools where
  type Rs DescribePublicIPv4Pools = DescribePublicIPv4PoolsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribePublicIPv4PoolsResponse'
            <$> ( x .@? "publicIpv4PoolSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribePublicIPv4Pools

instance NFData DescribePublicIPv4Pools

instance ToHeaders DescribePublicIPv4Pools where
  toHeaders = const mempty

instance ToPath DescribePublicIPv4Pools where
  toPath = const "/"

instance ToQuery DescribePublicIPv4Pools where
  toQuery DescribePublicIPv4Pools' {..} =
    mconcat
      [ "Action" =: ("DescribePublicIpv4Pools" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "PoolId" <$> _dpipPoolIds),
        toQuery (toQueryList "Filter" <$> _dpipFilters),
        "NextToken" =: _dpipNextToken,
        "MaxResults" =: _dpipMaxResults
      ]

-- | /See:/ 'describePublicIPv4PoolsResponse' smart constructor.
data DescribePublicIPv4PoolsResponse = DescribePublicIPv4PoolsResponse'
  { _dpiprsPublicIPv4Pools ::
      !(Maybe [PublicIPv4Pool]),
    _dpiprsNextToken ::
      !(Maybe Text),
    _dpiprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePublicIPv4PoolsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpiprsPublicIPv4Pools' - Information about the address pools.
--
-- * 'dpiprsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dpiprsResponseStatus' - -- | The response status code.
describePublicIPv4PoolsResponse ::
  -- | 'dpiprsResponseStatus'
  Int ->
  DescribePublicIPv4PoolsResponse
describePublicIPv4PoolsResponse pResponseStatus_ =
  DescribePublicIPv4PoolsResponse'
    { _dpiprsPublicIPv4Pools =
        Nothing,
      _dpiprsNextToken = Nothing,
      _dpiprsResponseStatus = pResponseStatus_
    }

-- | Information about the address pools.
dpiprsPublicIPv4Pools :: Lens' DescribePublicIPv4PoolsResponse [PublicIPv4Pool]
dpiprsPublicIPv4Pools = lens _dpiprsPublicIPv4Pools (\s a -> s {_dpiprsPublicIPv4Pools = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dpiprsNextToken :: Lens' DescribePublicIPv4PoolsResponse (Maybe Text)
dpiprsNextToken = lens _dpiprsNextToken (\s a -> s {_dpiprsNextToken = a})

-- | -- | The response status code.
dpiprsResponseStatus :: Lens' DescribePublicIPv4PoolsResponse Int
dpiprsResponseStatus = lens _dpiprsResponseStatus (\s a -> s {_dpiprsResponseStatus = a})

instance NFData DescribePublicIPv4PoolsResponse
