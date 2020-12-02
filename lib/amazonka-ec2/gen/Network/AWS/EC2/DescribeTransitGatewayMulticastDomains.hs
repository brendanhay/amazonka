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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway multicast domains.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
  ( -- * Creating a Request
    describeTransitGatewayMulticastDomains,
    DescribeTransitGatewayMulticastDomains,

    -- * Request Lenses
    dtgmdsTransitGatewayMulticastDomainIds,
    dtgmdsFilters,
    dtgmdsNextToken,
    dtgmdsDryRun,
    dtgmdsMaxResults,

    -- * Destructuring the Response
    describeTransitGatewayMulticastDomainsResponse,
    DescribeTransitGatewayMulticastDomainsResponse,

    -- * Response Lenses
    dtgmdsrsTransitGatewayMulticastDomains,
    dtgmdsrsNextToken,
    dtgmdsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTransitGatewayMulticastDomains' smart constructor.
data DescribeTransitGatewayMulticastDomains = DescribeTransitGatewayMulticastDomains'
  { _dtgmdsTransitGatewayMulticastDomainIds ::
      !( Maybe
           [Text]
       ),
    _dtgmdsFilters ::
      !( Maybe
           [Filter]
       ),
    _dtgmdsNextToken ::
      !(Maybe Text),
    _dtgmdsDryRun ::
      !(Maybe Bool),
    _dtgmdsMaxResults ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTransitGatewayMulticastDomains' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgmdsTransitGatewayMulticastDomainIds' - The ID of the transit gateway multicast domain.
--
-- * 'dtgmdsFilters' - One or more filters. The possible values are:     * @state@ - The state of the transit gateway multicast domain. Valid values are @pending@ | @available@ | @deleting@ | @deleted@ .     * @transit-gateway-id@ - The ID of the transit gateway.     * @transit-gateway-multicast-domain-id@ - The ID of the transit gateway multicast domain.
--
-- * 'dtgmdsNextToken' - The token for the next page of results.
--
-- * 'dtgmdsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgmdsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTransitGatewayMulticastDomains ::
  DescribeTransitGatewayMulticastDomains
describeTransitGatewayMulticastDomains =
  DescribeTransitGatewayMulticastDomains'
    { _dtgmdsTransitGatewayMulticastDomainIds =
        Nothing,
      _dtgmdsFilters = Nothing,
      _dtgmdsNextToken = Nothing,
      _dtgmdsDryRun = Nothing,
      _dtgmdsMaxResults = Nothing
    }

-- | The ID of the transit gateway multicast domain.
dtgmdsTransitGatewayMulticastDomainIds :: Lens' DescribeTransitGatewayMulticastDomains [Text]
dtgmdsTransitGatewayMulticastDomainIds = lens _dtgmdsTransitGatewayMulticastDomainIds (\s a -> s {_dtgmdsTransitGatewayMulticastDomainIds = a}) . _Default . _Coerce

-- | One or more filters. The possible values are:     * @state@ - The state of the transit gateway multicast domain. Valid values are @pending@ | @available@ | @deleting@ | @deleted@ .     * @transit-gateway-id@ - The ID of the transit gateway.     * @transit-gateway-multicast-domain-id@ - The ID of the transit gateway multicast domain.
dtgmdsFilters :: Lens' DescribeTransitGatewayMulticastDomains [Filter]
dtgmdsFilters = lens _dtgmdsFilters (\s a -> s {_dtgmdsFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtgmdsNextToken :: Lens' DescribeTransitGatewayMulticastDomains (Maybe Text)
dtgmdsNextToken = lens _dtgmdsNextToken (\s a -> s {_dtgmdsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgmdsDryRun :: Lens' DescribeTransitGatewayMulticastDomains (Maybe Bool)
dtgmdsDryRun = lens _dtgmdsDryRun (\s a -> s {_dtgmdsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtgmdsMaxResults :: Lens' DescribeTransitGatewayMulticastDomains (Maybe Natural)
dtgmdsMaxResults = lens _dtgmdsMaxResults (\s a -> s {_dtgmdsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTransitGatewayMulticastDomains where
  page rq rs
    | stop (rs ^. dtgmdsrsNextToken) = Nothing
    | stop (rs ^. dtgmdsrsTransitGatewayMulticastDomains) = Nothing
    | otherwise =
      Just $ rq & dtgmdsNextToken .~ rs ^. dtgmdsrsNextToken

instance AWSRequest DescribeTransitGatewayMulticastDomains where
  type
    Rs DescribeTransitGatewayMulticastDomains =
      DescribeTransitGatewayMulticastDomainsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeTransitGatewayMulticastDomainsResponse'
            <$> ( x .@? "transitGatewayMulticastDomains" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTransitGatewayMulticastDomains

instance NFData DescribeTransitGatewayMulticastDomains

instance ToHeaders DescribeTransitGatewayMulticastDomains where
  toHeaders = const mempty

instance ToPath DescribeTransitGatewayMulticastDomains where
  toPath = const "/"

instance ToQuery DescribeTransitGatewayMulticastDomains where
  toQuery DescribeTransitGatewayMulticastDomains' {..} =
    mconcat
      [ "Action"
          =: ("DescribeTransitGatewayMulticastDomains" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          ( toQueryList "TransitGatewayMulticastDomainIds"
              <$> _dtgmdsTransitGatewayMulticastDomainIds
          ),
        toQuery (toQueryList "Filter" <$> _dtgmdsFilters),
        "NextToken" =: _dtgmdsNextToken,
        "DryRun" =: _dtgmdsDryRun,
        "MaxResults" =: _dtgmdsMaxResults
      ]

-- | /See:/ 'describeTransitGatewayMulticastDomainsResponse' smart constructor.
data DescribeTransitGatewayMulticastDomainsResponse = DescribeTransitGatewayMulticastDomainsResponse'
  { _dtgmdsrsTransitGatewayMulticastDomains ::
      !( Maybe
           [TransitGatewayMulticastDomain]
       ),
    _dtgmdsrsNextToken ::
      !( Maybe
           Text
       ),
    _dtgmdsrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeTransitGatewayMulticastDomainsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgmdsrsTransitGatewayMulticastDomains' - Information about the transit gateway multicast domains.
--
-- * 'dtgmdsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtgmdsrsResponseStatus' - -- | The response status code.
describeTransitGatewayMulticastDomainsResponse ::
  -- | 'dtgmdsrsResponseStatus'
  Int ->
  DescribeTransitGatewayMulticastDomainsResponse
describeTransitGatewayMulticastDomainsResponse pResponseStatus_ =
  DescribeTransitGatewayMulticastDomainsResponse'
    { _dtgmdsrsTransitGatewayMulticastDomains =
        Nothing,
      _dtgmdsrsNextToken = Nothing,
      _dtgmdsrsResponseStatus = pResponseStatus_
    }

-- | Information about the transit gateway multicast domains.
dtgmdsrsTransitGatewayMulticastDomains :: Lens' DescribeTransitGatewayMulticastDomainsResponse [TransitGatewayMulticastDomain]
dtgmdsrsTransitGatewayMulticastDomains = lens _dtgmdsrsTransitGatewayMulticastDomains (\s a -> s {_dtgmdsrsTransitGatewayMulticastDomains = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtgmdsrsNextToken :: Lens' DescribeTransitGatewayMulticastDomainsResponse (Maybe Text)
dtgmdsrsNextToken = lens _dtgmdsrsNextToken (\s a -> s {_dtgmdsrsNextToken = a})

-- | -- | The response status code.
dtgmdsrsResponseStatus :: Lens' DescribeTransitGatewayMulticastDomainsResponse Int
dtgmdsrsResponseStatus = lens _dtgmdsrsResponseStatus (\s a -> s {_dtgmdsrsResponseStatus = a})

instance NFData DescribeTransitGatewayMulticastDomainsResponse
