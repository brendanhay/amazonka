{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVPNEndpoints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Client VPN endpoints in the account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVPNEndpoints
    (
    -- * Creating a Request
      describeClientVPNEndpoints
    , DescribeClientVPNEndpoints
    -- * Request Lenses
    , dcveFilters
    , dcveClientVPNEndpointIds
    , dcveNextToken
    , dcveDryRun
    , dcveMaxResults

    -- * Destructuring the Response
    , describeClientVPNEndpointsResponse
    , DescribeClientVPNEndpointsResponse
    -- * Response Lenses
    , dcversNextToken
    , dcversClientVPNEndpoints
    , dcversResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClientVPNEndpoints' smart constructor.
data DescribeClientVPNEndpoints = DescribeClientVPNEndpoints'
  { _dcveFilters              :: !(Maybe [Filter])
  , _dcveClientVPNEndpointIds :: !(Maybe [Text])
  , _dcveNextToken            :: !(Maybe Text)
  , _dcveDryRun               :: !(Maybe Bool)
  , _dcveMaxResults           :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClientVPNEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcveFilters' - One or more filters. Filter names and values are case-sensitive.
--
-- * 'dcveClientVPNEndpointIds' - The ID of the Client VPN endpoint.
--
-- * 'dcveNextToken' - The token to retrieve the next page of results.
--
-- * 'dcveDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dcveMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
describeClientVPNEndpoints
    :: DescribeClientVPNEndpoints
describeClientVPNEndpoints =
  DescribeClientVPNEndpoints'
    { _dcveFilters = Nothing
    , _dcveClientVPNEndpointIds = Nothing
    , _dcveNextToken = Nothing
    , _dcveDryRun = Nothing
    , _dcveMaxResults = Nothing
    }


-- | One or more filters. Filter names and values are case-sensitive.
dcveFilters :: Lens' DescribeClientVPNEndpoints [Filter]
dcveFilters = lens _dcveFilters (\ s a -> s{_dcveFilters = a}) . _Default . _Coerce

-- | The ID of the Client VPN endpoint.
dcveClientVPNEndpointIds :: Lens' DescribeClientVPNEndpoints [Text]
dcveClientVPNEndpointIds = lens _dcveClientVPNEndpointIds (\ s a -> s{_dcveClientVPNEndpointIds = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dcveNextToken :: Lens' DescribeClientVPNEndpoints (Maybe Text)
dcveNextToken = lens _dcveNextToken (\ s a -> s{_dcveNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dcveDryRun :: Lens' DescribeClientVPNEndpoints (Maybe Bool)
dcveDryRun = lens _dcveDryRun (\ s a -> s{_dcveDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
dcveMaxResults :: Lens' DescribeClientVPNEndpoints (Maybe Natural)
dcveMaxResults = lens _dcveMaxResults (\ s a -> s{_dcveMaxResults = a}) . mapping _Nat

instance AWSPager DescribeClientVPNEndpoints where
        page rq rs
          | stop (rs ^. dcversNextToken) = Nothing
          | stop (rs ^. dcversClientVPNEndpoints) = Nothing
          | otherwise =
            Just $ rq & dcveNextToken .~ rs ^. dcversNextToken

instance AWSRequest DescribeClientVPNEndpoints where
        type Rs DescribeClientVPNEndpoints =
             DescribeClientVPNEndpointsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeClientVPNEndpointsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "clientVpnEndpoint" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClientVPNEndpoints where

instance NFData DescribeClientVPNEndpoints where

instance ToHeaders DescribeClientVPNEndpoints where
        toHeaders = const mempty

instance ToPath DescribeClientVPNEndpoints where
        toPath = const "/"

instance ToQuery DescribeClientVPNEndpoints where
        toQuery DescribeClientVPNEndpoints'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClientVpnEndpoints" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dcveFilters),
               toQuery
                 (toQueryList "ClientVpnEndpointId" <$>
                    _dcveClientVPNEndpointIds),
               "NextToken" =: _dcveNextToken,
               "DryRun" =: _dcveDryRun,
               "MaxResults" =: _dcveMaxResults]

-- | /See:/ 'describeClientVPNEndpointsResponse' smart constructor.
data DescribeClientVPNEndpointsResponse = DescribeClientVPNEndpointsResponse'
  { _dcversNextToken          :: !(Maybe Text)
  , _dcversClientVPNEndpoints :: !(Maybe [ClientVPNEndpoint])
  , _dcversResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClientVPNEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcversNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dcversClientVPNEndpoints' - Information about the Client VPN endpoints.
--
-- * 'dcversResponseStatus' - -- | The response status code.
describeClientVPNEndpointsResponse
    :: Int -- ^ 'dcversResponseStatus'
    -> DescribeClientVPNEndpointsResponse
describeClientVPNEndpointsResponse pResponseStatus_ =
  DescribeClientVPNEndpointsResponse'
    { _dcversNextToken = Nothing
    , _dcversClientVPNEndpoints = Nothing
    , _dcversResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dcversNextToken :: Lens' DescribeClientVPNEndpointsResponse (Maybe Text)
dcversNextToken = lens _dcversNextToken (\ s a -> s{_dcversNextToken = a})

-- | Information about the Client VPN endpoints.
dcversClientVPNEndpoints :: Lens' DescribeClientVPNEndpointsResponse [ClientVPNEndpoint]
dcversClientVPNEndpoints = lens _dcversClientVPNEndpoints (\ s a -> s{_dcversClientVPNEndpoints = a}) . _Default . _Coerce

-- | -- | The response status code.
dcversResponseStatus :: Lens' DescribeClientVPNEndpointsResponse Int
dcversResponseStatus = lens _dcversResponseStatus (\ s a -> s{_dcversResponseStatus = a})

instance NFData DescribeClientVPNEndpointsResponse
         where
