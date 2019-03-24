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
-- Module      : Network.AWS.EC2.DescribeClientVPNAuthorizationRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the authorization rules for a specified Client VPN endpoint.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVPNAuthorizationRules
    (
    -- * Creating a Request
      describeClientVPNAuthorizationRules
    , DescribeClientVPNAuthorizationRules
    -- * Request Lenses
    , dcvarFilters
    , dcvarNextToken
    , dcvarDryRun
    , dcvarMaxResults
    , dcvarClientVPNEndpointId

    -- * Destructuring the Response
    , describeClientVPNAuthorizationRulesResponse
    , DescribeClientVPNAuthorizationRulesResponse
    -- * Response Lenses
    , dcvarrsAuthorizationRules
    , dcvarrsNextToken
    , dcvarrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClientVPNAuthorizationRules' smart constructor.
data DescribeClientVPNAuthorizationRules = DescribeClientVPNAuthorizationRules'
  { _dcvarFilters             :: !(Maybe [Filter])
  , _dcvarNextToken           :: !(Maybe Text)
  , _dcvarDryRun              :: !(Maybe Bool)
  , _dcvarMaxResults          :: !(Maybe Nat)
  , _dcvarClientVPNEndpointId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClientVPNAuthorizationRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvarFilters' - One or more filters. Filter names and values are case-sensitive.
--
-- * 'dcvarNextToken' - The token to retrieve the next page of results.
--
-- * 'dcvarDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dcvarMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- * 'dcvarClientVPNEndpointId' - The ID of the Client VPN endpoint.
describeClientVPNAuthorizationRules
    :: Text -- ^ 'dcvarClientVPNEndpointId'
    -> DescribeClientVPNAuthorizationRules
describeClientVPNAuthorizationRules pClientVPNEndpointId_ =
  DescribeClientVPNAuthorizationRules'
    { _dcvarFilters = Nothing
    , _dcvarNextToken = Nothing
    , _dcvarDryRun = Nothing
    , _dcvarMaxResults = Nothing
    , _dcvarClientVPNEndpointId = pClientVPNEndpointId_
    }


-- | One or more filters. Filter names and values are case-sensitive.
dcvarFilters :: Lens' DescribeClientVPNAuthorizationRules [Filter]
dcvarFilters = lens _dcvarFilters (\ s a -> s{_dcvarFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dcvarNextToken :: Lens' DescribeClientVPNAuthorizationRules (Maybe Text)
dcvarNextToken = lens _dcvarNextToken (\ s a -> s{_dcvarNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dcvarDryRun :: Lens' DescribeClientVPNAuthorizationRules (Maybe Bool)
dcvarDryRun = lens _dcvarDryRun (\ s a -> s{_dcvarDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
dcvarMaxResults :: Lens' DescribeClientVPNAuthorizationRules (Maybe Natural)
dcvarMaxResults = lens _dcvarMaxResults (\ s a -> s{_dcvarMaxResults = a}) . mapping _Nat

-- | The ID of the Client VPN endpoint.
dcvarClientVPNEndpointId :: Lens' DescribeClientVPNAuthorizationRules Text
dcvarClientVPNEndpointId = lens _dcvarClientVPNEndpointId (\ s a -> s{_dcvarClientVPNEndpointId = a})

instance AWSPager DescribeClientVPNAuthorizationRules
         where
        page rq rs
          | stop (rs ^. dcvarrsNextToken) = Nothing
          | stop (rs ^. dcvarrsAuthorizationRules) = Nothing
          | otherwise =
            Just $ rq & dcvarNextToken .~ rs ^. dcvarrsNextToken

instance AWSRequest
           DescribeClientVPNAuthorizationRules
         where
        type Rs DescribeClientVPNAuthorizationRules =
             DescribeClientVPNAuthorizationRulesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeClientVPNAuthorizationRulesResponse' <$>
                   (x .@? "authorizationRule" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClientVPNAuthorizationRules
         where

instance NFData DescribeClientVPNAuthorizationRules
         where

instance ToHeaders
           DescribeClientVPNAuthorizationRules
         where
        toHeaders = const mempty

instance ToPath DescribeClientVPNAuthorizationRules
         where
        toPath = const "/"

instance ToQuery DescribeClientVPNAuthorizationRules
         where
        toQuery DescribeClientVPNAuthorizationRules'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClientVpnAuthorizationRules" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dcvarFilters),
               "NextToken" =: _dcvarNextToken,
               "DryRun" =: _dcvarDryRun,
               "MaxResults" =: _dcvarMaxResults,
               "ClientVpnEndpointId" =: _dcvarClientVPNEndpointId]

-- | /See:/ 'describeClientVPNAuthorizationRulesResponse' smart constructor.
data DescribeClientVPNAuthorizationRulesResponse = DescribeClientVPNAuthorizationRulesResponse'
  { _dcvarrsAuthorizationRules :: !(Maybe [AuthorizationRule])
  , _dcvarrsNextToken          :: !(Maybe Text)
  , _dcvarrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClientVPNAuthorizationRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvarrsAuthorizationRules' - Information about the authorization rules.
--
-- * 'dcvarrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dcvarrsResponseStatus' - -- | The response status code.
describeClientVPNAuthorizationRulesResponse
    :: Int -- ^ 'dcvarrsResponseStatus'
    -> DescribeClientVPNAuthorizationRulesResponse
describeClientVPNAuthorizationRulesResponse pResponseStatus_ =
  DescribeClientVPNAuthorizationRulesResponse'
    { _dcvarrsAuthorizationRules = Nothing
    , _dcvarrsNextToken = Nothing
    , _dcvarrsResponseStatus = pResponseStatus_
    }


-- | Information about the authorization rules.
dcvarrsAuthorizationRules :: Lens' DescribeClientVPNAuthorizationRulesResponse [AuthorizationRule]
dcvarrsAuthorizationRules = lens _dcvarrsAuthorizationRules (\ s a -> s{_dcvarrsAuthorizationRules = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dcvarrsNextToken :: Lens' DescribeClientVPNAuthorizationRulesResponse (Maybe Text)
dcvarrsNextToken = lens _dcvarrsNextToken (\ s a -> s{_dcvarrsNextToken = a})

-- | -- | The response status code.
dcvarrsResponseStatus :: Lens' DescribeClientVPNAuthorizationRulesResponse Int
dcvarrsResponseStatus = lens _dcvarrsResponseStatus (\ s a -> s{_dcvarrsResponseStatus = a})

instance NFData
           DescribeClientVPNAuthorizationRulesResponse
         where
