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
-- Module      : Network.AWS.EC2.DescribeVPCEndpointServiceConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint service configurations in your account (your services).
--
--
module Network.AWS.EC2.DescribeVPCEndpointServiceConfigurations
    (
    -- * Creating a Request
      describeVPCEndpointServiceConfigurations
    , DescribeVPCEndpointServiceConfigurations
    -- * Request Lenses
    , dvescFilters
    , dvescServiceIds
    , dvescNextToken
    , dvescDryRun
    , dvescMaxResults

    -- * Destructuring the Response
    , describeVPCEndpointServiceConfigurationsResponse
    , DescribeVPCEndpointServiceConfigurationsResponse
    -- * Response Lenses
    , dvescrsNextToken
    , dvescrsServiceConfigurations
    , dvescrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPCEndpointServiceConfigurations' smart constructor.
data DescribeVPCEndpointServiceConfigurations = DescribeVPCEndpointServiceConfigurations'
  { _dvescFilters    :: !(Maybe [Filter])
  , _dvescServiceIds :: !(Maybe [Text])
  , _dvescNextToken  :: !(Maybe Text)
  , _dvescDryRun     :: !(Maybe Bool)
  , _dvescMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointServiceConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvescFilters' - One or more filters.     * @service-name@ - The name of the service.     * @service-id@ - The ID of the service.     * @service-state@ - The state of the service (@Pending@ | @Available@ | @Deleting@ | @Deleted@ | @Failed@ ).
--
-- * 'dvescServiceIds' - The IDs of one or more services.
--
-- * 'dvescNextToken' - The token to retrieve the next page of results.
--
-- * 'dvescDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvescMaxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned.
describeVPCEndpointServiceConfigurations
    :: DescribeVPCEndpointServiceConfigurations
describeVPCEndpointServiceConfigurations =
  DescribeVPCEndpointServiceConfigurations'
    { _dvescFilters = Nothing
    , _dvescServiceIds = Nothing
    , _dvescNextToken = Nothing
    , _dvescDryRun = Nothing
    , _dvescMaxResults = Nothing
    }


-- | One or more filters.     * @service-name@ - The name of the service.     * @service-id@ - The ID of the service.     * @service-state@ - The state of the service (@Pending@ | @Available@ | @Deleting@ | @Deleted@ | @Failed@ ).
dvescFilters :: Lens' DescribeVPCEndpointServiceConfigurations [Filter]
dvescFilters = lens _dvescFilters (\ s a -> s{_dvescFilters = a}) . _Default . _Coerce

-- | The IDs of one or more services.
dvescServiceIds :: Lens' DescribeVPCEndpointServiceConfigurations [Text]
dvescServiceIds = lens _dvescServiceIds (\ s a -> s{_dvescServiceIds = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dvescNextToken :: Lens' DescribeVPCEndpointServiceConfigurations (Maybe Text)
dvescNextToken = lens _dvescNextToken (\ s a -> s{_dvescNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvescDryRun :: Lens' DescribeVPCEndpointServiceConfigurations (Maybe Bool)
dvescDryRun = lens _dvescDryRun (\ s a -> s{_dvescDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned.
dvescMaxResults :: Lens' DescribeVPCEndpointServiceConfigurations (Maybe Int)
dvescMaxResults = lens _dvescMaxResults (\ s a -> s{_dvescMaxResults = a})

instance AWSRequest
           DescribeVPCEndpointServiceConfigurations
         where
        type Rs DescribeVPCEndpointServiceConfigurations =
             DescribeVPCEndpointServiceConfigurationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCEndpointServiceConfigurationsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "serviceConfigurationSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeVPCEndpointServiceConfigurations
         where

instance NFData
           DescribeVPCEndpointServiceConfigurations
         where

instance ToHeaders
           DescribeVPCEndpointServiceConfigurations
         where
        toHeaders = const mempty

instance ToPath
           DescribeVPCEndpointServiceConfigurations
         where
        toPath = const "/"

instance ToQuery
           DescribeVPCEndpointServiceConfigurations
         where
        toQuery DescribeVPCEndpointServiceConfigurations'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcEndpointServiceConfigurations" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvescFilters),
               toQuery
                 (toQueryList "ServiceId" <$> _dvescServiceIds),
               "NextToken" =: _dvescNextToken,
               "DryRun" =: _dvescDryRun,
               "MaxResults" =: _dvescMaxResults]

-- | /See:/ 'describeVPCEndpointServiceConfigurationsResponse' smart constructor.
data DescribeVPCEndpointServiceConfigurationsResponse = DescribeVPCEndpointServiceConfigurationsResponse'
  { _dvescrsNextToken             :: !(Maybe Text)
  , _dvescrsServiceConfigurations :: !(Maybe [ServiceConfiguration])
  , _dvescrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointServiceConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvescrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvescrsServiceConfigurations' - Information about one or more services.
--
-- * 'dvescrsResponseStatus' - -- | The response status code.
describeVPCEndpointServiceConfigurationsResponse
    :: Int -- ^ 'dvescrsResponseStatus'
    -> DescribeVPCEndpointServiceConfigurationsResponse
describeVPCEndpointServiceConfigurationsResponse pResponseStatus_ =
  DescribeVPCEndpointServiceConfigurationsResponse'
    { _dvescrsNextToken = Nothing
    , _dvescrsServiceConfigurations = Nothing
    , _dvescrsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvescrsNextToken :: Lens' DescribeVPCEndpointServiceConfigurationsResponse (Maybe Text)
dvescrsNextToken = lens _dvescrsNextToken (\ s a -> s{_dvescrsNextToken = a})

-- | Information about one or more services.
dvescrsServiceConfigurations :: Lens' DescribeVPCEndpointServiceConfigurationsResponse [ServiceConfiguration]
dvescrsServiceConfigurations = lens _dvescrsServiceConfigurations (\ s a -> s{_dvescrsServiceConfigurations = a}) . _Default . _Coerce

-- | -- | The response status code.
dvescrsResponseStatus :: Lens' DescribeVPCEndpointServiceConfigurationsResponse Int
dvescrsResponseStatus = lens _dvescrsResponseStatus (\ s a -> s{_dvescrsResponseStatus = a})

instance NFData
           DescribeVPCEndpointServiceConfigurationsResponse
         where
