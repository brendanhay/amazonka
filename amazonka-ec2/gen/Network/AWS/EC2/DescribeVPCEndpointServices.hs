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
-- Module      : Network.AWS.EC2.DescribeVPCEndpointServices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes available services to which you can create a VPC endpoint.
--
--
module Network.AWS.EC2.DescribeVPCEndpointServices
    (
    -- * Creating a Request
      describeVPCEndpointServices
    , DescribeVPCEndpointServices
    -- * Request Lenses
    , dvesFilters
    , dvesServiceNames
    , dvesNextToken
    , dvesDryRun
    , dvesMaxResults

    -- * Destructuring the Response
    , describeVPCEndpointServicesResponse
    , DescribeVPCEndpointServicesResponse
    -- * Response Lenses
    , dvesrsServiceDetails
    , dvesrsServiceNames
    , dvesrsNextToken
    , dvesrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeVpcEndpointServices.
--
--
--
-- /See:/ 'describeVPCEndpointServices' smart constructor.
data DescribeVPCEndpointServices = DescribeVPCEndpointServices'
  { _dvesFilters      :: !(Maybe [Filter])
  , _dvesServiceNames :: !(Maybe [Text])
  , _dvesNextToken    :: !(Maybe Text)
  , _dvesDryRun       :: !(Maybe Bool)
  , _dvesMaxResults   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvesFilters' - One or more filters.     * @service-name@ : The name of the service.
--
-- * 'dvesServiceNames' - One or more service names.
--
-- * 'dvesNextToken' - The token for the next set of items to return. (You received this token from a prior call.)
--
-- * 'dvesDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvesMaxResults' - The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. Constraint: If the value is greater than 1000, we return only 1000 items.
describeVPCEndpointServices
    :: DescribeVPCEndpointServices
describeVPCEndpointServices =
  DescribeVPCEndpointServices'
    { _dvesFilters = Nothing
    , _dvesServiceNames = Nothing
    , _dvesNextToken = Nothing
    , _dvesDryRun = Nothing
    , _dvesMaxResults = Nothing
    }


-- | One or more filters.     * @service-name@ : The name of the service.
dvesFilters :: Lens' DescribeVPCEndpointServices [Filter]
dvesFilters = lens _dvesFilters (\ s a -> s{_dvesFilters = a}) . _Default . _Coerce

-- | One or more service names.
dvesServiceNames :: Lens' DescribeVPCEndpointServices [Text]
dvesServiceNames = lens _dvesServiceNames (\ s a -> s{_dvesServiceNames = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a prior call.)
dvesNextToken :: Lens' DescribeVPCEndpointServices (Maybe Text)
dvesNextToken = lens _dvesNextToken (\ s a -> s{_dvesNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvesDryRun :: Lens' DescribeVPCEndpointServices (Maybe Bool)
dvesDryRun = lens _dvesDryRun (\ s a -> s{_dvesDryRun = a})

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. Constraint: If the value is greater than 1000, we return only 1000 items.
dvesMaxResults :: Lens' DescribeVPCEndpointServices (Maybe Int)
dvesMaxResults = lens _dvesMaxResults (\ s a -> s{_dvesMaxResults = a})

instance AWSRequest DescribeVPCEndpointServices where
        type Rs DescribeVPCEndpointServices =
             DescribeVPCEndpointServicesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCEndpointServicesResponse' <$>
                   (x .@? "serviceDetailSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*>
                     (x .@? "serviceNameSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVPCEndpointServices where

instance NFData DescribeVPCEndpointServices where

instance ToHeaders DescribeVPCEndpointServices where
        toHeaders = const mempty

instance ToPath DescribeVPCEndpointServices where
        toPath = const "/"

instance ToQuery DescribeVPCEndpointServices where
        toQuery DescribeVPCEndpointServices'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcEndpointServices" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvesFilters),
               toQuery
                 (toQueryList "ServiceName" <$> _dvesServiceNames),
               "NextToken" =: _dvesNextToken,
               "DryRun" =: _dvesDryRun,
               "MaxResults" =: _dvesMaxResults]

-- | Contains the output of DescribeVpcEndpointServices.
--
--
--
-- /See:/ 'describeVPCEndpointServicesResponse' smart constructor.
data DescribeVPCEndpointServicesResponse = DescribeVPCEndpointServicesResponse'
  { _dvesrsServiceDetails :: !(Maybe [ServiceDetail])
  , _dvesrsServiceNames   :: !(Maybe [Text])
  , _dvesrsNextToken      :: !(Maybe Text)
  , _dvesrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvesrsServiceDetails' - Information about the service.
--
-- * 'dvesrsServiceNames' - A list of supported services.
--
-- * 'dvesrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dvesrsResponseStatus' - -- | The response status code.
describeVPCEndpointServicesResponse
    :: Int -- ^ 'dvesrsResponseStatus'
    -> DescribeVPCEndpointServicesResponse
describeVPCEndpointServicesResponse pResponseStatus_ =
  DescribeVPCEndpointServicesResponse'
    { _dvesrsServiceDetails = Nothing
    , _dvesrsServiceNames = Nothing
    , _dvesrsNextToken = Nothing
    , _dvesrsResponseStatus = pResponseStatus_
    }


-- | Information about the service.
dvesrsServiceDetails :: Lens' DescribeVPCEndpointServicesResponse [ServiceDetail]
dvesrsServiceDetails = lens _dvesrsServiceDetails (\ s a -> s{_dvesrsServiceDetails = a}) . _Default . _Coerce

-- | A list of supported services.
dvesrsServiceNames :: Lens' DescribeVPCEndpointServicesResponse [Text]
dvesrsServiceNames = lens _dvesrsServiceNames (\ s a -> s{_dvesrsServiceNames = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dvesrsNextToken :: Lens' DescribeVPCEndpointServicesResponse (Maybe Text)
dvesrsNextToken = lens _dvesrsNextToken (\ s a -> s{_dvesrsNextToken = a})

-- | -- | The response status code.
dvesrsResponseStatus :: Lens' DescribeVPCEndpointServicesResponse Int
dvesrsResponseStatus = lens _dvesrsResponseStatus (\ s a -> s{_dvesrsResponseStatus = a})

instance NFData DescribeVPCEndpointServicesResponse
         where
