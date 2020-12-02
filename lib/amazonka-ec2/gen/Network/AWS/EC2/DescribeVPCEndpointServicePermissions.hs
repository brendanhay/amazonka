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
-- Module      : Network.AWS.EC2.DescribeVPCEndpointServicePermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the principals (service consumers) that are permitted to discover your VPC endpoint service.
--
--
module Network.AWS.EC2.DescribeVPCEndpointServicePermissions
    (
    -- * Creating a Request
      describeVPCEndpointServicePermissions
    , DescribeVPCEndpointServicePermissions
    -- * Request Lenses
    , dvespFilters
    , dvespNextToken
    , dvespDryRun
    , dvespMaxResults
    , dvespServiceId

    -- * Destructuring the Response
    , describeVPCEndpointServicePermissionsResponse
    , DescribeVPCEndpointServicePermissionsResponse
    -- * Response Lenses
    , dvesprsNextToken
    , dvesprsAllowedPrincipals
    , dvesprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPCEndpointServicePermissions' smart constructor.
data DescribeVPCEndpointServicePermissions = DescribeVPCEndpointServicePermissions'
  { _dvespFilters    :: !(Maybe [Filter])
  , _dvespNextToken  :: !(Maybe Text)
  , _dvespDryRun     :: !(Maybe Bool)
  , _dvespMaxResults :: !(Maybe Int)
  , _dvespServiceId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointServicePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvespFilters' - One or more filters.     * @principal@ - The ARN of the principal.     * @principal-type@ - The principal type (@All@ | @Service@ | @OrganizationUnit@ | @Account@ | @User@ | @Role@ ).
--
-- * 'dvespNextToken' - The token to retrieve the next page of results.
--
-- * 'dvespDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvespMaxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned.
--
-- * 'dvespServiceId' - The ID of the service.
describeVPCEndpointServicePermissions
    :: Text -- ^ 'dvespServiceId'
    -> DescribeVPCEndpointServicePermissions
describeVPCEndpointServicePermissions pServiceId_ =
  DescribeVPCEndpointServicePermissions'
    { _dvespFilters = Nothing
    , _dvespNextToken = Nothing
    , _dvespDryRun = Nothing
    , _dvespMaxResults = Nothing
    , _dvespServiceId = pServiceId_
    }


-- | One or more filters.     * @principal@ - The ARN of the principal.     * @principal-type@ - The principal type (@All@ | @Service@ | @OrganizationUnit@ | @Account@ | @User@ | @Role@ ).
dvespFilters :: Lens' DescribeVPCEndpointServicePermissions [Filter]
dvespFilters = lens _dvespFilters (\ s a -> s{_dvespFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dvespNextToken :: Lens' DescribeVPCEndpointServicePermissions (Maybe Text)
dvespNextToken = lens _dvespNextToken (\ s a -> s{_dvespNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvespDryRun :: Lens' DescribeVPCEndpointServicePermissions (Maybe Bool)
dvespDryRun = lens _dvespDryRun (\ s a -> s{_dvespDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned.
dvespMaxResults :: Lens' DescribeVPCEndpointServicePermissions (Maybe Int)
dvespMaxResults = lens _dvespMaxResults (\ s a -> s{_dvespMaxResults = a})

-- | The ID of the service.
dvespServiceId :: Lens' DescribeVPCEndpointServicePermissions Text
dvespServiceId = lens _dvespServiceId (\ s a -> s{_dvespServiceId = a})

instance AWSRequest
           DescribeVPCEndpointServicePermissions
         where
        type Rs DescribeVPCEndpointServicePermissions =
             DescribeVPCEndpointServicePermissionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCEndpointServicePermissionsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "allowedPrincipals" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeVPCEndpointServicePermissions
         where

instance NFData DescribeVPCEndpointServicePermissions
         where

instance ToHeaders
           DescribeVPCEndpointServicePermissions
         where
        toHeaders = const mempty

instance ToPath DescribeVPCEndpointServicePermissions
         where
        toPath = const "/"

instance ToQuery
           DescribeVPCEndpointServicePermissions
         where
        toQuery DescribeVPCEndpointServicePermissions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcEndpointServicePermissions" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvespFilters),
               "NextToken" =: _dvespNextToken,
               "DryRun" =: _dvespDryRun,
               "MaxResults" =: _dvespMaxResults,
               "ServiceId" =: _dvespServiceId]

-- | /See:/ 'describeVPCEndpointServicePermissionsResponse' smart constructor.
data DescribeVPCEndpointServicePermissionsResponse = DescribeVPCEndpointServicePermissionsResponse'
  { _dvesprsNextToken         :: !(Maybe Text)
  , _dvesprsAllowedPrincipals :: !(Maybe [AllowedPrincipal])
  , _dvesprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointServicePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvesprsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvesprsAllowedPrincipals' - Information about one or more allowed principals.
--
-- * 'dvesprsResponseStatus' - -- | The response status code.
describeVPCEndpointServicePermissionsResponse
    :: Int -- ^ 'dvesprsResponseStatus'
    -> DescribeVPCEndpointServicePermissionsResponse
describeVPCEndpointServicePermissionsResponse pResponseStatus_ =
  DescribeVPCEndpointServicePermissionsResponse'
    { _dvesprsNextToken = Nothing
    , _dvesprsAllowedPrincipals = Nothing
    , _dvesprsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvesprsNextToken :: Lens' DescribeVPCEndpointServicePermissionsResponse (Maybe Text)
dvesprsNextToken = lens _dvesprsNextToken (\ s a -> s{_dvesprsNextToken = a})

-- | Information about one or more allowed principals.
dvesprsAllowedPrincipals :: Lens' DescribeVPCEndpointServicePermissionsResponse [AllowedPrincipal]
dvesprsAllowedPrincipals = lens _dvesprsAllowedPrincipals (\ s a -> s{_dvesprsAllowedPrincipals = a}) . _Default . _Coerce

-- | -- | The response status code.
dvesprsResponseStatus :: Lens' DescribeVPCEndpointServicePermissionsResponse Int
dvesprsResponseStatus = lens _dvesprsResponseStatus (\ s a -> s{_dvesprsResponseStatus = a})

instance NFData
           DescribeVPCEndpointServicePermissionsResponse
         where
