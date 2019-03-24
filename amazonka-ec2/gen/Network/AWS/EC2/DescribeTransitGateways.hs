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
-- Module      : Network.AWS.EC2.DescribeTransitGateways
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateways. By default, all transit gateways are described. Alternatively, you can filter the results.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGateways
    (
    -- * Creating a Request
      describeTransitGateways
    , DescribeTransitGateways
    -- * Request Lenses
    , dtgsFilters
    , dtgsTransitGatewayIds
    , dtgsNextToken
    , dtgsDryRun
    , dtgsMaxResults

    -- * Destructuring the Response
    , describeTransitGatewaysResponse
    , DescribeTransitGatewaysResponse
    -- * Response Lenses
    , dtgrsTransitGateways
    , dtgrsNextToken
    , dtgrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTransitGateways' smart constructor.
data DescribeTransitGateways = DescribeTransitGateways'
  { _dtgsFilters           :: !(Maybe [Filter])
  , _dtgsTransitGatewayIds :: !(Maybe [Text])
  , _dtgsNextToken         :: !(Maybe Text)
  , _dtgsDryRun            :: !(Maybe Bool)
  , _dtgsMaxResults        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransitGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgsFilters' - One or more filters. The possible values are:     * @options.propagation-default-route-table-id@ - The ID of the default propagation route table.     * @options.amazon-side-asn@ - The private ASN for the Amazon side of a BGP session.     * @options.association-default-route-table-id@ - The ID of the default association route table.     * @options.auto-accept-shared-attachments@ - Indicates whether there is automatic acceptance of attachment requests (@enable@ | @disable@ ).     * @options.default-route-table-association@ - Indicates whether resource attachments are automatically associated with the default association route table (@enable@ | @disable@ ).     * @options.default-route-table-propagation@ - Indicates whether resource attachments automatically propagate routes to the default propagation route table (@enable@ | @disable@ ).     * @options.dns-support@ - Indicates whether DNS support is enabled (@enable@ | @disable@ ).     * @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath Protocol support is enabled (@enable@ | @disable@ ).     * @owner-id@ - The ID of the AWS account that owns the transit gateway.     * @state@ - The state of the attachment (@available@ | @deleted@ | @deleting@ | @failed@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).     * @transit-gateway-id@ - The ID of the transit gateway.
--
-- * 'dtgsTransitGatewayIds' - The IDs of the transit gateways.
--
-- * 'dtgsNextToken' - The token for the next page of results.
--
-- * 'dtgsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTransitGateways
    :: DescribeTransitGateways
describeTransitGateways =
  DescribeTransitGateways'
    { _dtgsFilters = Nothing
    , _dtgsTransitGatewayIds = Nothing
    , _dtgsNextToken = Nothing
    , _dtgsDryRun = Nothing
    , _dtgsMaxResults = Nothing
    }


-- | One or more filters. The possible values are:     * @options.propagation-default-route-table-id@ - The ID of the default propagation route table.     * @options.amazon-side-asn@ - The private ASN for the Amazon side of a BGP session.     * @options.association-default-route-table-id@ - The ID of the default association route table.     * @options.auto-accept-shared-attachments@ - Indicates whether there is automatic acceptance of attachment requests (@enable@ | @disable@ ).     * @options.default-route-table-association@ - Indicates whether resource attachments are automatically associated with the default association route table (@enable@ | @disable@ ).     * @options.default-route-table-propagation@ - Indicates whether resource attachments automatically propagate routes to the default propagation route table (@enable@ | @disable@ ).     * @options.dns-support@ - Indicates whether DNS support is enabled (@enable@ | @disable@ ).     * @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath Protocol support is enabled (@enable@ | @disable@ ).     * @owner-id@ - The ID of the AWS account that owns the transit gateway.     * @state@ - The state of the attachment (@available@ | @deleted@ | @deleting@ | @failed@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).     * @transit-gateway-id@ - The ID of the transit gateway.
dtgsFilters :: Lens' DescribeTransitGateways [Filter]
dtgsFilters = lens _dtgsFilters (\ s a -> s{_dtgsFilters = a}) . _Default . _Coerce

-- | The IDs of the transit gateways.
dtgsTransitGatewayIds :: Lens' DescribeTransitGateways [Text]
dtgsTransitGatewayIds = lens _dtgsTransitGatewayIds (\ s a -> s{_dtgsTransitGatewayIds = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtgsNextToken :: Lens' DescribeTransitGateways (Maybe Text)
dtgsNextToken = lens _dtgsNextToken (\ s a -> s{_dtgsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgsDryRun :: Lens' DescribeTransitGateways (Maybe Bool)
dtgsDryRun = lens _dtgsDryRun (\ s a -> s{_dtgsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtgsMaxResults :: Lens' DescribeTransitGateways (Maybe Natural)
dtgsMaxResults = lens _dtgsMaxResults (\ s a -> s{_dtgsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTransitGateways where
        page rq rs
          | stop (rs ^. dtgrsNextToken) = Nothing
          | stop (rs ^. dtgrsTransitGateways) = Nothing
          | otherwise =
            Just $ rq & dtgsNextToken .~ rs ^. dtgrsNextToken

instance AWSRequest DescribeTransitGateways where
        type Rs DescribeTransitGateways =
             DescribeTransitGatewaysResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeTransitGatewaysResponse' <$>
                   (x .@? "transitGatewaySet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTransitGateways where

instance NFData DescribeTransitGateways where

instance ToHeaders DescribeTransitGateways where
        toHeaders = const mempty

instance ToPath DescribeTransitGateways where
        toPath = const "/"

instance ToQuery DescribeTransitGateways where
        toQuery DescribeTransitGateways'{..}
          = mconcat
              ["Action" =:
                 ("DescribeTransitGateways" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dtgsFilters),
               toQuery
                 (toQueryList "TransitGatewayIds" <$>
                    _dtgsTransitGatewayIds),
               "NextToken" =: _dtgsNextToken,
               "DryRun" =: _dtgsDryRun,
               "MaxResults" =: _dtgsMaxResults]

-- | /See:/ 'describeTransitGatewaysResponse' smart constructor.
data DescribeTransitGatewaysResponse = DescribeTransitGatewaysResponse'
  { _dtgrsTransitGateways :: !(Maybe [TransitGateway])
  , _dtgrsNextToken       :: !(Maybe Text)
  , _dtgrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransitGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrsTransitGateways' - Information about the transit gateways.
--
-- * 'dtgrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtgrsResponseStatus' - -- | The response status code.
describeTransitGatewaysResponse
    :: Int -- ^ 'dtgrsResponseStatus'
    -> DescribeTransitGatewaysResponse
describeTransitGatewaysResponse pResponseStatus_ =
  DescribeTransitGatewaysResponse'
    { _dtgrsTransitGateways = Nothing
    , _dtgrsNextToken = Nothing
    , _dtgrsResponseStatus = pResponseStatus_
    }


-- | Information about the transit gateways.
dtgrsTransitGateways :: Lens' DescribeTransitGatewaysResponse [TransitGateway]
dtgrsTransitGateways = lens _dtgrsTransitGateways (\ s a -> s{_dtgrsTransitGateways = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtgrsNextToken :: Lens' DescribeTransitGatewaysResponse (Maybe Text)
dtgrsNextToken = lens _dtgrsNextToken (\ s a -> s{_dtgrsNextToken = a})

-- | -- | The response status code.
dtgrsResponseStatus :: Lens' DescribeTransitGatewaysResponse Int
dtgrsResponseStatus = lens _dtgrsResponseStatus (\ s a -> s{_dtgrsResponseStatus = a})

instance NFData DescribeTransitGatewaysResponse where
