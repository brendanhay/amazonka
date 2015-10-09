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
-- Module      : Network.AWS.EC2.DescribeCustomerGateways
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPN customer gateways.
--
-- For more information about VPN customer gateways, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeCustomerGateways.html AWS API Reference> for DescribeCustomerGateways.
module Network.AWS.EC2.DescribeCustomerGateways
    (
    -- * Creating a Request
      describeCustomerGateways
    , DescribeCustomerGateways
    -- * Request Lenses
    , dcgCustomerGatewayIds
    , dcgFilters
    , dcgDryRun

    -- * Destructuring the Response
    , describeCustomerGatewaysResponse
    , DescribeCustomerGatewaysResponse
    -- * Response Lenses
    , dcgrsCustomerGateways
    , dcgrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeCustomerGateways' smart constructor.
data DescribeCustomerGateways = DescribeCustomerGateways'
    { _dcgCustomerGatewayIds :: !(Maybe [Text])
    , _dcgFilters            :: !(Maybe [Filter])
    , _dcgDryRun             :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCustomerGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgCustomerGatewayIds'
--
-- * 'dcgFilters'
--
-- * 'dcgDryRun'
describeCustomerGateways
    :: DescribeCustomerGateways
describeCustomerGateways =
    DescribeCustomerGateways'
    { _dcgCustomerGatewayIds = Nothing
    , _dcgFilters = Nothing
    , _dcgDryRun = Nothing
    }

-- | One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
dcgCustomerGatewayIds :: Lens' DescribeCustomerGateways [Text]
dcgCustomerGatewayIds = lens _dcgCustomerGatewayIds (\ s a -> s{_dcgCustomerGatewayIds = a}) . _Default . _Coerce;

-- | One or more filters.
--
-- -   'bgp-asn' - The customer gateway\'s Border Gateway Protocol (BGP)
--     Autonomous System Number (ASN).
--
-- -   'customer-gateway-id' - The ID of the customer gateway.
--
-- -   'ip-address' - The IP address of the customer gateway\'s
--     Internet-routable external interface.
--
-- -   'state' - The state of the customer gateway ('pending' | 'available'
--     | 'deleting' | 'deleted').
--
-- -   'type' - The type of customer gateway. Currently, the only supported
--     type is 'ipsec.1'.
--
-- -   'tag':/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   'tag-key' - The key of a tag assigned to the resource. This filter
--     is independent of the 'tag-value' filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- -   'tag-value' - The value of a tag assigned to the resource. This
--     filter is independent of the 'tag-key' filter.
--
dcgFilters :: Lens' DescribeCustomerGateways [Filter]
dcgFilters = lens _dcgFilters (\ s a -> s{_dcgFilters = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dcgDryRun :: Lens' DescribeCustomerGateways (Maybe Bool)
dcgDryRun = lens _dcgDryRun (\ s a -> s{_dcgDryRun = a});

instance AWSRequest DescribeCustomerGateways where
        type Rs DescribeCustomerGateways =
             DescribeCustomerGatewaysResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 DescribeCustomerGatewaysResponse' <$>
                   (x .@? "customerGatewaySet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeCustomerGateways where
        toHeaders = const mempty

instance ToPath DescribeCustomerGateways where
        toPath = const "/"

instance ToQuery DescribeCustomerGateways where
        toQuery DescribeCustomerGateways'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCustomerGateways" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               toQuery
                 (toQueryList "CustomerGatewayId" <$>
                    _dcgCustomerGatewayIds),
               toQuery (toQueryList "Filter" <$> _dcgFilters),
               "DryRun" =: _dcgDryRun]

-- | /See:/ 'describeCustomerGatewaysResponse' smart constructor.
data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse'
    { _dcgrsCustomerGateways :: !(Maybe [CustomerGateway])
    , _dcgrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCustomerGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgrsCustomerGateways'
--
-- * 'dcgrsResponseStatus'
describeCustomerGatewaysResponse
    :: Int -- ^ 'dcgrsResponseStatus'
    -> DescribeCustomerGatewaysResponse
describeCustomerGatewaysResponse pResponseStatus_ =
    DescribeCustomerGatewaysResponse'
    { _dcgrsCustomerGateways = Nothing
    , _dcgrsResponseStatus = pResponseStatus_
    }

-- | Information about one or more customer gateways.
dcgrsCustomerGateways :: Lens' DescribeCustomerGatewaysResponse [CustomerGateway]
dcgrsCustomerGateways = lens _dcgrsCustomerGateways (\ s a -> s{_dcgrsCustomerGateways = a}) . _Default . _Coerce;

-- | The response status code.
dcgrsResponseStatus :: Lens' DescribeCustomerGatewaysResponse Int
dcgrsResponseStatus = lens _dcgrsResponseStatus (\ s a -> s{_dcgrsResponseStatus = a});
