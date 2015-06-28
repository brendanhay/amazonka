{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeCustomerGateways
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your VPN customer gateways.
--
-- For more information about VPN customer gateways, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeCustomerGateways.html>
module Network.AWS.EC2.DescribeCustomerGateways
    (
    -- * Request
      DescribeCustomerGateways
    -- ** Request constructor
    , describeCustomerGateways
    -- ** Request lenses
    , dcgCustomerGatewayIds
    , dcgFilters
    , dcgDryRun

    -- * Response
    , DescribeCustomerGatewaysResponse
    -- ** Response constructor
    , describeCustomerGatewaysResponse
    -- ** Response lenses
    , dcgrCustomerGateways
    , dcgrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeCustomerGateways' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcgCustomerGatewayIds'
--
-- * 'dcgFilters'
--
-- * 'dcgDryRun'
data DescribeCustomerGateways = DescribeCustomerGateways'
    { _dcgCustomerGatewayIds :: !(Maybe [Text])
    , _dcgFilters            :: !(Maybe [Filter])
    , _dcgDryRun             :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeCustomerGateways' smart constructor.
describeCustomerGateways :: DescribeCustomerGateways
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
dcgCustomerGatewayIds = lens _dcgCustomerGatewayIds (\ s a -> s{_dcgCustomerGatewayIds = a}) . _Default;

-- | One or more filters.
--
-- -   @bgp-asn@ - The customer gateway\'s Border Gateway Protocol (BGP)
--     Autonomous System Number (ASN).
--
-- -   @customer-gateway-id@ - The ID of the customer gateway.
--
-- -   @ip-address@ - The IP address of the customer gateway\'s
--     Internet-routable external interface.
--
-- -   @state@ - The state of the customer gateway (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @type@ - The type of customer gateway. Currently, the only supported
--     type is @ipsec.1@.
--
-- -   @tag@:/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. This filter
--     is independent of the @tag-value@ filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the @tag@:/key/=/value/ filter.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. This
--     filter is independent of the @tag-key@ filter.
--
dcgFilters :: Lens' DescribeCustomerGateways [Filter]
dcgFilters = lens _dcgFilters (\ s a -> s{_dcgFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dcgDryRun :: Lens' DescribeCustomerGateways (Maybe Bool)
dcgDryRun = lens _dcgDryRun (\ s a -> s{_dcgDryRun = a});

instance AWSRequest DescribeCustomerGateways where
        type Sv DescribeCustomerGateways = EC2
        type Rs DescribeCustomerGateways =
             DescribeCustomerGatewaysResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeCustomerGatewaysResponse' <$>
                   (may (parseXMLList "item") x) <*> (pure s))

instance ToHeaders DescribeCustomerGateways where
        toHeaders = const mempty

instance ToPath DescribeCustomerGateways where
        toPath = const "/"

instance ToQuery DescribeCustomerGateways where
        toQuery DescribeCustomerGateways'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCustomerGateways" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "CustomerGatewayId" <$>
                    _dcgCustomerGatewayIds),
               toQuery (toQueryList "Filter" <$> _dcgFilters),
               "DryRun" =: _dcgDryRun]

-- | /See:/ 'describeCustomerGatewaysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcgrCustomerGateways'
--
-- * 'dcgrStatus'
data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse'
    { _dcgrCustomerGateways :: !(Maybe [CustomerGateway])
    , _dcgrStatus           :: !Status
    } deriving (Eq,Show)

-- | 'DescribeCustomerGatewaysResponse' smart constructor.
describeCustomerGatewaysResponse :: Status -> DescribeCustomerGatewaysResponse
describeCustomerGatewaysResponse pStatus =
    DescribeCustomerGatewaysResponse'
    { _dcgrCustomerGateways = Nothing
    , _dcgrStatus = pStatus
    }

-- | Information about one or more customer gateways.
dcgrCustomerGateways :: Lens' DescribeCustomerGatewaysResponse [CustomerGateway]
dcgrCustomerGateways = lens _dcgrCustomerGateways (\ s a -> s{_dcgrCustomerGateways = a}) . _Default;

-- | FIXME: Undocumented member.
dcgrStatus :: Lens' DescribeCustomerGatewaysResponse Status
dcgrStatus = lens _dcgrStatus (\ s a -> s{_dcgrStatus = a});
