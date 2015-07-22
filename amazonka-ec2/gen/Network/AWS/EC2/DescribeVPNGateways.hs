{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPNGateways
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your virtual private gateways.
--
-- For more information about virtual private gateways, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding an IPsec Hardware VPN to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPNGateways.html>
module Network.AWS.EC2.DescribeVPNGateways
    (
    -- * Request
      DescribeVPNGateways
    -- ** Request constructor
    , describeVPNGateways
    -- ** Request lenses
    , dvgsrqFilters
    , dvgsrqDryRun
    , dvgsrqVPNGatewayIds

    -- * Response
    , DescribeVPNGatewaysResponse
    -- ** Response constructor
    , describeVPNGatewaysResponse
    -- ** Response lenses
    , dvgsrsVPNGateways
    , dvgsrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPNGateways' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvgsrqFilters'
--
-- * 'dvgsrqDryRun'
--
-- * 'dvgsrqVPNGatewayIds'
data DescribeVPNGateways = DescribeVPNGateways'
    { _dvgsrqFilters       :: !(Maybe [Filter])
    , _dvgsrqDryRun        :: !(Maybe Bool)
    , _dvgsrqVPNGatewayIds :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPNGateways' smart constructor.
describeVPNGateways :: DescribeVPNGateways
describeVPNGateways =
    DescribeVPNGateways'
    { _dvgsrqFilters = Nothing
    , _dvgsrqDryRun = Nothing
    , _dvgsrqVPNGatewayIds = Nothing
    }

-- | One or more filters.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@attaching@ | @attached@ | @detaching@ |
--     @detached@).
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @availability-zone@ - The Availability Zone for the virtual private
--     gateway.
--
-- -   @state@ - The state of the virtual private gateway (@pending@ |
--     @available@ | @deleting@ | @deleted@).
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
-- -   @type@ - The type of virtual private gateway. Currently the only
--     supported type is @ipsec.1@.
--
-- -   @vpn-gateway-id@ - The ID of the virtual private gateway.
--
dvgsrqFilters :: Lens' DescribeVPNGateways [Filter]
dvgsrqFilters = lens _dvgsrqFilters (\ s a -> s{_dvgsrqFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvgsrqDryRun :: Lens' DescribeVPNGateways (Maybe Bool)
dvgsrqDryRun = lens _dvgsrqDryRun (\ s a -> s{_dvgsrqDryRun = a});

-- | One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
dvgsrqVPNGatewayIds :: Lens' DescribeVPNGateways [Text]
dvgsrqVPNGatewayIds = lens _dvgsrqVPNGatewayIds (\ s a -> s{_dvgsrqVPNGatewayIds = a}) . _Default;

instance AWSRequest DescribeVPNGateways where
        type Sv DescribeVPNGateways = EC2
        type Rs DescribeVPNGateways =
             DescribeVPNGatewaysResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPNGatewaysResponse' <$>
                   (x .@? "vpnGatewaySet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeVPNGateways where
        toHeaders = const mempty

instance ToPath DescribeVPNGateways where
        toPath = const "/"

instance ToQuery DescribeVPNGateways where
        toQuery DescribeVPNGateways'{..}
          = mconcat
              ["Action" =: ("DescribeVPNGateways" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvgsrqFilters),
               "DryRun" =: _dvgsrqDryRun,
               toQuery
                 (toQueryList "VpnGatewayId" <$>
                    _dvgsrqVPNGatewayIds)]

-- | /See:/ 'describeVPNGatewaysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvgsrsVPNGateways'
--
-- * 'dvgsrsStatus'
data DescribeVPNGatewaysResponse = DescribeVPNGatewaysResponse'
    { _dvgsrsVPNGateways :: !(Maybe [VPNGateway])
    , _dvgsrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPNGatewaysResponse' smart constructor.
describeVPNGatewaysResponse :: Int -> DescribeVPNGatewaysResponse
describeVPNGatewaysResponse pStatus =
    DescribeVPNGatewaysResponse'
    { _dvgsrsVPNGateways = Nothing
    , _dvgsrsStatus = pStatus
    }

-- | Information about one or more virtual private gateways.
dvgsrsVPNGateways :: Lens' DescribeVPNGatewaysResponse [VPNGateway]
dvgsrsVPNGateways = lens _dvgsrsVPNGateways (\ s a -> s{_dvgsrsVPNGateways = a}) . _Default;

-- | FIXME: Undocumented member.
dvgsrsStatus :: Lens' DescribeVPNGatewaysResponse Int
dvgsrsStatus = lens _dvgsrsStatus (\ s a -> s{_dvgsrsStatus = a});
