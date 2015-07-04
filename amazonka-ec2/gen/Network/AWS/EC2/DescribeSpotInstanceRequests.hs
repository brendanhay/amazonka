{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.DescribeSpotInstanceRequests
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the Spot Instance requests that belong to your account. Spot
-- Instances are instances that Amazon EC2 launches when the bid price that
-- you specify exceeds the current Spot Price. Amazon EC2 periodically sets
-- the Spot Price based on available Spot Instance capacity and current
-- Spot Instance requests. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance Requests>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You can use @DescribeSpotInstanceRequests@ to find a running Spot
-- Instance by examining the response. If the status of the Spot Instance
-- is @fulfilled@, the instance ID appears in the response and contains the
-- identifier of the instance. Alternatively, you can use DescribeInstances
-- with a filter to look for instances where the instance lifecycle is
-- @spot@.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotInstanceRequests.html>
module Network.AWS.EC2.DescribeSpotInstanceRequests
    (
    -- * Request
      DescribeSpotInstanceRequests
    -- ** Request constructor
    , describeSpotInstanceRequests
    -- ** Request lenses
    , dsirFilters
    , dsirSpotInstanceRequestIds
    , dsirDryRun

    -- * Response
    , DescribeSpotInstanceRequestsResponse
    -- ** Response constructor
    , describeSpotInstanceRequestsResponse
    -- ** Response lenses
    , dsirrSpotInstanceRequests
    , dsirrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeSpotInstanceRequests.
--
-- /See:/ 'describeSpotInstanceRequests' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsirFilters'
--
-- * 'dsirSpotInstanceRequestIds'
--
-- * 'dsirDryRun'
data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests'
    { _dsirFilters                :: !(Maybe [Filter])
    , _dsirSpotInstanceRequestIds :: !(Maybe [Text])
    , _dsirDryRun                 :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotInstanceRequests' smart constructor.
describeSpotInstanceRequests :: DescribeSpotInstanceRequests
describeSpotInstanceRequests =
    DescribeSpotInstanceRequests'
    { _dsirFilters = Nothing
    , _dsirSpotInstanceRequestIds = Nothing
    , _dsirDryRun = Nothing
    }

-- | One or more filters.
--
-- -   @availability-zone-group@ - The Availability Zone group.
--
-- -   @create-time@ - The time stamp when the Spot Instance request was
--     created.
--
-- -   @fault-code@ - The fault code related to the request.
--
-- -   @fault-message@ - The fault message related to the request.
--
-- -   @instance-id@ - The ID of the instance that fulfilled the request.
--
-- -   @launch-group@ - The Spot Instance launch group.
--
-- -   @launch.block-device-mapping.delete-on-termination@ - Indicates
--     whether the Amazon EBS volume is deleted on instance termination.
--
-- -   @launch.block-device-mapping.device-name@ - The device name for the
--     Amazon EBS volume (for example, @\/dev\/sdh@).
--
-- -   @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot
--     used for the Amazon EBS volume.
--
-- -   @launch.block-device-mapping.volume-size@ - The size of the Amazon
--     EBS volume, in GiB.
--
-- -   @launch.block-device-mapping.volume-type@ - The type of the Amazon
--     EBS volume (@gp2@ | @standard@ | @io1@).
--
-- -   @launch.group-id@ - The security group for the instance.
--
-- -   @launch.image-id@ - The ID of the AMI.
--
-- -   @launch.instance-type@ - The type of instance (for example,
--     @m1.small@).
--
-- -   @launch.kernel-id@ - The kernel ID.
--
-- -   @launch.key-name@ - The name of the key pair the instance launched
--     with.
--
-- -   @launch.monitoring-enabled@ - Whether monitoring is enabled for the
--     Spot Instance.
--
-- -   @launch.ramdisk-id@ - The RAM disk ID.
--
-- -   @network-interface.network-interface-id@ - The ID of the network
--     interface.
--
-- -   @network-interface.device-index@ - The index of the device for the
--     network interface attachment on the instance.
--
-- -   @network-interface.subnet-id@ - The ID of the subnet for the
--     instance.
--
-- -   @network-interface.description@ - A description of the network
--     interface.
--
-- -   @network-interface.private-ip-address@ - The primary private IP
--     address of the network interface.
--
-- -   @network-interface.delete-on-termination@ - Indicates whether the
--     network interface is deleted when the instance is terminated.
--
-- -   @network-interface.group-id@ - The ID of the security group
--     associated with the network interface.
--
-- -   @network-interface.group-name@ - The name of the security group
--     associated with the network interface.
--
-- -   @network-interface.addresses.primary@ - Indicates whether the IP
--     address is the primary private IP address.
--
-- -   @product-description@ - The product description associated with the
--     instance (@Linux\/UNIX@ | @Windows@).
--
-- -   @spot-instance-request-id@ - The Spot Instance request ID.
--
-- -   @spot-price@ - The maximum hourly price for any Spot Instance
--     launched to fulfill the request.
--
-- -   @state@ - The state of the Spot Instance request (@open@ | @active@
--     | @closed@ | @cancelled@ | @failed@). Spot bid status information
--     can help you track your Amazon EC2 Spot Instance requests. For more
--     information, see
--     <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot Bid Status>
--     in the Amazon Elastic Compute Cloud User Guide.
--
-- -   @status-code@ - The short code describing the most recent evaluation
--     of your Spot Instance request.
--
-- -   @status-message@ - The message explaining the status of the Spot
--     Instance request.
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
-- -   @type@ - The type of Spot Instance request (@one-time@ |
--     @persistent@).
--
-- -   @launched-availability-zone@ - The Availability Zone in which the
--     bid is launched.
--
-- -   @valid-from@ - The start date of the request.
--
-- -   @valid-until@ - The end date of the request.
--
dsirFilters :: Lens' DescribeSpotInstanceRequests [Filter]
dsirFilters = lens _dsirFilters (\ s a -> s{_dsirFilters = a}) . _Default;

-- | One or more Spot Instance request IDs.
dsirSpotInstanceRequestIds :: Lens' DescribeSpotInstanceRequests [Text]
dsirSpotInstanceRequestIds = lens _dsirSpotInstanceRequestIds (\ s a -> s{_dsirSpotInstanceRequestIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsirDryRun :: Lens' DescribeSpotInstanceRequests (Maybe Bool)
dsirDryRun = lens _dsirDryRun (\ s a -> s{_dsirDryRun = a});

instance AWSRequest DescribeSpotInstanceRequests
         where
        type Sv DescribeSpotInstanceRequests = EC2
        type Rs DescribeSpotInstanceRequests =
             DescribeSpotInstanceRequestsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotInstanceRequestsResponse' <$>
                   (x .@? "spotInstanceRequestSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeSpotInstanceRequests where
        toHeaders = const mempty

instance ToPath DescribeSpotInstanceRequests where
        toPath = const "/"

instance ToQuery DescribeSpotInstanceRequests where
        toQuery DescribeSpotInstanceRequests'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotInstanceRequests" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dsirFilters),
               toQuery
                 (toQueryList "SpotInstanceRequestId" <$>
                    _dsirSpotInstanceRequestIds),
               "DryRun" =: _dsirDryRun]

-- | Contains the output of DescribeSpotInstanceRequests.
--
-- /See:/ 'describeSpotInstanceRequestsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsirrSpotInstanceRequests'
--
-- * 'dsirrStatus'
data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse'
    { _dsirrSpotInstanceRequests :: !(Maybe [SpotInstanceRequest])
    , _dsirrStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotInstanceRequestsResponse' smart constructor.
describeSpotInstanceRequestsResponse :: Int -> DescribeSpotInstanceRequestsResponse
describeSpotInstanceRequestsResponse pStatus =
    DescribeSpotInstanceRequestsResponse'
    { _dsirrSpotInstanceRequests = Nothing
    , _dsirrStatus = pStatus
    }

-- | One or more Spot Instance requests.
dsirrSpotInstanceRequests :: Lens' DescribeSpotInstanceRequestsResponse [SpotInstanceRequest]
dsirrSpotInstanceRequests = lens _dsirrSpotInstanceRequests (\ s a -> s{_dsirrSpotInstanceRequests = a}) . _Default;

-- | FIXME: Undocumented member.
dsirrStatus :: Lens' DescribeSpotInstanceRequestsResponse Int
dsirrStatus = lens _dsirrStatus (\ s a -> s{_dsirrStatus = a});
