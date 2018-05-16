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
-- Module      : Network.AWS.EC2.DescribeSpotInstanceRequests
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Spot Instance requests that belong to your account. Spot Instances are instances that Amazon EC2 launches when the Spot price that you specify exceeds the current Spot price. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance Requests> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
-- You can use @DescribeSpotInstanceRequests@ to find a running Spot Instance by examining the response. If the status of the Spot Instance is @fulfilled@ , the instance ID appears in the response and contains the identifier of the instance. Alternatively, you can use 'DescribeInstances' with a filter to look for instances where the instance lifecycle is @spot@ .
--
-- Spot Instance requests are deleted four hours after they are canceled and their instances are terminated.
--
module Network.AWS.EC2.DescribeSpotInstanceRequests
    (
    -- * Creating a Request
      describeSpotInstanceRequests
    , DescribeSpotInstanceRequests
    -- * Request Lenses
    , dsirFilters
    , dsirSpotInstanceRequestIds
    , dsirDryRun

    -- * Destructuring the Response
    , describeSpotInstanceRequestsResponse
    , DescribeSpotInstanceRequestsResponse
    -- * Response Lenses
    , dsirrsSpotInstanceRequests
    , dsirrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeSpotInstanceRequests.
--
--
--
-- /See:/ 'describeSpotInstanceRequests' smart constructor.
data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests'
  { _dsirFilters                :: !(Maybe [Filter])
  , _dsirSpotInstanceRequestIds :: !(Maybe [Text])
  , _dsirDryRun                 :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotInstanceRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsirFilters' - One or more filters.     * @availability-zone-group@ - The Availability Zone group.     * @create-time@ - The time stamp when the Spot Instance request was created.     * @fault-code@ - The fault code related to the request.     * @fault-message@ - The fault message related to the request.     * @instance-id@ - The ID of the instance that fulfilled the request.     * @launch-group@ - The Spot Instance launch group.     * @launch.block-device-mapping.delete-on-termination@ - Indicates whether the EBS volume is deleted on instance termination.     * @launch.block-device-mapping.device-name@ - The device name for the volume in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).     * @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot for the EBS volume.     * @launch.block-device-mapping.volume-size@ - The size of the EBS volume, in GiB.     * @launch.block-device-mapping.volume-type@ - The type of EBS volume: @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic.     * @launch.group-id@ - The ID of the security group for the instance.     * @launch.group-name@ - The name of the security group for the instance.     * @launch.image-id@ - The ID of the AMI.     * @launch.instance-type@ - The type of instance (for example, @m3.medium@ ).     * @launch.kernel-id@ - The kernel ID.     * @launch.key-name@ - The name of the key pair the instance launched with.     * @launch.monitoring-enabled@ - Whether detailed monitoring is enabled for the Spot Instance.     * @launch.ramdisk-id@ - The RAM disk ID.     * @launched-availability-zone@ - The Availability Zone in which the request is launched.     * @network-interface.addresses.primary@ - Indicates whether the IP address is the primary private IP address.     * @network-interface.delete-on-termination@ - Indicates whether the network interface is deleted when the instance is terminated.     * @network-interface.description@ - A description of the network interface.     * @network-interface.device-index@ - The index of the device for the network interface attachment on the instance.     * @network-interface.group-id@ - The ID of the security group associated with the network interface.     * @network-interface.network-interface-id@ - The ID of the network interface.     * @network-interface.private-ip-address@ - The primary private IP address of the network interface.     * @network-interface.subnet-id@ - The ID of the subnet for the instance.     * @product-description@ - The product description associated with the instance (@Linux/UNIX@ | @Windows@ ).     * @spot-instance-request-id@ - The Spot Instance request ID.     * @spot-price@ - The maximum hourly price for any Spot Instance launched to fulfill the request.     * @state@ - The state of the Spot Instance request (@open@ | @active@ | @closed@ | @cancelled@ | @failed@ ). Spot request status information can help you track your Amazon EC2 Spot Instance requests. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot Request Status> in the /Amazon EC2 User Guide for Linux Instances/ .     * @status-code@ - The short code describing the most recent evaluation of your Spot Instance request.     * @status-message@ - The message explaining the status of the Spot Instance request.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.     * @type@ - The type of Spot Instance request (@one-time@ | @persistent@ ).     * @valid-from@ - The start date of the request.     * @valid-until@ - The end date of the request.
--
-- * 'dsirSpotInstanceRequestIds' - One or more Spot Instance request IDs.
--
-- * 'dsirDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeSpotInstanceRequests
    :: DescribeSpotInstanceRequests
describeSpotInstanceRequests =
  DescribeSpotInstanceRequests'
    { _dsirFilters = Nothing
    , _dsirSpotInstanceRequestIds = Nothing
    , _dsirDryRun = Nothing
    }


-- | One or more filters.     * @availability-zone-group@ - The Availability Zone group.     * @create-time@ - The time stamp when the Spot Instance request was created.     * @fault-code@ - The fault code related to the request.     * @fault-message@ - The fault message related to the request.     * @instance-id@ - The ID of the instance that fulfilled the request.     * @launch-group@ - The Spot Instance launch group.     * @launch.block-device-mapping.delete-on-termination@ - Indicates whether the EBS volume is deleted on instance termination.     * @launch.block-device-mapping.device-name@ - The device name for the volume in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).     * @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot for the EBS volume.     * @launch.block-device-mapping.volume-size@ - The size of the EBS volume, in GiB.     * @launch.block-device-mapping.volume-type@ - The type of EBS volume: @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic.     * @launch.group-id@ - The ID of the security group for the instance.     * @launch.group-name@ - The name of the security group for the instance.     * @launch.image-id@ - The ID of the AMI.     * @launch.instance-type@ - The type of instance (for example, @m3.medium@ ).     * @launch.kernel-id@ - The kernel ID.     * @launch.key-name@ - The name of the key pair the instance launched with.     * @launch.monitoring-enabled@ - Whether detailed monitoring is enabled for the Spot Instance.     * @launch.ramdisk-id@ - The RAM disk ID.     * @launched-availability-zone@ - The Availability Zone in which the request is launched.     * @network-interface.addresses.primary@ - Indicates whether the IP address is the primary private IP address.     * @network-interface.delete-on-termination@ - Indicates whether the network interface is deleted when the instance is terminated.     * @network-interface.description@ - A description of the network interface.     * @network-interface.device-index@ - The index of the device for the network interface attachment on the instance.     * @network-interface.group-id@ - The ID of the security group associated with the network interface.     * @network-interface.network-interface-id@ - The ID of the network interface.     * @network-interface.private-ip-address@ - The primary private IP address of the network interface.     * @network-interface.subnet-id@ - The ID of the subnet for the instance.     * @product-description@ - The product description associated with the instance (@Linux/UNIX@ | @Windows@ ).     * @spot-instance-request-id@ - The Spot Instance request ID.     * @spot-price@ - The maximum hourly price for any Spot Instance launched to fulfill the request.     * @state@ - The state of the Spot Instance request (@open@ | @active@ | @closed@ | @cancelled@ | @failed@ ). Spot request status information can help you track your Amazon EC2 Spot Instance requests. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot Request Status> in the /Amazon EC2 User Guide for Linux Instances/ .     * @status-code@ - The short code describing the most recent evaluation of your Spot Instance request.     * @status-message@ - The message explaining the status of the Spot Instance request.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.     * @type@ - The type of Spot Instance request (@one-time@ | @persistent@ ).     * @valid-from@ - The start date of the request.     * @valid-until@ - The end date of the request.
dsirFilters :: Lens' DescribeSpotInstanceRequests [Filter]
dsirFilters = lens _dsirFilters (\ s a -> s{_dsirFilters = a}) . _Default . _Coerce

-- | One or more Spot Instance request IDs.
dsirSpotInstanceRequestIds :: Lens' DescribeSpotInstanceRequests [Text]
dsirSpotInstanceRequestIds = lens _dsirSpotInstanceRequestIds (\ s a -> s{_dsirSpotInstanceRequestIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsirDryRun :: Lens' DescribeSpotInstanceRequests (Maybe Bool)
dsirDryRun = lens _dsirDryRun (\ s a -> s{_dsirDryRun = a})

instance AWSRequest DescribeSpotInstanceRequests
         where
        type Rs DescribeSpotInstanceRequests =
             DescribeSpotInstanceRequestsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotInstanceRequestsResponse' <$>
                   (x .@? "spotInstanceRequestSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSpotInstanceRequests where

instance NFData DescribeSpotInstanceRequests where

instance ToHeaders DescribeSpotInstanceRequests where
        toHeaders = const mempty

instance ToPath DescribeSpotInstanceRequests where
        toPath = const "/"

instance ToQuery DescribeSpotInstanceRequests where
        toQuery DescribeSpotInstanceRequests'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotInstanceRequests" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dsirFilters),
               toQuery
                 (toQueryList "SpotInstanceRequestId" <$>
                    _dsirSpotInstanceRequestIds),
               "DryRun" =: _dsirDryRun]

-- | Contains the output of DescribeSpotInstanceRequests.
--
--
--
-- /See:/ 'describeSpotInstanceRequestsResponse' smart constructor.
data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse'
  { _dsirrsSpotInstanceRequests :: !(Maybe [SpotInstanceRequest])
  , _dsirrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotInstanceRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsirrsSpotInstanceRequests' - One or more Spot Instance requests.
--
-- * 'dsirrsResponseStatus' - -- | The response status code.
describeSpotInstanceRequestsResponse
    :: Int -- ^ 'dsirrsResponseStatus'
    -> DescribeSpotInstanceRequestsResponse
describeSpotInstanceRequestsResponse pResponseStatus_ =
  DescribeSpotInstanceRequestsResponse'
    { _dsirrsSpotInstanceRequests = Nothing
    , _dsirrsResponseStatus = pResponseStatus_
    }


-- | One or more Spot Instance requests.
dsirrsSpotInstanceRequests :: Lens' DescribeSpotInstanceRequestsResponse [SpotInstanceRequest]
dsirrsSpotInstanceRequests = lens _dsirrsSpotInstanceRequests (\ s a -> s{_dsirrsSpotInstanceRequests = a}) . _Default . _Coerce

-- | -- | The response status code.
dsirrsResponseStatus :: Lens' DescribeSpotInstanceRequestsResponse Int
dsirrsResponseStatus = lens _dsirrsResponseStatus (\ s a -> s{_dsirrsResponseStatus = a})

instance NFData DescribeSpotInstanceRequestsResponse
         where
