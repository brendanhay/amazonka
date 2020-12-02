{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotInstanceRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Spot Instance requests.
--
--
-- You can use @DescribeSpotInstanceRequests@ to find a running Spot Instance by examining the response. If the status of the Spot Instance is @fulfilled@ , the instance ID appears in the response and contains the identifier of the instance. Alternatively, you can use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances DescribeInstances> with a filter to look for instances where the instance lifecycle is @spot@ .
--
-- We recommend that you set @MaxResults@ to a value between 5 and 1000 to limit the number of results returned. This paginates the output, which makes the list more manageable and returns the results faster. If the list of results exceeds your @MaxResults@ value, then that number of results is returned along with a @NextToken@ value that can be passed to a subsequent @DescribeSpotInstanceRequests@ request to retrieve the remaining results.
--
-- Spot Instance requests are deleted four hours after they are canceled and their instances are terminated.
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotInstanceRequests
  ( -- * Creating a Request
    describeSpotInstanceRequests,
    DescribeSpotInstanceRequests,

    -- * Request Lenses
    dsirFilters,
    dsirSpotInstanceRequestIds,
    dsirNextToken,
    dsirDryRun,
    dsirMaxResults,

    -- * Destructuring the Response
    describeSpotInstanceRequestsResponse,
    DescribeSpotInstanceRequestsResponse,

    -- * Response Lenses
    dsirrsNextToken,
    dsirrsSpotInstanceRequests,
    dsirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeSpotInstanceRequests.
--
--
--
-- /See:/ 'describeSpotInstanceRequests' smart constructor.
data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests'
  { _dsirFilters ::
      !(Maybe [Filter]),
    _dsirSpotInstanceRequestIds ::
      !(Maybe [Text]),
    _dsirNextToken :: !(Maybe Text),
    _dsirDryRun :: !(Maybe Bool),
    _dsirMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSpotInstanceRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsirFilters' - One or more filters.     * @availability-zone-group@ - The Availability Zone group.     * @create-time@ - The time stamp when the Spot Instance request was created.     * @fault-code@ - The fault code related to the request.     * @fault-message@ - The fault message related to the request.     * @instance-id@ - The ID of the instance that fulfilled the request.     * @launch-group@ - The Spot Instance launch group.     * @launch.block-device-mapping.delete-on-termination@ - Indicates whether the EBS volume is deleted on instance termination.     * @launch.block-device-mapping.device-name@ - The device name for the volume in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).     * @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot for the EBS volume.     * @launch.block-device-mapping.volume-size@ - The size of the EBS volume, in GiB.     * @launch.block-device-mapping.volume-type@ - The type of EBS volume: @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic.     * @launch.group-id@ - The ID of the security group for the instance.     * @launch.group-name@ - The name of the security group for the instance.     * @launch.image-id@ - The ID of the AMI.     * @launch.instance-type@ - The type of instance (for example, @m3.medium@ ).     * @launch.kernel-id@ - The kernel ID.     * @launch.key-name@ - The name of the key pair the instance launched with.     * @launch.monitoring-enabled@ - Whether detailed monitoring is enabled for the Spot Instance.     * @launch.ramdisk-id@ - The RAM disk ID.     * @launched-availability-zone@ - The Availability Zone in which the request is launched.     * @network-interface.addresses.primary@ - Indicates whether the IP address is the primary private IP address.     * @network-interface.delete-on-termination@ - Indicates whether the network interface is deleted when the instance is terminated.     * @network-interface.description@ - A description of the network interface.     * @network-interface.device-index@ - The index of the device for the network interface attachment on the instance.     * @network-interface.group-id@ - The ID of the security group associated with the network interface.     * @network-interface.network-interface-id@ - The ID of the network interface.     * @network-interface.private-ip-address@ - The primary private IP address of the network interface.     * @network-interface.subnet-id@ - The ID of the subnet for the instance.     * @product-description@ - The product description associated with the instance (@Linux/UNIX@ | @Windows@ ).     * @spot-instance-request-id@ - The Spot Instance request ID.     * @spot-price@ - The maximum hourly price for any Spot Instance launched to fulfill the request.     * @state@ - The state of the Spot Instance request (@open@ | @active@ | @closed@ | @cancelled@ | @failed@ ). Spot request status information can help you track your Amazon EC2 Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot request status> in the /Amazon EC2 User Guide for Linux Instances/ .     * @status-code@ - The short code describing the most recent evaluation of your Spot Instance request.     * @status-message@ - The message explaining the status of the Spot Instance request.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @type@ - The type of Spot Instance request (@one-time@ | @persistent@ ).     * @valid-from@ - The start date of the request.     * @valid-until@ - The end date of the request.
--
-- * 'dsirSpotInstanceRequestIds' - One or more Spot Instance request IDs.
--
-- * 'dsirNextToken' - The token to request the next set of results. This value is @null@ when there are no more results to return.
--
-- * 'dsirDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dsirMaxResults' - The maximum number of results to return in a single call. Specify a value between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
describeSpotInstanceRequests ::
  DescribeSpotInstanceRequests
describeSpotInstanceRequests =
  DescribeSpotInstanceRequests'
    { _dsirFilters = Nothing,
      _dsirSpotInstanceRequestIds = Nothing,
      _dsirNextToken = Nothing,
      _dsirDryRun = Nothing,
      _dsirMaxResults = Nothing
    }

-- | One or more filters.     * @availability-zone-group@ - The Availability Zone group.     * @create-time@ - The time stamp when the Spot Instance request was created.     * @fault-code@ - The fault code related to the request.     * @fault-message@ - The fault message related to the request.     * @instance-id@ - The ID of the instance that fulfilled the request.     * @launch-group@ - The Spot Instance launch group.     * @launch.block-device-mapping.delete-on-termination@ - Indicates whether the EBS volume is deleted on instance termination.     * @launch.block-device-mapping.device-name@ - The device name for the volume in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).     * @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot for the EBS volume.     * @launch.block-device-mapping.volume-size@ - The size of the EBS volume, in GiB.     * @launch.block-device-mapping.volume-type@ - The type of EBS volume: @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic.     * @launch.group-id@ - The ID of the security group for the instance.     * @launch.group-name@ - The name of the security group for the instance.     * @launch.image-id@ - The ID of the AMI.     * @launch.instance-type@ - The type of instance (for example, @m3.medium@ ).     * @launch.kernel-id@ - The kernel ID.     * @launch.key-name@ - The name of the key pair the instance launched with.     * @launch.monitoring-enabled@ - Whether detailed monitoring is enabled for the Spot Instance.     * @launch.ramdisk-id@ - The RAM disk ID.     * @launched-availability-zone@ - The Availability Zone in which the request is launched.     * @network-interface.addresses.primary@ - Indicates whether the IP address is the primary private IP address.     * @network-interface.delete-on-termination@ - Indicates whether the network interface is deleted when the instance is terminated.     * @network-interface.description@ - A description of the network interface.     * @network-interface.device-index@ - The index of the device for the network interface attachment on the instance.     * @network-interface.group-id@ - The ID of the security group associated with the network interface.     * @network-interface.network-interface-id@ - The ID of the network interface.     * @network-interface.private-ip-address@ - The primary private IP address of the network interface.     * @network-interface.subnet-id@ - The ID of the subnet for the instance.     * @product-description@ - The product description associated with the instance (@Linux/UNIX@ | @Windows@ ).     * @spot-instance-request-id@ - The Spot Instance request ID.     * @spot-price@ - The maximum hourly price for any Spot Instance launched to fulfill the request.     * @state@ - The state of the Spot Instance request (@open@ | @active@ | @closed@ | @cancelled@ | @failed@ ). Spot request status information can help you track your Amazon EC2 Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot request status> in the /Amazon EC2 User Guide for Linux Instances/ .     * @status-code@ - The short code describing the most recent evaluation of your Spot Instance request.     * @status-message@ - The message explaining the status of the Spot Instance request.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @type@ - The type of Spot Instance request (@one-time@ | @persistent@ ).     * @valid-from@ - The start date of the request.     * @valid-until@ - The end date of the request.
dsirFilters :: Lens' DescribeSpotInstanceRequests [Filter]
dsirFilters = lens _dsirFilters (\s a -> s {_dsirFilters = a}) . _Default . _Coerce

-- | One or more Spot Instance request IDs.
dsirSpotInstanceRequestIds :: Lens' DescribeSpotInstanceRequests [Text]
dsirSpotInstanceRequestIds = lens _dsirSpotInstanceRequestIds (\s a -> s {_dsirSpotInstanceRequestIds = a}) . _Default . _Coerce

-- | The token to request the next set of results. This value is @null@ when there are no more results to return.
dsirNextToken :: Lens' DescribeSpotInstanceRequests (Maybe Text)
dsirNextToken = lens _dsirNextToken (\s a -> s {_dsirNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsirDryRun :: Lens' DescribeSpotInstanceRequests (Maybe Bool)
dsirDryRun = lens _dsirDryRun (\s a -> s {_dsirDryRun = a})

-- | The maximum number of results to return in a single call. Specify a value between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dsirMaxResults :: Lens' DescribeSpotInstanceRequests (Maybe Int)
dsirMaxResults = lens _dsirMaxResults (\s a -> s {_dsirMaxResults = a})

instance AWSPager DescribeSpotInstanceRequests where
  page rq rs
    | stop (rs ^. dsirrsNextToken) = Nothing
    | stop (rs ^. dsirrsSpotInstanceRequests) = Nothing
    | otherwise = Just $ rq & dsirNextToken .~ rs ^. dsirrsNextToken

instance AWSRequest DescribeSpotInstanceRequests where
  type
    Rs DescribeSpotInstanceRequests =
      DescribeSpotInstanceRequestsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeSpotInstanceRequestsResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "spotInstanceRequestSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeSpotInstanceRequests

instance NFData DescribeSpotInstanceRequests

instance ToHeaders DescribeSpotInstanceRequests where
  toHeaders = const mempty

instance ToPath DescribeSpotInstanceRequests where
  toPath = const "/"

instance ToQuery DescribeSpotInstanceRequests where
  toQuery DescribeSpotInstanceRequests' {..} =
    mconcat
      [ "Action" =: ("DescribeSpotInstanceRequests" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dsirFilters),
        toQuery
          ( toQueryList "SpotInstanceRequestId"
              <$> _dsirSpotInstanceRequestIds
          ),
        "NextToken" =: _dsirNextToken,
        "DryRun" =: _dsirDryRun,
        "MaxResults" =: _dsirMaxResults
      ]

-- | Contains the output of DescribeSpotInstanceRequests.
--
--
--
-- /See:/ 'describeSpotInstanceRequestsResponse' smart constructor.
data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse'
  { _dsirrsNextToken ::
      !(Maybe Text),
    _dsirrsSpotInstanceRequests ::
      !( Maybe
           [SpotInstanceRequest]
       ),
    _dsirrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSpotInstanceRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsirrsNextToken' - The token to use to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- * 'dsirrsSpotInstanceRequests' - One or more Spot Instance requests.
--
-- * 'dsirrsResponseStatus' - -- | The response status code.
describeSpotInstanceRequestsResponse ::
  -- | 'dsirrsResponseStatus'
  Int ->
  DescribeSpotInstanceRequestsResponse
describeSpotInstanceRequestsResponse pResponseStatus_ =
  DescribeSpotInstanceRequestsResponse'
    { _dsirrsNextToken = Nothing,
      _dsirrsSpotInstanceRequests = Nothing,
      _dsirrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next set of results. This value is @null@ when there are no more results to return.
dsirrsNextToken :: Lens' DescribeSpotInstanceRequestsResponse (Maybe Text)
dsirrsNextToken = lens _dsirrsNextToken (\s a -> s {_dsirrsNextToken = a})

-- | One or more Spot Instance requests.
dsirrsSpotInstanceRequests :: Lens' DescribeSpotInstanceRequestsResponse [SpotInstanceRequest]
dsirrsSpotInstanceRequests = lens _dsirrsSpotInstanceRequests (\s a -> s {_dsirrsSpotInstanceRequests = a}) . _Default . _Coerce

-- | -- | The response status code.
dsirrsResponseStatus :: Lens' DescribeSpotInstanceRequestsResponse Int
dsirrsResponseStatus = lens _dsirrsResponseStatus (\s a -> s {_dsirrsResponseStatus = a})

instance NFData DescribeSpotInstanceRequestsResponse
