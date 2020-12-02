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
-- Module      : Network.AWS.EC2.DescribeInstanceTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of the instance types that are offered in a location. The results can be filtered by the attributes of the instance types.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceTypes
  ( -- * Creating a Request
    describeInstanceTypes,
    DescribeInstanceTypes,

    -- * Request Lenses
    ditInstanceTypes,
    ditFilters,
    ditNextToken,
    ditDryRun,
    ditMaxResults,

    -- * Destructuring the Response
    describeInstanceTypesResponse,
    DescribeInstanceTypesResponse,

    -- * Response Lenses
    ditrsInstanceTypes,
    ditrsNextToken,
    ditrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInstanceTypes' smart constructor.
data DescribeInstanceTypes = DescribeInstanceTypes'
  { _ditInstanceTypes ::
      !(Maybe [InstanceType]),
    _ditFilters :: !(Maybe [Filter]),
    _ditNextToken :: !(Maybe Text),
    _ditDryRun :: !(Maybe Bool),
    _ditMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstanceTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ditInstanceTypes' - The instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ditFilters' - One or more filters. Filter names and values are case-sensitive.     * @auto-recovery-supported@ - Indicates whether auto recovery is supported (@true@ | @false@ ).     * @bare-metal@ - Indicates whether it is a bare metal instance type (@true@ | @false@ ).     * @burstable-performance-supported@ - Indicates whether it is a burstable performance instance type (@true@ | @false@ ).     * @current-generation@ - Indicates whether this instance type is the latest generation instance type of an instance family (@true@ | @false@ ).     * @ebs-info.ebs-optimized-info.baseline-bandwidth-in-mbps@ - The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.     * @ebs-info.ebs-optimized-info.baseline-iops@ - The baseline input/output storage operations per second for an EBS-optimized instance type.     * @ebs-info.ebs-optimized-info.baseline-throughput-in-mbps@ - The baseline throughput performance for an EBS-optimized instance type, in MB/s.     * @ebs-info.ebs-optimized-info.maximum-bandwidth-in-mbps@ - The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.     * @ebs-info.ebs-optimized-info.maximum-iops@ - The maximum input/output storage operations per second for an EBS-optimized instance type.     * @ebs-info.ebs-optimized-info.maximum-throughput-in-mbps@ - The maximum throughput performance for an EBS-optimized instance type, in MB/s.     * @ebs-info.ebs-optimized-support@ - Indicates whether the instance type is EBS-optimized (@supported@ | @unsupported@ | @default@ ).     * @ebs-info.encryption-support@ - Indicates whether EBS encryption is supported (@supported@ | @unsupported@ ).     * @ebs-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for EBS volumes (@required@ | @supported@ | @unsupported@ ).     * @free-tier-eligible@ - Indicates whether the instance type is eligible to use in the free tier (@true@ | @false@ ).     * @hibernation-supported@ - Indicates whether On-Demand hibernation is supported (@true@ | @false@ ).     * @hypervisor@ - The hypervisor (@nitro@ | @xen@ ).     * @instance-storage-info.disk.count@ - The number of local disks.     * @instance-storage-info.disk.size-in-gb@ - The storage size of each instance storage disk, in GB.     * @instance-storage-info.disk.type@ - The storage technology for the local instance storage disks (@hdd@ | @ssd@ ).     * @instance-storage-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for instance store (@required@ | @supported@ ) | @unsupported@ ).     * @instance-storage-info.total-size-in-gb@ - The total amount of storage available from all local instance storage, in GB.     * @instance-storage-supported@ - Indicates whether the instance type has local instance storage (@true@ | @false@ ).     * @instance-type@ - The instance type (for example @c5.2xlarge@ or c5*).     * @memory-info.size-in-mib@ - The memory size.     * @network-info.efa-supported@ - Indicates whether the instance type supports Elastic Fabric Adapter (EFA) (@true@ | @false@ ).     * @network-info.ena-support@ - Indicates whether Elastic Network Adapter (ENA) is supported or required (@required@ | @supported@ | @unsupported@ ).     * @network-info.ipv4-addresses-per-interface@ - The maximum number of private IPv4 addresses per network interface.     * @network-info.ipv6-addresses-per-interface@ - The maximum number of private IPv6 addresses per network interface.     * @network-info.ipv6-supported@ - Indicates whether the instance type supports IPv6 (@true@ | @false@ ).     * @network-info.maximum-network-interfaces@ - The maximum number of network interfaces per instance.     * @network-info.network-performance@ - The network performance (for example, "25 Gigabit").     * @processor-info.supported-architecture@ - The CPU architecture (@arm64@ | @i386@ | @x86_64@ ).     * @processor-info.sustained-clock-speed-in-ghz@ - The CPU clock speed, in GHz.     * @supported-root-device-type@ - The root device type (@ebs@ | @instance-store@ ).     * @supported-usage-class@ - The usage class (@on-demand@ | @spot@ ).     * @supported-virtualization-type@ - The virtualization type (@hvm@ | @paravirtual@ ).     * @vcpu-info.default-cores@ - The default number of cores for the instance type.     * @vcpu-info.default-threads-per-core@ - The default number of threads per core for the instance type.     * @vcpu-info.default-vcpus@ - The default number of vCPUs for the instance type.     * @vcpu-info.valid-cores@ - The number of cores that can be configured for the instance type.     * @vcpu-info.valid-threads-per-core@ - The number of threads per core that can be configured for the instance type. For example, "1" or "1,2".
--
-- * 'ditNextToken' - The token to retrieve the next page of results.
--
-- * 'ditDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ditMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
describeInstanceTypes ::
  DescribeInstanceTypes
describeInstanceTypes =
  DescribeInstanceTypes'
    { _ditInstanceTypes = Nothing,
      _ditFilters = Nothing,
      _ditNextToken = Nothing,
      _ditDryRun = Nothing,
      _ditMaxResults = Nothing
    }

-- | The instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
ditInstanceTypes :: Lens' DescribeInstanceTypes [InstanceType]
ditInstanceTypes = lens _ditInstanceTypes (\s a -> s {_ditInstanceTypes = a}) . _Default . _Coerce

-- | One or more filters. Filter names and values are case-sensitive.     * @auto-recovery-supported@ - Indicates whether auto recovery is supported (@true@ | @false@ ).     * @bare-metal@ - Indicates whether it is a bare metal instance type (@true@ | @false@ ).     * @burstable-performance-supported@ - Indicates whether it is a burstable performance instance type (@true@ | @false@ ).     * @current-generation@ - Indicates whether this instance type is the latest generation instance type of an instance family (@true@ | @false@ ).     * @ebs-info.ebs-optimized-info.baseline-bandwidth-in-mbps@ - The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.     * @ebs-info.ebs-optimized-info.baseline-iops@ - The baseline input/output storage operations per second for an EBS-optimized instance type.     * @ebs-info.ebs-optimized-info.baseline-throughput-in-mbps@ - The baseline throughput performance for an EBS-optimized instance type, in MB/s.     * @ebs-info.ebs-optimized-info.maximum-bandwidth-in-mbps@ - The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.     * @ebs-info.ebs-optimized-info.maximum-iops@ - The maximum input/output storage operations per second for an EBS-optimized instance type.     * @ebs-info.ebs-optimized-info.maximum-throughput-in-mbps@ - The maximum throughput performance for an EBS-optimized instance type, in MB/s.     * @ebs-info.ebs-optimized-support@ - Indicates whether the instance type is EBS-optimized (@supported@ | @unsupported@ | @default@ ).     * @ebs-info.encryption-support@ - Indicates whether EBS encryption is supported (@supported@ | @unsupported@ ).     * @ebs-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for EBS volumes (@required@ | @supported@ | @unsupported@ ).     * @free-tier-eligible@ - Indicates whether the instance type is eligible to use in the free tier (@true@ | @false@ ).     * @hibernation-supported@ - Indicates whether On-Demand hibernation is supported (@true@ | @false@ ).     * @hypervisor@ - The hypervisor (@nitro@ | @xen@ ).     * @instance-storage-info.disk.count@ - The number of local disks.     * @instance-storage-info.disk.size-in-gb@ - The storage size of each instance storage disk, in GB.     * @instance-storage-info.disk.type@ - The storage technology for the local instance storage disks (@hdd@ | @ssd@ ).     * @instance-storage-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for instance store (@required@ | @supported@ ) | @unsupported@ ).     * @instance-storage-info.total-size-in-gb@ - The total amount of storage available from all local instance storage, in GB.     * @instance-storage-supported@ - Indicates whether the instance type has local instance storage (@true@ | @false@ ).     * @instance-type@ - The instance type (for example @c5.2xlarge@ or c5*).     * @memory-info.size-in-mib@ - The memory size.     * @network-info.efa-supported@ - Indicates whether the instance type supports Elastic Fabric Adapter (EFA) (@true@ | @false@ ).     * @network-info.ena-support@ - Indicates whether Elastic Network Adapter (ENA) is supported or required (@required@ | @supported@ | @unsupported@ ).     * @network-info.ipv4-addresses-per-interface@ - The maximum number of private IPv4 addresses per network interface.     * @network-info.ipv6-addresses-per-interface@ - The maximum number of private IPv6 addresses per network interface.     * @network-info.ipv6-supported@ - Indicates whether the instance type supports IPv6 (@true@ | @false@ ).     * @network-info.maximum-network-interfaces@ - The maximum number of network interfaces per instance.     * @network-info.network-performance@ - The network performance (for example, "25 Gigabit").     * @processor-info.supported-architecture@ - The CPU architecture (@arm64@ | @i386@ | @x86_64@ ).     * @processor-info.sustained-clock-speed-in-ghz@ - The CPU clock speed, in GHz.     * @supported-root-device-type@ - The root device type (@ebs@ | @instance-store@ ).     * @supported-usage-class@ - The usage class (@on-demand@ | @spot@ ).     * @supported-virtualization-type@ - The virtualization type (@hvm@ | @paravirtual@ ).     * @vcpu-info.default-cores@ - The default number of cores for the instance type.     * @vcpu-info.default-threads-per-core@ - The default number of threads per core for the instance type.     * @vcpu-info.default-vcpus@ - The default number of vCPUs for the instance type.     * @vcpu-info.valid-cores@ - The number of cores that can be configured for the instance type.     * @vcpu-info.valid-threads-per-core@ - The number of threads per core that can be configured for the instance type. For example, "1" or "1,2".
ditFilters :: Lens' DescribeInstanceTypes [Filter]
ditFilters = lens _ditFilters (\s a -> s {_ditFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
ditNextToken :: Lens' DescribeInstanceTypes (Maybe Text)
ditNextToken = lens _ditNextToken (\s a -> s {_ditNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ditDryRun :: Lens' DescribeInstanceTypes (Maybe Bool)
ditDryRun = lens _ditDryRun (\s a -> s {_ditDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
ditMaxResults :: Lens' DescribeInstanceTypes (Maybe Natural)
ditMaxResults = lens _ditMaxResults (\s a -> s {_ditMaxResults = a}) . mapping _Nat

instance AWSPager DescribeInstanceTypes where
  page rq rs
    | stop (rs ^. ditrsNextToken) = Nothing
    | stop (rs ^. ditrsInstanceTypes) = Nothing
    | otherwise = Just $ rq & ditNextToken .~ rs ^. ditrsNextToken

instance AWSRequest DescribeInstanceTypes where
  type Rs DescribeInstanceTypes = DescribeInstanceTypesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeInstanceTypesResponse'
            <$> (x .@? "instanceTypeSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeInstanceTypes

instance NFData DescribeInstanceTypes

instance ToHeaders DescribeInstanceTypes where
  toHeaders = const mempty

instance ToPath DescribeInstanceTypes where
  toPath = const "/"

instance ToQuery DescribeInstanceTypes where
  toQuery DescribeInstanceTypes' {..} =
    mconcat
      [ "Action" =: ("DescribeInstanceTypes" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "InstanceType" <$> _ditInstanceTypes),
        toQuery (toQueryList "Filter" <$> _ditFilters),
        "NextToken" =: _ditNextToken,
        "DryRun" =: _ditDryRun,
        "MaxResults" =: _ditMaxResults
      ]

-- | /See:/ 'describeInstanceTypesResponse' smart constructor.
data DescribeInstanceTypesResponse = DescribeInstanceTypesResponse'
  { _ditrsInstanceTypes ::
      !(Maybe [InstanceTypeInfo]),
    _ditrsNextToken ::
      !(Maybe Text),
    _ditrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstanceTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ditrsInstanceTypes' - The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ditrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ditrsResponseStatus' - -- | The response status code.
describeInstanceTypesResponse ::
  -- | 'ditrsResponseStatus'
  Int ->
  DescribeInstanceTypesResponse
describeInstanceTypesResponse pResponseStatus_ =
  DescribeInstanceTypesResponse'
    { _ditrsInstanceTypes = Nothing,
      _ditrsNextToken = Nothing,
      _ditrsResponseStatus = pResponseStatus_
    }

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
ditrsInstanceTypes :: Lens' DescribeInstanceTypesResponse [InstanceTypeInfo]
ditrsInstanceTypes = lens _ditrsInstanceTypes (\s a -> s {_ditrsInstanceTypes = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
ditrsNextToken :: Lens' DescribeInstanceTypesResponse (Maybe Text)
ditrsNextToken = lens _ditrsNextToken (\s a -> s {_ditrsNextToken = a})

-- | -- | The response status code.
ditrsResponseStatus :: Lens' DescribeInstanceTypesResponse Int
ditrsResponseStatus = lens _ditrsResponseStatus (\s a -> s {_ditrsResponseStatus = a})

instance NFData DescribeInstanceTypesResponse
