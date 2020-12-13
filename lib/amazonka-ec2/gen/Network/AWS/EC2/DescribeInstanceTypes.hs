{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceTypes
  ( -- * Creating a request
    DescribeInstanceTypes (..),
    mkDescribeInstanceTypes,

    -- ** Request lenses
    ditInstanceTypes,
    ditFilters,
    ditNextToken,
    ditDryRun,
    ditMaxResults,

    -- * Destructuring the response
    DescribeInstanceTypesResponse (..),
    mkDescribeInstanceTypesResponse,

    -- ** Response lenses
    ditrsInstanceTypes,
    ditrsNextToken,
    ditrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceTypes' smart constructor.
data DescribeInstanceTypes = DescribeInstanceTypes'
  { -- | The instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceTypes :: Lude.Maybe [InstanceType],
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    --
    --     * @auto-recovery-supported@ - Indicates whether auto recovery is supported (@true@ | @false@ ).
    --
    --
    --     * @bare-metal@ - Indicates whether it is a bare metal instance type (@true@ | @false@ ).
    --
    --
    --     * @burstable-performance-supported@ - Indicates whether it is a burstable performance instance type (@true@ | @false@ ).
    --
    --
    --     * @current-generation@ - Indicates whether this instance type is the latest generation instance type of an instance family (@true@ | @false@ ).
    --
    --
    --     * @ebs-info.ebs-optimized-info.baseline-bandwidth-in-mbps@ - The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.
    --
    --
    --     * @ebs-info.ebs-optimized-info.baseline-iops@ - The baseline input/output storage operations per second for an EBS-optimized instance type.
    --
    --
    --     * @ebs-info.ebs-optimized-info.baseline-throughput-in-mbps@ - The baseline throughput performance for an EBS-optimized instance type, in MB/s.
    --
    --
    --     * @ebs-info.ebs-optimized-info.maximum-bandwidth-in-mbps@ - The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.
    --
    --
    --     * @ebs-info.ebs-optimized-info.maximum-iops@ - The maximum input/output storage operations per second for an EBS-optimized instance type.
    --
    --
    --     * @ebs-info.ebs-optimized-info.maximum-throughput-in-mbps@ - The maximum throughput performance for an EBS-optimized instance type, in MB/s.
    --
    --
    --     * @ebs-info.ebs-optimized-support@ - Indicates whether the instance type is EBS-optimized (@supported@ | @unsupported@ | @default@ ).
    --
    --
    --     * @ebs-info.encryption-support@ - Indicates whether EBS encryption is supported (@supported@ | @unsupported@ ).
    --
    --
    --     * @ebs-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for EBS volumes (@required@ | @supported@ | @unsupported@ ).
    --
    --
    --     * @free-tier-eligible@ - Indicates whether the instance type is eligible to use in the free tier (@true@ | @false@ ).
    --
    --
    --     * @hibernation-supported@ - Indicates whether On-Demand hibernation is supported (@true@ | @false@ ).
    --
    --
    --     * @hypervisor@ - The hypervisor (@nitro@ | @xen@ ).
    --
    --
    --     * @instance-storage-info.disk.count@ - The number of local disks.
    --
    --
    --     * @instance-storage-info.disk.size-in-gb@ - The storage size of each instance storage disk, in GB.
    --
    --
    --     * @instance-storage-info.disk.type@ - The storage technology for the local instance storage disks (@hdd@ | @ssd@ ).
    --
    --
    --     * @instance-storage-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for instance store (@required@ | @supported@ ) | @unsupported@ ).
    --
    --
    --     * @instance-storage-info.total-size-in-gb@ - The total amount of storage available from all local instance storage, in GB.
    --
    --
    --     * @instance-storage-supported@ - Indicates whether the instance type has local instance storage (@true@ | @false@ ).
    --
    --
    --     * @instance-type@ - The instance type (for example @c5.2xlarge@ or c5*).
    --
    --
    --     * @memory-info.size-in-mib@ - The memory size.
    --
    --
    --     * @network-info.efa-supported@ - Indicates whether the instance type supports Elastic Fabric Adapter (EFA) (@true@ | @false@ ).
    --
    --
    --     * @network-info.ena-support@ - Indicates whether Elastic Network Adapter (ENA) is supported or required (@required@ | @supported@ | @unsupported@ ).
    --
    --
    --     * @network-info.ipv4-addresses-per-interface@ - The maximum number of private IPv4 addresses per network interface.
    --
    --
    --     * @network-info.ipv6-addresses-per-interface@ - The maximum number of private IPv6 addresses per network interface.
    --
    --
    --     * @network-info.ipv6-supported@ - Indicates whether the instance type supports IPv6 (@true@ | @false@ ).
    --
    --
    --     * @network-info.maximum-network-interfaces@ - The maximum number of network interfaces per instance.
    --
    --
    --     * @network-info.network-performance@ - The network performance (for example, "25 Gigabit").
    --
    --
    --     * @processor-info.supported-architecture@ - The CPU architecture (@arm64@ | @i386@ | @x86_64@ ).
    --
    --
    --     * @processor-info.sustained-clock-speed-in-ghz@ - The CPU clock speed, in GHz.
    --
    --
    --     * @supported-root-device-type@ - The root device type (@ebs@ | @instance-store@ ).
    --
    --
    --     * @supported-usage-class@ - The usage class (@on-demand@ | @spot@ ).
    --
    --
    --     * @supported-virtualization-type@ - The virtualization type (@hvm@ | @paravirtual@ ).
    --
    --
    --     * @vcpu-info.default-cores@ - The default number of cores for the instance type.
    --
    --
    --     * @vcpu-info.default-threads-per-core@ - The default number of threads per core for the instance type.
    --
    --
    --     * @vcpu-info.default-vcpus@ - The default number of vCPUs for the instance type.
    --
    --
    --     * @vcpu-info.valid-cores@ - The number of cores that can be configured for the instance type.
    --
    --
    --     * @vcpu-info.valid-threads-per-core@ - The number of threads per core that can be configured for the instance type. For example, "1" or "1,2".
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceTypes' with the minimum fields required to make a request.
--
-- * 'instanceTypes' - The instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
--
--
--     * @auto-recovery-supported@ - Indicates whether auto recovery is supported (@true@ | @false@ ).
--
--
--     * @bare-metal@ - Indicates whether it is a bare metal instance type (@true@ | @false@ ).
--
--
--     * @burstable-performance-supported@ - Indicates whether it is a burstable performance instance type (@true@ | @false@ ).
--
--
--     * @current-generation@ - Indicates whether this instance type is the latest generation instance type of an instance family (@true@ | @false@ ).
--
--
--     * @ebs-info.ebs-optimized-info.baseline-bandwidth-in-mbps@ - The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.
--
--
--     * @ebs-info.ebs-optimized-info.baseline-iops@ - The baseline input/output storage operations per second for an EBS-optimized instance type.
--
--
--     * @ebs-info.ebs-optimized-info.baseline-throughput-in-mbps@ - The baseline throughput performance for an EBS-optimized instance type, in MB/s.
--
--
--     * @ebs-info.ebs-optimized-info.maximum-bandwidth-in-mbps@ - The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.
--
--
--     * @ebs-info.ebs-optimized-info.maximum-iops@ - The maximum input/output storage operations per second for an EBS-optimized instance type.
--
--
--     * @ebs-info.ebs-optimized-info.maximum-throughput-in-mbps@ - The maximum throughput performance for an EBS-optimized instance type, in MB/s.
--
--
--     * @ebs-info.ebs-optimized-support@ - Indicates whether the instance type is EBS-optimized (@supported@ | @unsupported@ | @default@ ).
--
--
--     * @ebs-info.encryption-support@ - Indicates whether EBS encryption is supported (@supported@ | @unsupported@ ).
--
--
--     * @ebs-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for EBS volumes (@required@ | @supported@ | @unsupported@ ).
--
--
--     * @free-tier-eligible@ - Indicates whether the instance type is eligible to use in the free tier (@true@ | @false@ ).
--
--
--     * @hibernation-supported@ - Indicates whether On-Demand hibernation is supported (@true@ | @false@ ).
--
--
--     * @hypervisor@ - The hypervisor (@nitro@ | @xen@ ).
--
--
--     * @instance-storage-info.disk.count@ - The number of local disks.
--
--
--     * @instance-storage-info.disk.size-in-gb@ - The storage size of each instance storage disk, in GB.
--
--
--     * @instance-storage-info.disk.type@ - The storage technology for the local instance storage disks (@hdd@ | @ssd@ ).
--
--
--     * @instance-storage-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for instance store (@required@ | @supported@ ) | @unsupported@ ).
--
--
--     * @instance-storage-info.total-size-in-gb@ - The total amount of storage available from all local instance storage, in GB.
--
--
--     * @instance-storage-supported@ - Indicates whether the instance type has local instance storage (@true@ | @false@ ).
--
--
--     * @instance-type@ - The instance type (for example @c5.2xlarge@ or c5*).
--
--
--     * @memory-info.size-in-mib@ - The memory size.
--
--
--     * @network-info.efa-supported@ - Indicates whether the instance type supports Elastic Fabric Adapter (EFA) (@true@ | @false@ ).
--
--
--     * @network-info.ena-support@ - Indicates whether Elastic Network Adapter (ENA) is supported or required (@required@ | @supported@ | @unsupported@ ).
--
--
--     * @network-info.ipv4-addresses-per-interface@ - The maximum number of private IPv4 addresses per network interface.
--
--
--     * @network-info.ipv6-addresses-per-interface@ - The maximum number of private IPv6 addresses per network interface.
--
--
--     * @network-info.ipv6-supported@ - Indicates whether the instance type supports IPv6 (@true@ | @false@ ).
--
--
--     * @network-info.maximum-network-interfaces@ - The maximum number of network interfaces per instance.
--
--
--     * @network-info.network-performance@ - The network performance (for example, "25 Gigabit").
--
--
--     * @processor-info.supported-architecture@ - The CPU architecture (@arm64@ | @i386@ | @x86_64@ ).
--
--
--     * @processor-info.sustained-clock-speed-in-ghz@ - The CPU clock speed, in GHz.
--
--
--     * @supported-root-device-type@ - The root device type (@ebs@ | @instance-store@ ).
--
--
--     * @supported-usage-class@ - The usage class (@on-demand@ | @spot@ ).
--
--
--     * @supported-virtualization-type@ - The virtualization type (@hvm@ | @paravirtual@ ).
--
--
--     * @vcpu-info.default-cores@ - The default number of cores for the instance type.
--
--
--     * @vcpu-info.default-threads-per-core@ - The default number of threads per core for the instance type.
--
--
--     * @vcpu-info.default-vcpus@ - The default number of vCPUs for the instance type.
--
--
--     * @vcpu-info.valid-cores@ - The number of cores that can be configured for the instance type.
--
--
--     * @vcpu-info.valid-threads-per-core@ - The number of threads per core that can be configured for the instance type. For example, "1" or "1,2".
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
mkDescribeInstanceTypes ::
  DescribeInstanceTypes
mkDescribeInstanceTypes =
  DescribeInstanceTypes'
    { instanceTypes = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditInstanceTypes :: Lens.Lens' DescribeInstanceTypes (Lude.Maybe [InstanceType])
ditInstanceTypes = Lens.lens (instanceTypes :: DescribeInstanceTypes -> Lude.Maybe [InstanceType]) (\s a -> s {instanceTypes = a} :: DescribeInstanceTypes)
{-# DEPRECATED ditInstanceTypes "Use generic-lens or generic-optics with 'instanceTypes' instead." #-}

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @auto-recovery-supported@ - Indicates whether auto recovery is supported (@true@ | @false@ ).
--
--
--     * @bare-metal@ - Indicates whether it is a bare metal instance type (@true@ | @false@ ).
--
--
--     * @burstable-performance-supported@ - Indicates whether it is a burstable performance instance type (@true@ | @false@ ).
--
--
--     * @current-generation@ - Indicates whether this instance type is the latest generation instance type of an instance family (@true@ | @false@ ).
--
--
--     * @ebs-info.ebs-optimized-info.baseline-bandwidth-in-mbps@ - The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.
--
--
--     * @ebs-info.ebs-optimized-info.baseline-iops@ - The baseline input/output storage operations per second for an EBS-optimized instance type.
--
--
--     * @ebs-info.ebs-optimized-info.baseline-throughput-in-mbps@ - The baseline throughput performance for an EBS-optimized instance type, in MB/s.
--
--
--     * @ebs-info.ebs-optimized-info.maximum-bandwidth-in-mbps@ - The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.
--
--
--     * @ebs-info.ebs-optimized-info.maximum-iops@ - The maximum input/output storage operations per second for an EBS-optimized instance type.
--
--
--     * @ebs-info.ebs-optimized-info.maximum-throughput-in-mbps@ - The maximum throughput performance for an EBS-optimized instance type, in MB/s.
--
--
--     * @ebs-info.ebs-optimized-support@ - Indicates whether the instance type is EBS-optimized (@supported@ | @unsupported@ | @default@ ).
--
--
--     * @ebs-info.encryption-support@ - Indicates whether EBS encryption is supported (@supported@ | @unsupported@ ).
--
--
--     * @ebs-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for EBS volumes (@required@ | @supported@ | @unsupported@ ).
--
--
--     * @free-tier-eligible@ - Indicates whether the instance type is eligible to use in the free tier (@true@ | @false@ ).
--
--
--     * @hibernation-supported@ - Indicates whether On-Demand hibernation is supported (@true@ | @false@ ).
--
--
--     * @hypervisor@ - The hypervisor (@nitro@ | @xen@ ).
--
--
--     * @instance-storage-info.disk.count@ - The number of local disks.
--
--
--     * @instance-storage-info.disk.size-in-gb@ - The storage size of each instance storage disk, in GB.
--
--
--     * @instance-storage-info.disk.type@ - The storage technology for the local instance storage disks (@hdd@ | @ssd@ ).
--
--
--     * @instance-storage-info.nvme-support@ - Indicates whether non-volatile memory express (NVMe) is supported for instance store (@required@ | @supported@ ) | @unsupported@ ).
--
--
--     * @instance-storage-info.total-size-in-gb@ - The total amount of storage available from all local instance storage, in GB.
--
--
--     * @instance-storage-supported@ - Indicates whether the instance type has local instance storage (@true@ | @false@ ).
--
--
--     * @instance-type@ - The instance type (for example @c5.2xlarge@ or c5*).
--
--
--     * @memory-info.size-in-mib@ - The memory size.
--
--
--     * @network-info.efa-supported@ - Indicates whether the instance type supports Elastic Fabric Adapter (EFA) (@true@ | @false@ ).
--
--
--     * @network-info.ena-support@ - Indicates whether Elastic Network Adapter (ENA) is supported or required (@required@ | @supported@ | @unsupported@ ).
--
--
--     * @network-info.ipv4-addresses-per-interface@ - The maximum number of private IPv4 addresses per network interface.
--
--
--     * @network-info.ipv6-addresses-per-interface@ - The maximum number of private IPv6 addresses per network interface.
--
--
--     * @network-info.ipv6-supported@ - Indicates whether the instance type supports IPv6 (@true@ | @false@ ).
--
--
--     * @network-info.maximum-network-interfaces@ - The maximum number of network interfaces per instance.
--
--
--     * @network-info.network-performance@ - The network performance (for example, "25 Gigabit").
--
--
--     * @processor-info.supported-architecture@ - The CPU architecture (@arm64@ | @i386@ | @x86_64@ ).
--
--
--     * @processor-info.sustained-clock-speed-in-ghz@ - The CPU clock speed, in GHz.
--
--
--     * @supported-root-device-type@ - The root device type (@ebs@ | @instance-store@ ).
--
--
--     * @supported-usage-class@ - The usage class (@on-demand@ | @spot@ ).
--
--
--     * @supported-virtualization-type@ - The virtualization type (@hvm@ | @paravirtual@ ).
--
--
--     * @vcpu-info.default-cores@ - The default number of cores for the instance type.
--
--
--     * @vcpu-info.default-threads-per-core@ - The default number of threads per core for the instance type.
--
--
--     * @vcpu-info.default-vcpus@ - The default number of vCPUs for the instance type.
--
--
--     * @vcpu-info.valid-cores@ - The number of cores that can be configured for the instance type.
--
--
--     * @vcpu-info.valid-threads-per-core@ - The number of threads per core that can be configured for the instance type. For example, "1" or "1,2".
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditFilters :: Lens.Lens' DescribeInstanceTypes (Lude.Maybe [Filter])
ditFilters = Lens.lens (filters :: DescribeInstanceTypes -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeInstanceTypes)
{-# DEPRECATED ditFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditNextToken :: Lens.Lens' DescribeInstanceTypes (Lude.Maybe Lude.Text)
ditNextToken = Lens.lens (nextToken :: DescribeInstanceTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceTypes)
{-# DEPRECATED ditNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditDryRun :: Lens.Lens' DescribeInstanceTypes (Lude.Maybe Lude.Bool)
ditDryRun = Lens.lens (dryRun :: DescribeInstanceTypes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeInstanceTypes)
{-# DEPRECATED ditDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditMaxResults :: Lens.Lens' DescribeInstanceTypes (Lude.Maybe Lude.Natural)
ditMaxResults = Lens.lens (maxResults :: DescribeInstanceTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInstanceTypes)
{-# DEPRECATED ditMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInstanceTypes where
  page rq rs
    | Page.stop (rs Lens.^. ditrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ditrsInstanceTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ditNextToken Lens..~ rs Lens.^. ditrsNextToken

instance Lude.AWSRequest DescribeInstanceTypes where
  type Rs DescribeInstanceTypes = DescribeInstanceTypesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeInstanceTypesResponse'
            Lude.<$> ( x Lude..@? "instanceTypeSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstanceTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceTypes where
  toQuery DescribeInstanceTypes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInstanceTypes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "InstanceType" Lude.<$> instanceTypes),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeInstanceTypesResponse' smart constructor.
data DescribeInstanceTypesResponse = DescribeInstanceTypesResponse'
  { -- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceTypes :: Lude.Maybe [InstanceTypeInfo],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceTypesResponse' with the minimum fields required to make a request.
--
-- * 'instanceTypes' - The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceTypesResponse
mkDescribeInstanceTypesResponse pResponseStatus_ =
  DescribeInstanceTypesResponse'
    { instanceTypes = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrsInstanceTypes :: Lens.Lens' DescribeInstanceTypesResponse (Lude.Maybe [InstanceTypeInfo])
ditrsInstanceTypes = Lens.lens (instanceTypes :: DescribeInstanceTypesResponse -> Lude.Maybe [InstanceTypeInfo]) (\s a -> s {instanceTypes = a} :: DescribeInstanceTypesResponse)
{-# DEPRECATED ditrsInstanceTypes "Use generic-lens or generic-optics with 'instanceTypes' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrsNextToken :: Lens.Lens' DescribeInstanceTypesResponse (Lude.Maybe Lude.Text)
ditrsNextToken = Lens.lens (nextToken :: DescribeInstanceTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceTypesResponse)
{-# DEPRECATED ditrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrsResponseStatus :: Lens.Lens' DescribeInstanceTypesResponse Lude.Int
ditrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceTypesResponse)
{-# DEPRECATED ditrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
