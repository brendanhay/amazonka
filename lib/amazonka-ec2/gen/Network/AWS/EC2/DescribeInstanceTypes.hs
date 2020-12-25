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
    ditDryRun,
    ditFilters,
    ditInstanceTypes,
    ditMaxResults,
    ditNextToken,

    -- * Destructuring the response
    DescribeInstanceTypesResponse (..),
    mkDescribeInstanceTypesResponse,

    -- ** Response lenses
    ditrrsInstanceTypes,
    ditrrsNextToken,
    ditrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceTypes' smart constructor.
data DescribeInstanceTypes = DescribeInstanceTypes'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceTypes :: Core.Maybe [Types.InstanceType],
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceTypes' value with any optional fields omitted.
mkDescribeInstanceTypes ::
  DescribeInstanceTypes
mkDescribeInstanceTypes =
  DescribeInstanceTypes'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      instanceTypes = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditDryRun :: Lens.Lens' DescribeInstanceTypes (Core.Maybe Core.Bool)
ditDryRun = Lens.field @"dryRun"
{-# DEPRECATED ditDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
ditFilters :: Lens.Lens' DescribeInstanceTypes (Core.Maybe [Types.Filter])
ditFilters = Lens.field @"filters"
{-# DEPRECATED ditFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditInstanceTypes :: Lens.Lens' DescribeInstanceTypes (Core.Maybe [Types.InstanceType])
ditInstanceTypes = Lens.field @"instanceTypes"
{-# DEPRECATED ditInstanceTypes "Use generic-lens or generic-optics with 'instanceTypes' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditMaxResults :: Lens.Lens' DescribeInstanceTypes (Core.Maybe Core.Natural)
ditMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ditMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditNextToken :: Lens.Lens' DescribeInstanceTypes (Core.Maybe Types.NextToken)
ditNextToken = Lens.field @"nextToken"
{-# DEPRECATED ditNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeInstanceTypes where
  type Rs DescribeInstanceTypes = DescribeInstanceTypesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeInstanceTypes")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "InstanceType" Core.<$> instanceTypes)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceTypesResponse'
            Core.<$> (x Core..@? "instanceTypeSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeInstanceTypes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"instanceTypes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeInstanceTypesResponse' smart constructor.
data DescribeInstanceTypesResponse = DescribeInstanceTypesResponse'
  { -- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceTypes :: Core.Maybe [Types.InstanceTypeInfo],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceTypesResponse' value with any optional fields omitted.
mkDescribeInstanceTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceTypesResponse
mkDescribeInstanceTypesResponse responseStatus =
  DescribeInstanceTypesResponse'
    { instanceTypes = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsInstanceTypes :: Lens.Lens' DescribeInstanceTypesResponse (Core.Maybe [Types.InstanceTypeInfo])
ditrrsInstanceTypes = Lens.field @"instanceTypes"
{-# DEPRECATED ditrrsInstanceTypes "Use generic-lens or generic-optics with 'instanceTypes' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsNextToken :: Lens.Lens' DescribeInstanceTypesResponse (Core.Maybe Types.NextToken)
ditrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ditrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsResponseStatus :: Lens.Lens' DescribeInstanceTypesResponse Core.Int
ditrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ditrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
