{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.DescribeInstanceTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of the instance types that are offered in a
-- location. The results can be filtered by the attributes of the instance
-- types.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeInstanceTypes
  ( -- * Creating a Request
    DescribeInstanceTypes (..),
    newDescribeInstanceTypes,

    -- * Request Lenses
    describeInstanceTypes_nextToken,
    describeInstanceTypes_instanceTypes,
    describeInstanceTypes_filters,
    describeInstanceTypes_dryRun,
    describeInstanceTypes_maxResults,

    -- * Destructuring the Response
    DescribeInstanceTypesResponse (..),
    newDescribeInstanceTypesResponse,

    -- * Response Lenses
    describeInstanceTypesResponse_nextToken,
    describeInstanceTypesResponse_instanceTypes,
    describeInstanceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceTypes' smart constructor.
data DescribeInstanceTypes = DescribeInstanceTypes'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The instance types. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    instanceTypes :: Prelude.Maybe [InstanceType],
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @auto-recovery-supported@ - Indicates whether auto recovery is
    --     supported (@true@ | @false@).
    --
    -- -   @bare-metal@ - Indicates whether it is a bare metal instance type
    --     (@true@ | @false@).
    --
    -- -   @burstable-performance-supported@ - Indicates whether it is a
    --     burstable performance instance type (@true@ | @false@).
    --
    -- -   @current-generation@ - Indicates whether this instance type is the
    --     latest generation instance type of an instance family (@true@ |
    --     @false@).
    --
    -- -   @ebs-info.ebs-optimized-info.baseline-bandwidth-in-mbps@ - The
    --     baseline bandwidth performance for an EBS-optimized instance type,
    --     in Mbps.
    --
    -- -   @ebs-info.ebs-optimized-info.baseline-iops@ - The baseline
    --     input\/output storage operations per second for an EBS-optimized
    --     instance type.
    --
    -- -   @ebs-info.ebs-optimized-info.baseline-throughput-in-mbps@ - The
    --     baseline throughput performance for an EBS-optimized instance type,
    --     in MB\/s.
    --
    -- -   @ebs-info.ebs-optimized-info.maximum-bandwidth-in-mbps@ - The
    --     maximum bandwidth performance for an EBS-optimized instance type, in
    --     Mbps.
    --
    -- -   @ebs-info.ebs-optimized-info.maximum-iops@ - The maximum
    --     input\/output storage operations per second for an EBS-optimized
    --     instance type.
    --
    -- -   @ebs-info.ebs-optimized-info.maximum-throughput-in-mbps@ - The
    --     maximum throughput performance for an EBS-optimized instance type,
    --     in MB\/s.
    --
    -- -   @ebs-info.ebs-optimized-support@ - Indicates whether the instance
    --     type is EBS-optimized (@supported@ | @unsupported@ | @default@).
    --
    -- -   @ebs-info.encryption-support@ - Indicates whether EBS encryption is
    --     supported (@supported@ | @unsupported@).
    --
    -- -   @ebs-info.nvme-support@ - Indicates whether non-volatile memory
    --     express (NVMe) is supported for EBS volumes (@required@ |
    --     @supported@ | @unsupported@).
    --
    -- -   @free-tier-eligible@ - Indicates whether the instance type is
    --     eligible to use in the free tier (@true@ | @false@).
    --
    -- -   @hibernation-supported@ - Indicates whether On-Demand hibernation is
    --     supported (@true@ | @false@).
    --
    -- -   @hypervisor@ - The hypervisor (@nitro@ | @xen@).
    --
    -- -   @instance-storage-info.disk.count@ - The number of local disks.
    --
    -- -   @instance-storage-info.disk.size-in-gb@ - The storage size of each
    --     instance storage disk, in GB.
    --
    -- -   @instance-storage-info.disk.type@ - The storage technology for the
    --     local instance storage disks (@hdd@ | @ssd@).
    --
    -- -   @instance-storage-info.encryption-support@ - Indicates whether data
    --     is encrypted at rest (@required@ | @supported@ | @unsupported@).
    --
    -- -   @instance-storage-info.nvme-support@ - Indicates whether
    --     non-volatile memory express (NVMe) is supported for instance store
    --     (@required@ | @supported@ | @unsupported@).
    --
    -- -   @instance-storage-info.total-size-in-gb@ - The total amount of
    --     storage available from all local instance storage, in GB.
    --
    -- -   @instance-storage-supported@ - Indicates whether the instance type
    --     has local instance storage (@true@ | @false@).
    --
    -- -   @instance-type@ - The instance type (for example @c5.2xlarge@ or
    --     c5*).
    --
    -- -   @memory-info.size-in-mib@ - The memory size.
    --
    -- -   @network-info.efa-info.maximum-efa-interfaces@ - The maximum number
    --     of Elastic Fabric Adapters (EFAs) per instance.
    --
    -- -   @network-info.efa-supported@ - Indicates whether the instance type
    --     supports Elastic Fabric Adapter (EFA) (@true@ | @false@).
    --
    -- -   @network-info.ena-support@ - Indicates whether Elastic Network
    --     Adapter (ENA) is supported or required (@required@ | @supported@ |
    --     @unsupported@).
    --
    -- -   @network-info.encryption-in-transit-supported@ - Indicates whether
    --     the instance type automatically encrypts in-transit traffic between
    --     instances (@true@ | @false@).
    --
    -- -   @network-info.ipv4-addresses-per-interface@ - The maximum number of
    --     private IPv4 addresses per network interface.
    --
    -- -   @network-info.ipv6-addresses-per-interface@ - The maximum number of
    --     private IPv6 addresses per network interface.
    --
    -- -   @network-info.ipv6-supported@ - Indicates whether the instance type
    --     supports IPv6 (@true@ | @false@).
    --
    -- -   @network-info.maximum-network-cards@ - The maximum number of network
    --     cards per instance.
    --
    -- -   @network-info.maximum-network-interfaces@ - The maximum number of
    --     network interfaces per instance.
    --
    -- -   @network-info.network-performance@ - The network performance (for
    --     example, \"25 Gigabit\").
    --
    -- -   @processor-info.supported-architecture@ - The CPU architecture
    --     (@arm64@ | @i386@ | @x86_64@).
    --
    -- -   @processor-info.sustained-clock-speed-in-ghz@ - The CPU clock speed,
    --     in GHz.
    --
    -- -   @supported-boot-mode@ - The boot mode (@legacy-bios@ | @uefi@).
    --
    -- -   @supported-root-device-type@ - The root device type (@ebs@ |
    --     @instance-store@).
    --
    -- -   @supported-usage-class@ - The usage class (@on-demand@ | @spot@).
    --
    -- -   @supported-virtualization-type@ - The virtualization type (@hvm@ |
    --     @paravirtual@).
    --
    -- -   @vcpu-info.default-cores@ - The default number of cores for the
    --     instance type.
    --
    -- -   @vcpu-info.default-threads-per-core@ - The default number of threads
    --     per core for the instance type.
    --
    -- -   @vcpu-info.default-vcpus@ - The default number of vCPUs for the
    --     instance type.
    --
    -- -   @vcpu-info.valid-cores@ - The number of cores that can be configured
    --     for the instance type.
    --
    -- -   @vcpu-info.valid-threads-per-core@ - The number of threads per core
    --     that can be configured for the instance type. For example, \"1\" or
    --     \"1,2\".
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the next token value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceTypes_nextToken' - The token to retrieve the next page of results.
--
-- 'instanceTypes', 'describeInstanceTypes_instanceTypes' - The instance types. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- 'filters', 'describeInstanceTypes_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @auto-recovery-supported@ - Indicates whether auto recovery is
--     supported (@true@ | @false@).
--
-- -   @bare-metal@ - Indicates whether it is a bare metal instance type
--     (@true@ | @false@).
--
-- -   @burstable-performance-supported@ - Indicates whether it is a
--     burstable performance instance type (@true@ | @false@).
--
-- -   @current-generation@ - Indicates whether this instance type is the
--     latest generation instance type of an instance family (@true@ |
--     @false@).
--
-- -   @ebs-info.ebs-optimized-info.baseline-bandwidth-in-mbps@ - The
--     baseline bandwidth performance for an EBS-optimized instance type,
--     in Mbps.
--
-- -   @ebs-info.ebs-optimized-info.baseline-iops@ - The baseline
--     input\/output storage operations per second for an EBS-optimized
--     instance type.
--
-- -   @ebs-info.ebs-optimized-info.baseline-throughput-in-mbps@ - The
--     baseline throughput performance for an EBS-optimized instance type,
--     in MB\/s.
--
-- -   @ebs-info.ebs-optimized-info.maximum-bandwidth-in-mbps@ - The
--     maximum bandwidth performance for an EBS-optimized instance type, in
--     Mbps.
--
-- -   @ebs-info.ebs-optimized-info.maximum-iops@ - The maximum
--     input\/output storage operations per second for an EBS-optimized
--     instance type.
--
-- -   @ebs-info.ebs-optimized-info.maximum-throughput-in-mbps@ - The
--     maximum throughput performance for an EBS-optimized instance type,
--     in MB\/s.
--
-- -   @ebs-info.ebs-optimized-support@ - Indicates whether the instance
--     type is EBS-optimized (@supported@ | @unsupported@ | @default@).
--
-- -   @ebs-info.encryption-support@ - Indicates whether EBS encryption is
--     supported (@supported@ | @unsupported@).
--
-- -   @ebs-info.nvme-support@ - Indicates whether non-volatile memory
--     express (NVMe) is supported for EBS volumes (@required@ |
--     @supported@ | @unsupported@).
--
-- -   @free-tier-eligible@ - Indicates whether the instance type is
--     eligible to use in the free tier (@true@ | @false@).
--
-- -   @hibernation-supported@ - Indicates whether On-Demand hibernation is
--     supported (@true@ | @false@).
--
-- -   @hypervisor@ - The hypervisor (@nitro@ | @xen@).
--
-- -   @instance-storage-info.disk.count@ - The number of local disks.
--
-- -   @instance-storage-info.disk.size-in-gb@ - The storage size of each
--     instance storage disk, in GB.
--
-- -   @instance-storage-info.disk.type@ - The storage technology for the
--     local instance storage disks (@hdd@ | @ssd@).
--
-- -   @instance-storage-info.encryption-support@ - Indicates whether data
--     is encrypted at rest (@required@ | @supported@ | @unsupported@).
--
-- -   @instance-storage-info.nvme-support@ - Indicates whether
--     non-volatile memory express (NVMe) is supported for instance store
--     (@required@ | @supported@ | @unsupported@).
--
-- -   @instance-storage-info.total-size-in-gb@ - The total amount of
--     storage available from all local instance storage, in GB.
--
-- -   @instance-storage-supported@ - Indicates whether the instance type
--     has local instance storage (@true@ | @false@).
--
-- -   @instance-type@ - The instance type (for example @c5.2xlarge@ or
--     c5*).
--
-- -   @memory-info.size-in-mib@ - The memory size.
--
-- -   @network-info.efa-info.maximum-efa-interfaces@ - The maximum number
--     of Elastic Fabric Adapters (EFAs) per instance.
--
-- -   @network-info.efa-supported@ - Indicates whether the instance type
--     supports Elastic Fabric Adapter (EFA) (@true@ | @false@).
--
-- -   @network-info.ena-support@ - Indicates whether Elastic Network
--     Adapter (ENA) is supported or required (@required@ | @supported@ |
--     @unsupported@).
--
-- -   @network-info.encryption-in-transit-supported@ - Indicates whether
--     the instance type automatically encrypts in-transit traffic between
--     instances (@true@ | @false@).
--
-- -   @network-info.ipv4-addresses-per-interface@ - The maximum number of
--     private IPv4 addresses per network interface.
--
-- -   @network-info.ipv6-addresses-per-interface@ - The maximum number of
--     private IPv6 addresses per network interface.
--
-- -   @network-info.ipv6-supported@ - Indicates whether the instance type
--     supports IPv6 (@true@ | @false@).
--
-- -   @network-info.maximum-network-cards@ - The maximum number of network
--     cards per instance.
--
-- -   @network-info.maximum-network-interfaces@ - The maximum number of
--     network interfaces per instance.
--
-- -   @network-info.network-performance@ - The network performance (for
--     example, \"25 Gigabit\").
--
-- -   @processor-info.supported-architecture@ - The CPU architecture
--     (@arm64@ | @i386@ | @x86_64@).
--
-- -   @processor-info.sustained-clock-speed-in-ghz@ - The CPU clock speed,
--     in GHz.
--
-- -   @supported-boot-mode@ - The boot mode (@legacy-bios@ | @uefi@).
--
-- -   @supported-root-device-type@ - The root device type (@ebs@ |
--     @instance-store@).
--
-- -   @supported-usage-class@ - The usage class (@on-demand@ | @spot@).
--
-- -   @supported-virtualization-type@ - The virtualization type (@hvm@ |
--     @paravirtual@).
--
-- -   @vcpu-info.default-cores@ - The default number of cores for the
--     instance type.
--
-- -   @vcpu-info.default-threads-per-core@ - The default number of threads
--     per core for the instance type.
--
-- -   @vcpu-info.default-vcpus@ - The default number of vCPUs for the
--     instance type.
--
-- -   @vcpu-info.valid-cores@ - The number of cores that can be configured
--     for the instance type.
--
-- -   @vcpu-info.valid-threads-per-core@ - The number of threads per core
--     that can be configured for the instance type. For example, \"1\" or
--     \"1,2\".
--
-- 'dryRun', 'describeInstanceTypes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeInstanceTypes_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the next token value.
newDescribeInstanceTypes ::
  DescribeInstanceTypes
newDescribeInstanceTypes =
  DescribeInstanceTypes'
    { nextToken = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
describeInstanceTypes_nextToken :: Lens.Lens' DescribeInstanceTypes (Prelude.Maybe Prelude.Text)
describeInstanceTypes_nextToken = Lens.lens (\DescribeInstanceTypes' {nextToken} -> nextToken) (\s@DescribeInstanceTypes' {} a -> s {nextToken = a} :: DescribeInstanceTypes)

-- | The instance types. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
describeInstanceTypes_instanceTypes :: Lens.Lens' DescribeInstanceTypes (Prelude.Maybe [InstanceType])
describeInstanceTypes_instanceTypes = Lens.lens (\DescribeInstanceTypes' {instanceTypes} -> instanceTypes) (\s@DescribeInstanceTypes' {} a -> s {instanceTypes = a} :: DescribeInstanceTypes) Prelude.. Lens.mapping Lens.coerced

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @auto-recovery-supported@ - Indicates whether auto recovery is
--     supported (@true@ | @false@).
--
-- -   @bare-metal@ - Indicates whether it is a bare metal instance type
--     (@true@ | @false@).
--
-- -   @burstable-performance-supported@ - Indicates whether it is a
--     burstable performance instance type (@true@ | @false@).
--
-- -   @current-generation@ - Indicates whether this instance type is the
--     latest generation instance type of an instance family (@true@ |
--     @false@).
--
-- -   @ebs-info.ebs-optimized-info.baseline-bandwidth-in-mbps@ - The
--     baseline bandwidth performance for an EBS-optimized instance type,
--     in Mbps.
--
-- -   @ebs-info.ebs-optimized-info.baseline-iops@ - The baseline
--     input\/output storage operations per second for an EBS-optimized
--     instance type.
--
-- -   @ebs-info.ebs-optimized-info.baseline-throughput-in-mbps@ - The
--     baseline throughput performance for an EBS-optimized instance type,
--     in MB\/s.
--
-- -   @ebs-info.ebs-optimized-info.maximum-bandwidth-in-mbps@ - The
--     maximum bandwidth performance for an EBS-optimized instance type, in
--     Mbps.
--
-- -   @ebs-info.ebs-optimized-info.maximum-iops@ - The maximum
--     input\/output storage operations per second for an EBS-optimized
--     instance type.
--
-- -   @ebs-info.ebs-optimized-info.maximum-throughput-in-mbps@ - The
--     maximum throughput performance for an EBS-optimized instance type,
--     in MB\/s.
--
-- -   @ebs-info.ebs-optimized-support@ - Indicates whether the instance
--     type is EBS-optimized (@supported@ | @unsupported@ | @default@).
--
-- -   @ebs-info.encryption-support@ - Indicates whether EBS encryption is
--     supported (@supported@ | @unsupported@).
--
-- -   @ebs-info.nvme-support@ - Indicates whether non-volatile memory
--     express (NVMe) is supported for EBS volumes (@required@ |
--     @supported@ | @unsupported@).
--
-- -   @free-tier-eligible@ - Indicates whether the instance type is
--     eligible to use in the free tier (@true@ | @false@).
--
-- -   @hibernation-supported@ - Indicates whether On-Demand hibernation is
--     supported (@true@ | @false@).
--
-- -   @hypervisor@ - The hypervisor (@nitro@ | @xen@).
--
-- -   @instance-storage-info.disk.count@ - The number of local disks.
--
-- -   @instance-storage-info.disk.size-in-gb@ - The storage size of each
--     instance storage disk, in GB.
--
-- -   @instance-storage-info.disk.type@ - The storage technology for the
--     local instance storage disks (@hdd@ | @ssd@).
--
-- -   @instance-storage-info.encryption-support@ - Indicates whether data
--     is encrypted at rest (@required@ | @supported@ | @unsupported@).
--
-- -   @instance-storage-info.nvme-support@ - Indicates whether
--     non-volatile memory express (NVMe) is supported for instance store
--     (@required@ | @supported@ | @unsupported@).
--
-- -   @instance-storage-info.total-size-in-gb@ - The total amount of
--     storage available from all local instance storage, in GB.
--
-- -   @instance-storage-supported@ - Indicates whether the instance type
--     has local instance storage (@true@ | @false@).
--
-- -   @instance-type@ - The instance type (for example @c5.2xlarge@ or
--     c5*).
--
-- -   @memory-info.size-in-mib@ - The memory size.
--
-- -   @network-info.efa-info.maximum-efa-interfaces@ - The maximum number
--     of Elastic Fabric Adapters (EFAs) per instance.
--
-- -   @network-info.efa-supported@ - Indicates whether the instance type
--     supports Elastic Fabric Adapter (EFA) (@true@ | @false@).
--
-- -   @network-info.ena-support@ - Indicates whether Elastic Network
--     Adapter (ENA) is supported or required (@required@ | @supported@ |
--     @unsupported@).
--
-- -   @network-info.encryption-in-transit-supported@ - Indicates whether
--     the instance type automatically encrypts in-transit traffic between
--     instances (@true@ | @false@).
--
-- -   @network-info.ipv4-addresses-per-interface@ - The maximum number of
--     private IPv4 addresses per network interface.
--
-- -   @network-info.ipv6-addresses-per-interface@ - The maximum number of
--     private IPv6 addresses per network interface.
--
-- -   @network-info.ipv6-supported@ - Indicates whether the instance type
--     supports IPv6 (@true@ | @false@).
--
-- -   @network-info.maximum-network-cards@ - The maximum number of network
--     cards per instance.
--
-- -   @network-info.maximum-network-interfaces@ - The maximum number of
--     network interfaces per instance.
--
-- -   @network-info.network-performance@ - The network performance (for
--     example, \"25 Gigabit\").
--
-- -   @processor-info.supported-architecture@ - The CPU architecture
--     (@arm64@ | @i386@ | @x86_64@).
--
-- -   @processor-info.sustained-clock-speed-in-ghz@ - The CPU clock speed,
--     in GHz.
--
-- -   @supported-boot-mode@ - The boot mode (@legacy-bios@ | @uefi@).
--
-- -   @supported-root-device-type@ - The root device type (@ebs@ |
--     @instance-store@).
--
-- -   @supported-usage-class@ - The usage class (@on-demand@ | @spot@).
--
-- -   @supported-virtualization-type@ - The virtualization type (@hvm@ |
--     @paravirtual@).
--
-- -   @vcpu-info.default-cores@ - The default number of cores for the
--     instance type.
--
-- -   @vcpu-info.default-threads-per-core@ - The default number of threads
--     per core for the instance type.
--
-- -   @vcpu-info.default-vcpus@ - The default number of vCPUs for the
--     instance type.
--
-- -   @vcpu-info.valid-cores@ - The number of cores that can be configured
--     for the instance type.
--
-- -   @vcpu-info.valid-threads-per-core@ - The number of threads per core
--     that can be configured for the instance type. For example, \"1\" or
--     \"1,2\".
describeInstanceTypes_filters :: Lens.Lens' DescribeInstanceTypes (Prelude.Maybe [Filter])
describeInstanceTypes_filters = Lens.lens (\DescribeInstanceTypes' {filters} -> filters) (\s@DescribeInstanceTypes' {} a -> s {filters = a} :: DescribeInstanceTypes) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceTypes_dryRun :: Lens.Lens' DescribeInstanceTypes (Prelude.Maybe Prelude.Bool)
describeInstanceTypes_dryRun = Lens.lens (\DescribeInstanceTypes' {dryRun} -> dryRun) (\s@DescribeInstanceTypes' {} a -> s {dryRun = a} :: DescribeInstanceTypes)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the next token value.
describeInstanceTypes_maxResults :: Lens.Lens' DescribeInstanceTypes (Prelude.Maybe Prelude.Natural)
describeInstanceTypes_maxResults = Lens.lens (\DescribeInstanceTypes' {maxResults} -> maxResults) (\s@DescribeInstanceTypes' {} a -> s {maxResults = a} :: DescribeInstanceTypes)

instance Core.AWSPager DescribeInstanceTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstanceTypesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstanceTypesResponse_instanceTypes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstanceTypes_nextToken
          Lens..~ rs
          Lens.^? describeInstanceTypesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstanceTypes where
  type
    AWSResponse DescribeInstanceTypes =
      DescribeInstanceTypesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceTypesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "instanceTypeSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceTypes where
  hashWithSalt _salt DescribeInstanceTypes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceTypes
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeInstanceTypes where
  rnf DescribeInstanceTypes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeInstanceTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInstanceTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInstanceTypes where
  toQuery DescribeInstanceTypes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeInstanceTypes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "InstanceType"
              Prelude.<$> instanceTypes
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeInstanceTypesResponse' smart constructor.
data DescribeInstanceTypesResponse = DescribeInstanceTypesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    instanceTypes :: Prelude.Maybe [InstanceTypeInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceTypesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'instanceTypes', 'describeInstanceTypesResponse_instanceTypes' - The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- 'httpStatus', 'describeInstanceTypesResponse_httpStatus' - The response's http status code.
newDescribeInstanceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceTypesResponse
newDescribeInstanceTypesResponse pHttpStatus_ =
  DescribeInstanceTypesResponse'
    { nextToken =
        Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeInstanceTypesResponse_nextToken :: Lens.Lens' DescribeInstanceTypesResponse (Prelude.Maybe Prelude.Text)
describeInstanceTypesResponse_nextToken = Lens.lens (\DescribeInstanceTypesResponse' {nextToken} -> nextToken) (\s@DescribeInstanceTypesResponse' {} a -> s {nextToken = a} :: DescribeInstanceTypesResponse)

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
describeInstanceTypesResponse_instanceTypes :: Lens.Lens' DescribeInstanceTypesResponse (Prelude.Maybe [InstanceTypeInfo])
describeInstanceTypesResponse_instanceTypes = Lens.lens (\DescribeInstanceTypesResponse' {instanceTypes} -> instanceTypes) (\s@DescribeInstanceTypesResponse' {} a -> s {instanceTypes = a} :: DescribeInstanceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstanceTypesResponse_httpStatus :: Lens.Lens' DescribeInstanceTypesResponse Prelude.Int
describeInstanceTypesResponse_httpStatus = Lens.lens (\DescribeInstanceTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceTypesResponse' {} a -> s {httpStatus = a} :: DescribeInstanceTypesResponse)

instance Prelude.NFData DescribeInstanceTypesResponse where
  rnf DescribeInstanceTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf httpStatus
