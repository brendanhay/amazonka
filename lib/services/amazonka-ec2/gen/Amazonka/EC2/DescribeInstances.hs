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
-- Module      : Amazonka.EC2.DescribeInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified instances or all instances.
--
-- If you specify instance IDs, the output includes information for only
-- the specified instances. If you specify filters, the output includes
-- information for only those instances that meet the filter criteria. If
-- you do not specify instance IDs or filters, the output includes
-- information for all instances, which can affect performance. We
-- recommend that you use pagination to ensure that the operation returns
-- quickly and successfully.
--
-- If you specify an instance ID that is not valid, an error is returned.
-- If you specify an instance that you do not own, it is not included in
-- the output.
--
-- Recently terminated instances might appear in the returned results. This
-- interval is usually less than one hour.
--
-- If you describe instances in the rare case where an Availability Zone is
-- experiencing a service disruption and you specify instance IDs that are
-- in the affected zone, or do not specify any instance IDs at all, the
-- call fails. If you describe instances and specify only instance IDs that
-- are in an unaffected zone, the call works normally.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeInstances
  ( -- * Creating a Request
    DescribeInstances (..),
    newDescribeInstances,

    -- * Request Lenses
    describeInstances_dryRun,
    describeInstances_filters,
    describeInstances_instanceIds,
    describeInstances_maxResults,
    describeInstances_nextToken,

    -- * Destructuring the Response
    DescribeInstancesResponse (..),
    newDescribeInstancesResponse,

    -- * Response Lenses
    describeInstancesResponse_nextToken,
    describeInstancesResponse_reservations,
    describeInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @affinity@ - The affinity setting for an instance running on a
    --     Dedicated Host (@default@ | @host@).
    --
    -- -   @architecture@ - The instance architecture (@i386@ | @x86_64@ |
    --     @arm64@).
    --
    -- -   @availability-zone@ - The Availability Zone of the instance.
    --
    -- -   @block-device-mapping.attach-time@ - The attach time for an EBS
    --     volume mapped to the instance, for example,
    --     @2010-09-15T17:15:20.000Z@.
    --
    -- -   @block-device-mapping.delete-on-termination@ - A Boolean that
    --     indicates whether the EBS volume is deleted on instance termination.
    --
    -- -   @block-device-mapping.device-name@ - The device name specified in
    --     the block device mapping (for example, @\/dev\/sdh@ or @xvdh@).
    --
    -- -   @block-device-mapping.status@ - The status for the EBS volume
    --     (@attaching@ | @attached@ | @detaching@ | @detached@).
    --
    -- -   @block-device-mapping.volume-id@ - The volume ID of the EBS volume.
    --
    -- -   @capacity-reservation-id@ - The ID of the Capacity Reservation into
    --     which the instance was launched.
    --
    -- -   @client-token@ - The idempotency token you provided when you
    --     launched the instance.
    --
    -- -   @dns-name@ - The public DNS name of the instance.
    --
    -- -   @group-id@ - The ID of the security group for the instance.
    --     EC2-Classic only.
    --
    -- -   @group-name@ - The name of the security group for the instance.
    --     EC2-Classic only.
    --
    -- -   @hibernation-options.configured@ - A Boolean that indicates whether
    --     the instance is enabled for hibernation. A value of @true@ means
    --     that the instance is enabled for hibernation.
    --
    -- -   @host-id@ - The ID of the Dedicated Host on which the instance is
    --     running, if applicable.
    --
    -- -   @hypervisor@ - The hypervisor type of the instance (@ovm@ | @xen@).
    --     The value @xen@ is used for both Xen and Nitro hypervisors.
    --
    -- -   @iam-instance-profile.arn@ - The instance profile associated with
    --     the instance. Specified as an ARN.
    --
    -- -   @image-id@ - The ID of the image used to launch the instance.
    --
    -- -   @instance-id@ - The ID of the instance.
    --
    -- -   @instance-lifecycle@ - Indicates whether this is a Spot Instance or
    --     a Scheduled Instance (@spot@ | @scheduled@).
    --
    -- -   @instance-state-code@ - The state of the instance, as a 16-bit
    --     unsigned integer. The high byte is used for internal purposes and
    --     should be ignored. The low byte is set based on the state
    --     represented. The valid values are: 0 (pending), 16 (running), 32
    --     (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
    --
    -- -   @instance-state-name@ - The state of the instance (@pending@ |
    --     @running@ | @shutting-down@ | @terminated@ | @stopping@ |
    --     @stopped@).
    --
    -- -   @instance-type@ - The type of instance (for example, @t2.micro@).
    --
    -- -   @instance.group-id@ - The ID of the security group for the instance.
    --
    -- -   @instance.group-name@ - The name of the security group for the
    --     instance.
    --
    -- -   @ip-address@ - The public IPv4 address of the instance.
    --
    -- -   @kernel-id@ - The kernel ID.
    --
    -- -   @key-name@ - The name of the key pair used when the instance was
    --     launched.
    --
    -- -   @launch-index@ - When launching multiple instances, this is the
    --     index for the instance in the launch group (for example, 0, 1, 2,
    --     and so on).
    --
    -- -   @launch-time@ - The time when the instance was launched, in the ISO
    --     8601 format in the UTC time zone (YYYY-MM-DDThh:mm:ss.sssZ), for
    --     example, @2021-09-29T11:04:43.305Z@. You can use a wildcard (@*@),
    --     for example, @2021-09-29T*@, which matches an entire day.
    --
    -- -   @metadata-options.http-tokens@ - The metadata request authorization
    --     state (@optional@ | @required@)
    --
    -- -   @metadata-options.http-put-response-hop-limit@ - The http metadata
    --     request put response hop limit (integer, possible values @1@ to
    --     @64@)
    --
    -- -   @metadata-options.http-endpoint@ - Enable or disable metadata access
    --     on http endpoint (@enabled@ | @disabled@)
    --
    -- -   @monitoring-state@ - Indicates whether detailed monitoring is
    --     enabled (@disabled@ | @enabled@).
    --
    -- -   @network-interface.addresses.private-ip-address@ - The private IPv4
    --     address associated with the network interface.
    --
    -- -   @network-interface.addresses.primary@ - Specifies whether the IPv4
    --     address of the network interface is the primary private IPv4
    --     address.
    --
    -- -   @network-interface.addresses.association.public-ip@ - The ID of the
    --     association of an Elastic IP address (IPv4) with a network
    --     interface.
    --
    -- -   @network-interface.addresses.association.ip-owner-id@ - The owner ID
    --     of the private IPv4 address associated with the network interface.
    --
    -- -   @network-interface.association.public-ip@ - The address of the
    --     Elastic IP address (IPv4) bound to the network interface.
    --
    -- -   @network-interface.association.ip-owner-id@ - The owner of the
    --     Elastic IP address (IPv4) associated with the network interface.
    --
    -- -   @network-interface.association.allocation-id@ - The allocation ID
    --     returned when you allocated the Elastic IP address (IPv4) for your
    --     network interface.
    --
    -- -   @network-interface.association.association-id@ - The association ID
    --     returned when the network interface was associated with an IPv4
    --     address.
    --
    -- -   @network-interface.attachment.attachment-id@ - The ID of the
    --     interface attachment.
    --
    -- -   @network-interface.attachment.instance-id@ - The ID of the instance
    --     to which the network interface is attached.
    --
    -- -   @network-interface.attachment.instance-owner-id@ - The owner ID of
    --     the instance to which the network interface is attached.
    --
    -- -   @network-interface.attachment.device-index@ - The device index to
    --     which the network interface is attached.
    --
    -- -   @network-interface.attachment.status@ - The status of the attachment
    --     (@attaching@ | @attached@ | @detaching@ | @detached@).
    --
    -- -   @network-interface.attachment.attach-time@ - The time that the
    --     network interface was attached to an instance.
    --
    -- -   @network-interface.attachment.delete-on-termination@ - Specifies
    --     whether the attachment is deleted when an instance is terminated.
    --
    -- -   @network-interface.availability-zone@ - The Availability Zone for
    --     the network interface.
    --
    -- -   @network-interface.description@ - The description of the network
    --     interface.
    --
    -- -   @network-interface.group-id@ - The ID of a security group associated
    --     with the network interface.
    --
    -- -   @network-interface.group-name@ - The name of a security group
    --     associated with the network interface.
    --
    -- -   @network-interface.ipv6-addresses.ipv6-address@ - The IPv6 address
    --     associated with the network interface.
    --
    -- -   @network-interface.mac-address@ - The MAC address of the network
    --     interface.
    --
    -- -   @network-interface.network-interface-id@ - The ID of the network
    --     interface.
    --
    -- -   @network-interface.owner-id@ - The ID of the owner of the network
    --     interface.
    --
    -- -   @network-interface.private-dns-name@ - The private DNS name of the
    --     network interface.
    --
    -- -   @network-interface.requester-id@ - The requester ID for the network
    --     interface.
    --
    -- -   @network-interface.requester-managed@ - Indicates whether the
    --     network interface is being managed by Amazon Web Services.
    --
    -- -   @network-interface.status@ - The status of the network interface
    --     (@available@) | @in-use@).
    --
    -- -   @network-interface.source-dest-check@ - Whether the network
    --     interface performs source\/destination checking. A value of @true@
    --     means that checking is enabled, and @false@ means that checking is
    --     disabled. The value must be @false@ for the network interface to
    --     perform network address translation (NAT) in your VPC.
    --
    -- -   @network-interface.subnet-id@ - The ID of the subnet for the network
    --     interface.
    --
    -- -   @network-interface.vpc-id@ - The ID of the VPC for the network
    --     interface.
    --
    -- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
    --
    -- -   @owner-id@ - The Amazon Web Services account ID of the instance
    --     owner.
    --
    -- -   @placement-group-name@ - The name of the placement group for the
    --     instance.
    --
    -- -   @placement-partition-number@ - The partition in which the instance
    --     is located.
    --
    -- -   @platform@ - The platform. To list only Windows instances, use
    --     @windows@.
    --
    -- -   @private-dns-name@ - The private IPv4 DNS name of the instance.
    --
    -- -   @private-ip-address@ - The private IPv4 address of the instance.
    --
    -- -   @product-code@ - The product code associated with the AMI used to
    --     launch the instance.
    --
    -- -   @product-code.type@ - The type of product code (@devpay@ |
    --     @marketplace@).
    --
    -- -   @ramdisk-id@ - The RAM disk ID.
    --
    -- -   @reason@ - The reason for the current state of the instance (for
    --     example, shows \"User Initiated [date]\" when you stop or terminate
    --     the instance). Similar to the state-reason-code filter.
    --
    -- -   @requester-id@ - The ID of the entity that launched the instance on
    --     your behalf (for example, Amazon Web Services Management Console,
    --     Auto Scaling, and so on).
    --
    -- -   @reservation-id@ - The ID of the instance\'s reservation. A
    --     reservation ID is created any time you launch an instance. A
    --     reservation ID has a one-to-one relationship with an instance launch
    --     request, but can be associated with more than one instance if you
    --     launch multiple instances using the same launch request. For
    --     example, if you launch one instance, you get one reservation ID. If
    --     you launch ten instances using the same launch request, you also get
    --     one reservation ID.
    --
    -- -   @root-device-name@ - The device name of the root device volume (for
    --     example, @\/dev\/sda1@).
    --
    -- -   @root-device-type@ - The type of the root device volume (@ebs@ |
    --     @instance-store@).
    --
    -- -   @source-dest-check@ - Indicates whether the instance performs
    --     source\/destination checking. A value of @true@ means that checking
    --     is enabled, and @false@ means that checking is disabled. The value
    --     must be @false@ for the instance to perform network address
    --     translation (NAT) in your VPC.
    --
    -- -   @spot-instance-request-id@ - The ID of the Spot Instance request.
    --
    -- -   @state-reason-code@ - The reason code for the state change.
    --
    -- -   @state-reason-message@ - A message that describes the state change.
    --
    -- -   @subnet-id@ - The ID of the subnet for the instance.
    --
    -- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources that have a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @tenancy@ - The tenancy of an instance (@dedicated@ | @default@ |
    --     @host@).
    --
    -- -   @virtualization-type@ - The virtualization type of the instance
    --     (@paravirtual@ | @hvm@).
    --
    -- -   @vpc-id@ - The ID of the VPC that the instance is running in.
    filters :: Prelude.Maybe [Filter],
    -- | The instance IDs.
    --
    -- Default: Describes all your instances.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 5 and 1000. You cannot specify this
    -- parameter and the instance IDs parameter in the same call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeInstances_filters' - The filters.
--
-- -   @affinity@ - The affinity setting for an instance running on a
--     Dedicated Host (@default@ | @host@).
--
-- -   @architecture@ - The instance architecture (@i386@ | @x86_64@ |
--     @arm64@).
--
-- -   @availability-zone@ - The Availability Zone of the instance.
--
-- -   @block-device-mapping.attach-time@ - The attach time for an EBS
--     volume mapped to the instance, for example,
--     @2010-09-15T17:15:20.000Z@.
--
-- -   @block-device-mapping.delete-on-termination@ - A Boolean that
--     indicates whether the EBS volume is deleted on instance termination.
--
-- -   @block-device-mapping.device-name@ - The device name specified in
--     the block device mapping (for example, @\/dev\/sdh@ or @xvdh@).
--
-- -   @block-device-mapping.status@ - The status for the EBS volume
--     (@attaching@ | @attached@ | @detaching@ | @detached@).
--
-- -   @block-device-mapping.volume-id@ - The volume ID of the EBS volume.
--
-- -   @capacity-reservation-id@ - The ID of the Capacity Reservation into
--     which the instance was launched.
--
-- -   @client-token@ - The idempotency token you provided when you
--     launched the instance.
--
-- -   @dns-name@ - The public DNS name of the instance.
--
-- -   @group-id@ - The ID of the security group for the instance.
--     EC2-Classic only.
--
-- -   @group-name@ - The name of the security group for the instance.
--     EC2-Classic only.
--
-- -   @hibernation-options.configured@ - A Boolean that indicates whether
--     the instance is enabled for hibernation. A value of @true@ means
--     that the instance is enabled for hibernation.
--
-- -   @host-id@ - The ID of the Dedicated Host on which the instance is
--     running, if applicable.
--
-- -   @hypervisor@ - The hypervisor type of the instance (@ovm@ | @xen@).
--     The value @xen@ is used for both Xen and Nitro hypervisors.
--
-- -   @iam-instance-profile.arn@ - The instance profile associated with
--     the instance. Specified as an ARN.
--
-- -   @image-id@ - The ID of the image used to launch the instance.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @instance-lifecycle@ - Indicates whether this is a Spot Instance or
--     a Scheduled Instance (@spot@ | @scheduled@).
--
-- -   @instance-state-code@ - The state of the instance, as a 16-bit
--     unsigned integer. The high byte is used for internal purposes and
--     should be ignored. The low byte is set based on the state
--     represented. The valid values are: 0 (pending), 16 (running), 32
--     (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
--
-- -   @instance-state-name@ - The state of the instance (@pending@ |
--     @running@ | @shutting-down@ | @terminated@ | @stopping@ |
--     @stopped@).
--
-- -   @instance-type@ - The type of instance (for example, @t2.micro@).
--
-- -   @instance.group-id@ - The ID of the security group for the instance.
--
-- -   @instance.group-name@ - The name of the security group for the
--     instance.
--
-- -   @ip-address@ - The public IPv4 address of the instance.
--
-- -   @kernel-id@ - The kernel ID.
--
-- -   @key-name@ - The name of the key pair used when the instance was
--     launched.
--
-- -   @launch-index@ - When launching multiple instances, this is the
--     index for the instance in the launch group (for example, 0, 1, 2,
--     and so on).
--
-- -   @launch-time@ - The time when the instance was launched, in the ISO
--     8601 format in the UTC time zone (YYYY-MM-DDThh:mm:ss.sssZ), for
--     example, @2021-09-29T11:04:43.305Z@. You can use a wildcard (@*@),
--     for example, @2021-09-29T*@, which matches an entire day.
--
-- -   @metadata-options.http-tokens@ - The metadata request authorization
--     state (@optional@ | @required@)
--
-- -   @metadata-options.http-put-response-hop-limit@ - The http metadata
--     request put response hop limit (integer, possible values @1@ to
--     @64@)
--
-- -   @metadata-options.http-endpoint@ - Enable or disable metadata access
--     on http endpoint (@enabled@ | @disabled@)
--
-- -   @monitoring-state@ - Indicates whether detailed monitoring is
--     enabled (@disabled@ | @enabled@).
--
-- -   @network-interface.addresses.private-ip-address@ - The private IPv4
--     address associated with the network interface.
--
-- -   @network-interface.addresses.primary@ - Specifies whether the IPv4
--     address of the network interface is the primary private IPv4
--     address.
--
-- -   @network-interface.addresses.association.public-ip@ - The ID of the
--     association of an Elastic IP address (IPv4) with a network
--     interface.
--
-- -   @network-interface.addresses.association.ip-owner-id@ - The owner ID
--     of the private IPv4 address associated with the network interface.
--
-- -   @network-interface.association.public-ip@ - The address of the
--     Elastic IP address (IPv4) bound to the network interface.
--
-- -   @network-interface.association.ip-owner-id@ - The owner of the
--     Elastic IP address (IPv4) associated with the network interface.
--
-- -   @network-interface.association.allocation-id@ - The allocation ID
--     returned when you allocated the Elastic IP address (IPv4) for your
--     network interface.
--
-- -   @network-interface.association.association-id@ - The association ID
--     returned when the network interface was associated with an IPv4
--     address.
--
-- -   @network-interface.attachment.attachment-id@ - The ID of the
--     interface attachment.
--
-- -   @network-interface.attachment.instance-id@ - The ID of the instance
--     to which the network interface is attached.
--
-- -   @network-interface.attachment.instance-owner-id@ - The owner ID of
--     the instance to which the network interface is attached.
--
-- -   @network-interface.attachment.device-index@ - The device index to
--     which the network interface is attached.
--
-- -   @network-interface.attachment.status@ - The status of the attachment
--     (@attaching@ | @attached@ | @detaching@ | @detached@).
--
-- -   @network-interface.attachment.attach-time@ - The time that the
--     network interface was attached to an instance.
--
-- -   @network-interface.attachment.delete-on-termination@ - Specifies
--     whether the attachment is deleted when an instance is terminated.
--
-- -   @network-interface.availability-zone@ - The Availability Zone for
--     the network interface.
--
-- -   @network-interface.description@ - The description of the network
--     interface.
--
-- -   @network-interface.group-id@ - The ID of a security group associated
--     with the network interface.
--
-- -   @network-interface.group-name@ - The name of a security group
--     associated with the network interface.
--
-- -   @network-interface.ipv6-addresses.ipv6-address@ - The IPv6 address
--     associated with the network interface.
--
-- -   @network-interface.mac-address@ - The MAC address of the network
--     interface.
--
-- -   @network-interface.network-interface-id@ - The ID of the network
--     interface.
--
-- -   @network-interface.owner-id@ - The ID of the owner of the network
--     interface.
--
-- -   @network-interface.private-dns-name@ - The private DNS name of the
--     network interface.
--
-- -   @network-interface.requester-id@ - The requester ID for the network
--     interface.
--
-- -   @network-interface.requester-managed@ - Indicates whether the
--     network interface is being managed by Amazon Web Services.
--
-- -   @network-interface.status@ - The status of the network interface
--     (@available@) | @in-use@).
--
-- -   @network-interface.source-dest-check@ - Whether the network
--     interface performs source\/destination checking. A value of @true@
--     means that checking is enabled, and @false@ means that checking is
--     disabled. The value must be @false@ for the network interface to
--     perform network address translation (NAT) in your VPC.
--
-- -   @network-interface.subnet-id@ - The ID of the subnet for the network
--     interface.
--
-- -   @network-interface.vpc-id@ - The ID of the VPC for the network
--     interface.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @owner-id@ - The Amazon Web Services account ID of the instance
--     owner.
--
-- -   @placement-group-name@ - The name of the placement group for the
--     instance.
--
-- -   @placement-partition-number@ - The partition in which the instance
--     is located.
--
-- -   @platform@ - The platform. To list only Windows instances, use
--     @windows@.
--
-- -   @private-dns-name@ - The private IPv4 DNS name of the instance.
--
-- -   @private-ip-address@ - The private IPv4 address of the instance.
--
-- -   @product-code@ - The product code associated with the AMI used to
--     launch the instance.
--
-- -   @product-code.type@ - The type of product code (@devpay@ |
--     @marketplace@).
--
-- -   @ramdisk-id@ - The RAM disk ID.
--
-- -   @reason@ - The reason for the current state of the instance (for
--     example, shows \"User Initiated [date]\" when you stop or terminate
--     the instance). Similar to the state-reason-code filter.
--
-- -   @requester-id@ - The ID of the entity that launched the instance on
--     your behalf (for example, Amazon Web Services Management Console,
--     Auto Scaling, and so on).
--
-- -   @reservation-id@ - The ID of the instance\'s reservation. A
--     reservation ID is created any time you launch an instance. A
--     reservation ID has a one-to-one relationship with an instance launch
--     request, but can be associated with more than one instance if you
--     launch multiple instances using the same launch request. For
--     example, if you launch one instance, you get one reservation ID. If
--     you launch ten instances using the same launch request, you also get
--     one reservation ID.
--
-- -   @root-device-name@ - The device name of the root device volume (for
--     example, @\/dev\/sda1@).
--
-- -   @root-device-type@ - The type of the root device volume (@ebs@ |
--     @instance-store@).
--
-- -   @source-dest-check@ - Indicates whether the instance performs
--     source\/destination checking. A value of @true@ means that checking
--     is enabled, and @false@ means that checking is disabled. The value
--     must be @false@ for the instance to perform network address
--     translation (NAT) in your VPC.
--
-- -   @spot-instance-request-id@ - The ID of the Spot Instance request.
--
-- -   @state-reason-code@ - The reason code for the state change.
--
-- -   @state-reason-message@ - A message that describes the state change.
--
-- -   @subnet-id@ - The ID of the subnet for the instance.
--
-- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific key,
--     regardless of the tag value.
--
-- -   @tenancy@ - The tenancy of an instance (@dedicated@ | @default@ |
--     @host@).
--
-- -   @virtualization-type@ - The virtualization type of the instance
--     (@paravirtual@ | @hvm@).
--
-- -   @vpc-id@ - The ID of the VPC that the instance is running in.
--
-- 'instanceIds', 'describeInstances_instanceIds' - The instance IDs.
--
-- Default: Describes all your instances.
--
-- 'maxResults', 'describeInstances_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000. You cannot specify this
-- parameter and the instance IDs parameter in the same call.
--
-- 'nextToken', 'describeInstances_nextToken' - The token to request the next page of results.
newDescribeInstances ::
  DescribeInstances
newDescribeInstances =
  DescribeInstances'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstances_dryRun :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Bool)
describeInstances_dryRun = Lens.lens (\DescribeInstances' {dryRun} -> dryRun) (\s@DescribeInstances' {} a -> s {dryRun = a} :: DescribeInstances)

-- | The filters.
--
-- -   @affinity@ - The affinity setting for an instance running on a
--     Dedicated Host (@default@ | @host@).
--
-- -   @architecture@ - The instance architecture (@i386@ | @x86_64@ |
--     @arm64@).
--
-- -   @availability-zone@ - The Availability Zone of the instance.
--
-- -   @block-device-mapping.attach-time@ - The attach time for an EBS
--     volume mapped to the instance, for example,
--     @2010-09-15T17:15:20.000Z@.
--
-- -   @block-device-mapping.delete-on-termination@ - A Boolean that
--     indicates whether the EBS volume is deleted on instance termination.
--
-- -   @block-device-mapping.device-name@ - The device name specified in
--     the block device mapping (for example, @\/dev\/sdh@ or @xvdh@).
--
-- -   @block-device-mapping.status@ - The status for the EBS volume
--     (@attaching@ | @attached@ | @detaching@ | @detached@).
--
-- -   @block-device-mapping.volume-id@ - The volume ID of the EBS volume.
--
-- -   @capacity-reservation-id@ - The ID of the Capacity Reservation into
--     which the instance was launched.
--
-- -   @client-token@ - The idempotency token you provided when you
--     launched the instance.
--
-- -   @dns-name@ - The public DNS name of the instance.
--
-- -   @group-id@ - The ID of the security group for the instance.
--     EC2-Classic only.
--
-- -   @group-name@ - The name of the security group for the instance.
--     EC2-Classic only.
--
-- -   @hibernation-options.configured@ - A Boolean that indicates whether
--     the instance is enabled for hibernation. A value of @true@ means
--     that the instance is enabled for hibernation.
--
-- -   @host-id@ - The ID of the Dedicated Host on which the instance is
--     running, if applicable.
--
-- -   @hypervisor@ - The hypervisor type of the instance (@ovm@ | @xen@).
--     The value @xen@ is used for both Xen and Nitro hypervisors.
--
-- -   @iam-instance-profile.arn@ - The instance profile associated with
--     the instance. Specified as an ARN.
--
-- -   @image-id@ - The ID of the image used to launch the instance.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @instance-lifecycle@ - Indicates whether this is a Spot Instance or
--     a Scheduled Instance (@spot@ | @scheduled@).
--
-- -   @instance-state-code@ - The state of the instance, as a 16-bit
--     unsigned integer. The high byte is used for internal purposes and
--     should be ignored. The low byte is set based on the state
--     represented. The valid values are: 0 (pending), 16 (running), 32
--     (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
--
-- -   @instance-state-name@ - The state of the instance (@pending@ |
--     @running@ | @shutting-down@ | @terminated@ | @stopping@ |
--     @stopped@).
--
-- -   @instance-type@ - The type of instance (for example, @t2.micro@).
--
-- -   @instance.group-id@ - The ID of the security group for the instance.
--
-- -   @instance.group-name@ - The name of the security group for the
--     instance.
--
-- -   @ip-address@ - The public IPv4 address of the instance.
--
-- -   @kernel-id@ - The kernel ID.
--
-- -   @key-name@ - The name of the key pair used when the instance was
--     launched.
--
-- -   @launch-index@ - When launching multiple instances, this is the
--     index for the instance in the launch group (for example, 0, 1, 2,
--     and so on).
--
-- -   @launch-time@ - The time when the instance was launched, in the ISO
--     8601 format in the UTC time zone (YYYY-MM-DDThh:mm:ss.sssZ), for
--     example, @2021-09-29T11:04:43.305Z@. You can use a wildcard (@*@),
--     for example, @2021-09-29T*@, which matches an entire day.
--
-- -   @metadata-options.http-tokens@ - The metadata request authorization
--     state (@optional@ | @required@)
--
-- -   @metadata-options.http-put-response-hop-limit@ - The http metadata
--     request put response hop limit (integer, possible values @1@ to
--     @64@)
--
-- -   @metadata-options.http-endpoint@ - Enable or disable metadata access
--     on http endpoint (@enabled@ | @disabled@)
--
-- -   @monitoring-state@ - Indicates whether detailed monitoring is
--     enabled (@disabled@ | @enabled@).
--
-- -   @network-interface.addresses.private-ip-address@ - The private IPv4
--     address associated with the network interface.
--
-- -   @network-interface.addresses.primary@ - Specifies whether the IPv4
--     address of the network interface is the primary private IPv4
--     address.
--
-- -   @network-interface.addresses.association.public-ip@ - The ID of the
--     association of an Elastic IP address (IPv4) with a network
--     interface.
--
-- -   @network-interface.addresses.association.ip-owner-id@ - The owner ID
--     of the private IPv4 address associated with the network interface.
--
-- -   @network-interface.association.public-ip@ - The address of the
--     Elastic IP address (IPv4) bound to the network interface.
--
-- -   @network-interface.association.ip-owner-id@ - The owner of the
--     Elastic IP address (IPv4) associated with the network interface.
--
-- -   @network-interface.association.allocation-id@ - The allocation ID
--     returned when you allocated the Elastic IP address (IPv4) for your
--     network interface.
--
-- -   @network-interface.association.association-id@ - The association ID
--     returned when the network interface was associated with an IPv4
--     address.
--
-- -   @network-interface.attachment.attachment-id@ - The ID of the
--     interface attachment.
--
-- -   @network-interface.attachment.instance-id@ - The ID of the instance
--     to which the network interface is attached.
--
-- -   @network-interface.attachment.instance-owner-id@ - The owner ID of
--     the instance to which the network interface is attached.
--
-- -   @network-interface.attachment.device-index@ - The device index to
--     which the network interface is attached.
--
-- -   @network-interface.attachment.status@ - The status of the attachment
--     (@attaching@ | @attached@ | @detaching@ | @detached@).
--
-- -   @network-interface.attachment.attach-time@ - The time that the
--     network interface was attached to an instance.
--
-- -   @network-interface.attachment.delete-on-termination@ - Specifies
--     whether the attachment is deleted when an instance is terminated.
--
-- -   @network-interface.availability-zone@ - The Availability Zone for
--     the network interface.
--
-- -   @network-interface.description@ - The description of the network
--     interface.
--
-- -   @network-interface.group-id@ - The ID of a security group associated
--     with the network interface.
--
-- -   @network-interface.group-name@ - The name of a security group
--     associated with the network interface.
--
-- -   @network-interface.ipv6-addresses.ipv6-address@ - The IPv6 address
--     associated with the network interface.
--
-- -   @network-interface.mac-address@ - The MAC address of the network
--     interface.
--
-- -   @network-interface.network-interface-id@ - The ID of the network
--     interface.
--
-- -   @network-interface.owner-id@ - The ID of the owner of the network
--     interface.
--
-- -   @network-interface.private-dns-name@ - The private DNS name of the
--     network interface.
--
-- -   @network-interface.requester-id@ - The requester ID for the network
--     interface.
--
-- -   @network-interface.requester-managed@ - Indicates whether the
--     network interface is being managed by Amazon Web Services.
--
-- -   @network-interface.status@ - The status of the network interface
--     (@available@) | @in-use@).
--
-- -   @network-interface.source-dest-check@ - Whether the network
--     interface performs source\/destination checking. A value of @true@
--     means that checking is enabled, and @false@ means that checking is
--     disabled. The value must be @false@ for the network interface to
--     perform network address translation (NAT) in your VPC.
--
-- -   @network-interface.subnet-id@ - The ID of the subnet for the network
--     interface.
--
-- -   @network-interface.vpc-id@ - The ID of the VPC for the network
--     interface.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @owner-id@ - The Amazon Web Services account ID of the instance
--     owner.
--
-- -   @placement-group-name@ - The name of the placement group for the
--     instance.
--
-- -   @placement-partition-number@ - The partition in which the instance
--     is located.
--
-- -   @platform@ - The platform. To list only Windows instances, use
--     @windows@.
--
-- -   @private-dns-name@ - The private IPv4 DNS name of the instance.
--
-- -   @private-ip-address@ - The private IPv4 address of the instance.
--
-- -   @product-code@ - The product code associated with the AMI used to
--     launch the instance.
--
-- -   @product-code.type@ - The type of product code (@devpay@ |
--     @marketplace@).
--
-- -   @ramdisk-id@ - The RAM disk ID.
--
-- -   @reason@ - The reason for the current state of the instance (for
--     example, shows \"User Initiated [date]\" when you stop or terminate
--     the instance). Similar to the state-reason-code filter.
--
-- -   @requester-id@ - The ID of the entity that launched the instance on
--     your behalf (for example, Amazon Web Services Management Console,
--     Auto Scaling, and so on).
--
-- -   @reservation-id@ - The ID of the instance\'s reservation. A
--     reservation ID is created any time you launch an instance. A
--     reservation ID has a one-to-one relationship with an instance launch
--     request, but can be associated with more than one instance if you
--     launch multiple instances using the same launch request. For
--     example, if you launch one instance, you get one reservation ID. If
--     you launch ten instances using the same launch request, you also get
--     one reservation ID.
--
-- -   @root-device-name@ - The device name of the root device volume (for
--     example, @\/dev\/sda1@).
--
-- -   @root-device-type@ - The type of the root device volume (@ebs@ |
--     @instance-store@).
--
-- -   @source-dest-check@ - Indicates whether the instance performs
--     source\/destination checking. A value of @true@ means that checking
--     is enabled, and @false@ means that checking is disabled. The value
--     must be @false@ for the instance to perform network address
--     translation (NAT) in your VPC.
--
-- -   @spot-instance-request-id@ - The ID of the Spot Instance request.
--
-- -   @state-reason-code@ - The reason code for the state change.
--
-- -   @state-reason-message@ - A message that describes the state change.
--
-- -   @subnet-id@ - The ID of the subnet for the instance.
--
-- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific key,
--     regardless of the tag value.
--
-- -   @tenancy@ - The tenancy of an instance (@dedicated@ | @default@ |
--     @host@).
--
-- -   @virtualization-type@ - The virtualization type of the instance
--     (@paravirtual@ | @hvm@).
--
-- -   @vpc-id@ - The ID of the VPC that the instance is running in.
describeInstances_filters :: Lens.Lens' DescribeInstances (Prelude.Maybe [Filter])
describeInstances_filters = Lens.lens (\DescribeInstances' {filters} -> filters) (\s@DescribeInstances' {} a -> s {filters = a} :: DescribeInstances) Prelude.. Lens.mapping Lens.coerced

-- | The instance IDs.
--
-- Default: Describes all your instances.
describeInstances_instanceIds :: Lens.Lens' DescribeInstances (Prelude.Maybe [Prelude.Text])
describeInstances_instanceIds = Lens.lens (\DescribeInstances' {instanceIds} -> instanceIds) (\s@DescribeInstances' {} a -> s {instanceIds = a} :: DescribeInstances) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000. You cannot specify this
-- parameter and the instance IDs parameter in the same call.
describeInstances_maxResults :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Int)
describeInstances_maxResults = Lens.lens (\DescribeInstances' {maxResults} -> maxResults) (\s@DescribeInstances' {} a -> s {maxResults = a} :: DescribeInstances)

-- | The token to request the next page of results.
describeInstances_nextToken :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Text)
describeInstances_nextToken = Lens.lens (\DescribeInstances' {nextToken} -> nextToken) (\s@DescribeInstances' {} a -> s {nextToken = a} :: DescribeInstances)

instance Core.AWSPager DescribeInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancesResponse_reservations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeInstances_nextToken
              Lens..~ rs
              Lens.^? describeInstancesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstances where
  type
    AWSResponse DescribeInstances =
      DescribeInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstancesResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "reservationSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstances where
  hashWithSalt _salt DescribeInstances' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeInstances where
  rnf DescribeInstances' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf instanceIds `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nextToken

instance Data.ToHeaders DescribeInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstances where
  toQuery DescribeInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "InstanceId"
              Prelude.<$> instanceIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the reservations.
    reservations :: Prelude.Maybe [Reservation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'reservations', 'describeInstancesResponse_reservations' - Information about the reservations.
--
-- 'httpStatus', 'describeInstancesResponse_httpStatus' - The response's http status code.
newDescribeInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstancesResponse
newDescribeInstancesResponse pHttpStatus_ =
  DescribeInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      reservations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeInstancesResponse_nextToken :: Lens.Lens' DescribeInstancesResponse (Prelude.Maybe Prelude.Text)
describeInstancesResponse_nextToken = Lens.lens (\DescribeInstancesResponse' {nextToken} -> nextToken) (\s@DescribeInstancesResponse' {} a -> s {nextToken = a} :: DescribeInstancesResponse)

-- | Information about the reservations.
describeInstancesResponse_reservations :: Lens.Lens' DescribeInstancesResponse (Prelude.Maybe [Reservation])
describeInstancesResponse_reservations = Lens.lens (\DescribeInstancesResponse' {reservations} -> reservations) (\s@DescribeInstancesResponse' {} a -> s {reservations = a} :: DescribeInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstancesResponse_httpStatus :: Lens.Lens' DescribeInstancesResponse Prelude.Int
describeInstancesResponse_httpStatus = Lens.lens (\DescribeInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancesResponse' {} a -> s {httpStatus = a} :: DescribeInstancesResponse)

instance Prelude.NFData DescribeInstancesResponse where
  rnf DescribeInstancesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf reservations `Prelude.seq`
        Prelude.rnf httpStatus
