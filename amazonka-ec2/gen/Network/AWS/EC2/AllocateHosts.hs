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
-- Module      : Network.AWS.EC2.AllocateHosts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a Dedicated Host to your account. At a minimum, specify the
-- supported instance type or instance family, the Availability Zone in
-- which to allocate the host, and the number of hosts to allocate.
module Network.AWS.EC2.AllocateHosts
  ( -- * Creating a Request
    AllocateHosts (..),
    newAllocateHosts,

    -- * Request Lenses
    allocateHosts_instanceFamily,
    allocateHosts_tagSpecifications,
    allocateHosts_instanceType,
    allocateHosts_autoPlacement,
    allocateHosts_hostRecovery,
    allocateHosts_clientToken,
    allocateHosts_availabilityZone,
    allocateHosts_quantity,

    -- * Destructuring the Response
    AllocateHostsResponse (..),
    newAllocateHostsResponse,

    -- * Response Lenses
    allocateHostsResponse_hostIds,
    allocateHostsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAllocateHosts' smart constructor.
data AllocateHosts = AllocateHosts'
  { -- | Specifies the instance family to be supported by the Dedicated Hosts. If
    -- you specify an instance family, the Dedicated Hosts support multiple
    -- instance types within that instance family.
    --
    -- If you want the Dedicated Hosts to support a specific instance type
    -- only, omit this parameter and specify __InstanceType__ instead. You
    -- cannot specify __InstanceFamily__ and __InstanceType__ in the same
    -- request.
    instanceFamily :: Core.Maybe Core.Text,
    -- | The tags to apply to the Dedicated Host during creation.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Specifies the instance type to be supported by the Dedicated Hosts. If
    -- you specify an instance type, the Dedicated Hosts support instances of
    -- the specified instance type only.
    --
    -- If you want the Dedicated Hosts to support multiple instance types in a
    -- specific instance family, omit this parameter and specify
    -- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
    -- __InstanceFamily__ in the same request.
    instanceType :: Core.Maybe Core.Text,
    -- | Indicates whether the host accepts any untargeted instance launches that
    -- match its instance type configuration, or if it only accepts Host
    -- tenancy instance launches that specify its unique host ID. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding auto-placement and affinity>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: @on@
    autoPlacement :: Core.Maybe AutoPlacement,
    -- | Indicates whether to enable or disable host recovery for the Dedicated
    -- Host. Host recovery is disabled by default. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: @off@
    hostRecovery :: Core.Maybe HostRecovery,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The Availability Zone in which to allocate the Dedicated Host.
    availabilityZone :: Core.Text,
    -- | The number of Dedicated Hosts to allocate to your account with these
    -- parameters.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AllocateHosts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'allocateHosts_instanceFamily' - Specifies the instance family to be supported by the Dedicated Hosts. If
-- you specify an instance family, the Dedicated Hosts support multiple
-- instance types within that instance family.
--
-- If you want the Dedicated Hosts to support a specific instance type
-- only, omit this parameter and specify __InstanceType__ instead. You
-- cannot specify __InstanceFamily__ and __InstanceType__ in the same
-- request.
--
-- 'tagSpecifications', 'allocateHosts_tagSpecifications' - The tags to apply to the Dedicated Host during creation.
--
-- 'instanceType', 'allocateHosts_instanceType' - Specifies the instance type to be supported by the Dedicated Hosts. If
-- you specify an instance type, the Dedicated Hosts support instances of
-- the specified instance type only.
--
-- If you want the Dedicated Hosts to support multiple instance types in a
-- specific instance family, omit this parameter and specify
-- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
-- __InstanceFamily__ in the same request.
--
-- 'autoPlacement', 'allocateHosts_autoPlacement' - Indicates whether the host accepts any untargeted instance launches that
-- match its instance type configuration, or if it only accepts Host
-- tenancy instance launches that specify its unique host ID. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding auto-placement and affinity>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @on@
--
-- 'hostRecovery', 'allocateHosts_hostRecovery' - Indicates whether to enable or disable host recovery for the Dedicated
-- Host. Host recovery is disabled by default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @off@
--
-- 'clientToken', 'allocateHosts_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'availabilityZone', 'allocateHosts_availabilityZone' - The Availability Zone in which to allocate the Dedicated Host.
--
-- 'quantity', 'allocateHosts_quantity' - The number of Dedicated Hosts to allocate to your account with these
-- parameters.
newAllocateHosts ::
  -- | 'availabilityZone'
  Core.Text ->
  -- | 'quantity'
  Core.Int ->
  AllocateHosts
newAllocateHosts pAvailabilityZone_ pQuantity_ =
  AllocateHosts'
    { instanceFamily = Core.Nothing,
      tagSpecifications = Core.Nothing,
      instanceType = Core.Nothing,
      autoPlacement = Core.Nothing,
      hostRecovery = Core.Nothing,
      clientToken = Core.Nothing,
      availabilityZone = pAvailabilityZone_,
      quantity = pQuantity_
    }

-- | Specifies the instance family to be supported by the Dedicated Hosts. If
-- you specify an instance family, the Dedicated Hosts support multiple
-- instance types within that instance family.
--
-- If you want the Dedicated Hosts to support a specific instance type
-- only, omit this parameter and specify __InstanceType__ instead. You
-- cannot specify __InstanceFamily__ and __InstanceType__ in the same
-- request.
allocateHosts_instanceFamily :: Lens.Lens' AllocateHosts (Core.Maybe Core.Text)
allocateHosts_instanceFamily = Lens.lens (\AllocateHosts' {instanceFamily} -> instanceFamily) (\s@AllocateHosts' {} a -> s {instanceFamily = a} :: AllocateHosts)

-- | The tags to apply to the Dedicated Host during creation.
allocateHosts_tagSpecifications :: Lens.Lens' AllocateHosts (Core.Maybe [TagSpecification])
allocateHosts_tagSpecifications = Lens.lens (\AllocateHosts' {tagSpecifications} -> tagSpecifications) (\s@AllocateHosts' {} a -> s {tagSpecifications = a} :: AllocateHosts) Core.. Lens.mapping Lens._Coerce

-- | Specifies the instance type to be supported by the Dedicated Hosts. If
-- you specify an instance type, the Dedicated Hosts support instances of
-- the specified instance type only.
--
-- If you want the Dedicated Hosts to support multiple instance types in a
-- specific instance family, omit this parameter and specify
-- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
-- __InstanceFamily__ in the same request.
allocateHosts_instanceType :: Lens.Lens' AllocateHosts (Core.Maybe Core.Text)
allocateHosts_instanceType = Lens.lens (\AllocateHosts' {instanceType} -> instanceType) (\s@AllocateHosts' {} a -> s {instanceType = a} :: AllocateHosts)

-- | Indicates whether the host accepts any untargeted instance launches that
-- match its instance type configuration, or if it only accepts Host
-- tenancy instance launches that specify its unique host ID. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding auto-placement and affinity>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @on@
allocateHosts_autoPlacement :: Lens.Lens' AllocateHosts (Core.Maybe AutoPlacement)
allocateHosts_autoPlacement = Lens.lens (\AllocateHosts' {autoPlacement} -> autoPlacement) (\s@AllocateHosts' {} a -> s {autoPlacement = a} :: AllocateHosts)

-- | Indicates whether to enable or disable host recovery for the Dedicated
-- Host. Host recovery is disabled by default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @off@
allocateHosts_hostRecovery :: Lens.Lens' AllocateHosts (Core.Maybe HostRecovery)
allocateHosts_hostRecovery = Lens.lens (\AllocateHosts' {hostRecovery} -> hostRecovery) (\s@AllocateHosts' {} a -> s {hostRecovery = a} :: AllocateHosts)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
allocateHosts_clientToken :: Lens.Lens' AllocateHosts (Core.Maybe Core.Text)
allocateHosts_clientToken = Lens.lens (\AllocateHosts' {clientToken} -> clientToken) (\s@AllocateHosts' {} a -> s {clientToken = a} :: AllocateHosts)

-- | The Availability Zone in which to allocate the Dedicated Host.
allocateHosts_availabilityZone :: Lens.Lens' AllocateHosts Core.Text
allocateHosts_availabilityZone = Lens.lens (\AllocateHosts' {availabilityZone} -> availabilityZone) (\s@AllocateHosts' {} a -> s {availabilityZone = a} :: AllocateHosts)

-- | The number of Dedicated Hosts to allocate to your account with these
-- parameters.
allocateHosts_quantity :: Lens.Lens' AllocateHosts Core.Int
allocateHosts_quantity = Lens.lens (\AllocateHosts' {quantity} -> quantity) (\s@AllocateHosts' {} a -> s {quantity = a} :: AllocateHosts)

instance Core.AWSRequest AllocateHosts where
  type
    AWSResponse AllocateHosts =
      AllocateHostsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AllocateHostsResponse'
            Core.<$> ( x Core..@? "hostIdSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AllocateHosts

instance Core.NFData AllocateHosts

instance Core.ToHeaders AllocateHosts where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AllocateHosts where
  toPath = Core.const "/"

instance Core.ToQuery AllocateHosts where
  toQuery AllocateHosts' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AllocateHosts" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "InstanceFamily" Core.=: instanceFamily,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "InstanceType" Core.=: instanceType,
        "AutoPlacement" Core.=: autoPlacement,
        "HostRecovery" Core.=: hostRecovery,
        "ClientToken" Core.=: clientToken,
        "AvailabilityZone" Core.=: availabilityZone,
        "Quantity" Core.=: quantity
      ]

-- | Contains the output of AllocateHosts.
--
-- /See:/ 'newAllocateHostsResponse' smart constructor.
data AllocateHostsResponse = AllocateHostsResponse'
  { -- | The ID of the allocated Dedicated Host. This is used to launch an
    -- instance onto a specific host.
    hostIds :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AllocateHostsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostIds', 'allocateHostsResponse_hostIds' - The ID of the allocated Dedicated Host. This is used to launch an
-- instance onto a specific host.
--
-- 'httpStatus', 'allocateHostsResponse_httpStatus' - The response's http status code.
newAllocateHostsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AllocateHostsResponse
newAllocateHostsResponse pHttpStatus_ =
  AllocateHostsResponse'
    { hostIds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the allocated Dedicated Host. This is used to launch an
-- instance onto a specific host.
allocateHostsResponse_hostIds :: Lens.Lens' AllocateHostsResponse (Core.Maybe [Core.Text])
allocateHostsResponse_hostIds = Lens.lens (\AllocateHostsResponse' {hostIds} -> hostIds) (\s@AllocateHostsResponse' {} a -> s {hostIds = a} :: AllocateHostsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
allocateHostsResponse_httpStatus :: Lens.Lens' AllocateHostsResponse Core.Int
allocateHostsResponse_httpStatus = Lens.lens (\AllocateHostsResponse' {httpStatus} -> httpStatus) (\s@AllocateHostsResponse' {} a -> s {httpStatus = a} :: AllocateHostsResponse)

instance Core.NFData AllocateHostsResponse
