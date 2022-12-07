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
-- Module      : Amazonka.EC2.AllocateHosts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a Dedicated Host to your account. At a minimum, specify the
-- supported instance type or instance family, the Availability Zone in
-- which to allocate the host, and the number of hosts to allocate.
module Amazonka.EC2.AllocateHosts
  ( -- * Creating a Request
    AllocateHosts (..),
    newAllocateHosts,

    -- * Request Lenses
    allocateHosts_autoPlacement,
    allocateHosts_clientToken,
    allocateHosts_outpostArn,
    allocateHosts_hostRecovery,
    allocateHosts_instanceType,
    allocateHosts_instanceFamily,
    allocateHosts_tagSpecifications,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAllocateHosts' smart constructor.
data AllocateHosts = AllocateHosts'
  { -- | Indicates whether the host accepts any untargeted instance launches that
    -- match its instance type configuration, or if it only accepts Host
    -- tenancy instance launches that specify its unique host ID. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding auto-placement and affinity>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: @on@
    autoPlacement :: Prelude.Maybe AutoPlacement,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Outpost on
    -- which to allocate the Dedicated Host.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to enable or disable host recovery for the Dedicated
    -- Host. Host recovery is disabled by default. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: @off@
    hostRecovery :: Prelude.Maybe HostRecovery,
    -- | Specifies the instance type to be supported by the Dedicated Hosts. If
    -- you specify an instance type, the Dedicated Hosts support instances of
    -- the specified instance type only.
    --
    -- If you want the Dedicated Hosts to support multiple instance types in a
    -- specific instance family, omit this parameter and specify
    -- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
    -- __InstanceFamily__ in the same request.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the instance family to be supported by the Dedicated Hosts. If
    -- you specify an instance family, the Dedicated Hosts support multiple
    -- instance types within that instance family.
    --
    -- If you want the Dedicated Hosts to support a specific instance type
    -- only, omit this parameter and specify __InstanceType__ instead. You
    -- cannot specify __InstanceFamily__ and __InstanceType__ in the same
    -- request.
    instanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the Dedicated Host during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The Availability Zone in which to allocate the Dedicated Host.
    availabilityZone :: Prelude.Text,
    -- | The number of Dedicated Hosts to allocate to your account with these
    -- parameters.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllocateHosts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'clientToken', 'allocateHosts_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'outpostArn', 'allocateHosts_outpostArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Outpost on
-- which to allocate the Dedicated Host.
--
-- 'hostRecovery', 'allocateHosts_hostRecovery' - Indicates whether to enable or disable host recovery for the Dedicated
-- Host. Host recovery is disabled by default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @off@
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
-- 'availabilityZone', 'allocateHosts_availabilityZone' - The Availability Zone in which to allocate the Dedicated Host.
--
-- 'quantity', 'allocateHosts_quantity' - The number of Dedicated Hosts to allocate to your account with these
-- parameters.
newAllocateHosts ::
  -- | 'availabilityZone'
  Prelude.Text ->
  -- | 'quantity'
  Prelude.Int ->
  AllocateHosts
newAllocateHosts pAvailabilityZone_ pQuantity_ =
  AllocateHosts'
    { autoPlacement = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      hostRecovery = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instanceFamily = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      availabilityZone = pAvailabilityZone_,
      quantity = pQuantity_
    }

-- | Indicates whether the host accepts any untargeted instance launches that
-- match its instance type configuration, or if it only accepts Host
-- tenancy instance launches that specify its unique host ID. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding auto-placement and affinity>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @on@
allocateHosts_autoPlacement :: Lens.Lens' AllocateHosts (Prelude.Maybe AutoPlacement)
allocateHosts_autoPlacement = Lens.lens (\AllocateHosts' {autoPlacement} -> autoPlacement) (\s@AllocateHosts' {} a -> s {autoPlacement = a} :: AllocateHosts)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
allocateHosts_clientToken :: Lens.Lens' AllocateHosts (Prelude.Maybe Prelude.Text)
allocateHosts_clientToken = Lens.lens (\AllocateHosts' {clientToken} -> clientToken) (\s@AllocateHosts' {} a -> s {clientToken = a} :: AllocateHosts)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Outpost on
-- which to allocate the Dedicated Host.
allocateHosts_outpostArn :: Lens.Lens' AllocateHosts (Prelude.Maybe Prelude.Text)
allocateHosts_outpostArn = Lens.lens (\AllocateHosts' {outpostArn} -> outpostArn) (\s@AllocateHosts' {} a -> s {outpostArn = a} :: AllocateHosts)

-- | Indicates whether to enable or disable host recovery for the Dedicated
-- Host. Host recovery is disabled by default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @off@
allocateHosts_hostRecovery :: Lens.Lens' AllocateHosts (Prelude.Maybe HostRecovery)
allocateHosts_hostRecovery = Lens.lens (\AllocateHosts' {hostRecovery} -> hostRecovery) (\s@AllocateHosts' {} a -> s {hostRecovery = a} :: AllocateHosts)

-- | Specifies the instance type to be supported by the Dedicated Hosts. If
-- you specify an instance type, the Dedicated Hosts support instances of
-- the specified instance type only.
--
-- If you want the Dedicated Hosts to support multiple instance types in a
-- specific instance family, omit this parameter and specify
-- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
-- __InstanceFamily__ in the same request.
allocateHosts_instanceType :: Lens.Lens' AllocateHosts (Prelude.Maybe Prelude.Text)
allocateHosts_instanceType = Lens.lens (\AllocateHosts' {instanceType} -> instanceType) (\s@AllocateHosts' {} a -> s {instanceType = a} :: AllocateHosts)

-- | Specifies the instance family to be supported by the Dedicated Hosts. If
-- you specify an instance family, the Dedicated Hosts support multiple
-- instance types within that instance family.
--
-- If you want the Dedicated Hosts to support a specific instance type
-- only, omit this parameter and specify __InstanceType__ instead. You
-- cannot specify __InstanceFamily__ and __InstanceType__ in the same
-- request.
allocateHosts_instanceFamily :: Lens.Lens' AllocateHosts (Prelude.Maybe Prelude.Text)
allocateHosts_instanceFamily = Lens.lens (\AllocateHosts' {instanceFamily} -> instanceFamily) (\s@AllocateHosts' {} a -> s {instanceFamily = a} :: AllocateHosts)

-- | The tags to apply to the Dedicated Host during creation.
allocateHosts_tagSpecifications :: Lens.Lens' AllocateHosts (Prelude.Maybe [TagSpecification])
allocateHosts_tagSpecifications = Lens.lens (\AllocateHosts' {tagSpecifications} -> tagSpecifications) (\s@AllocateHosts' {} a -> s {tagSpecifications = a} :: AllocateHosts) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zone in which to allocate the Dedicated Host.
allocateHosts_availabilityZone :: Lens.Lens' AllocateHosts Prelude.Text
allocateHosts_availabilityZone = Lens.lens (\AllocateHosts' {availabilityZone} -> availabilityZone) (\s@AllocateHosts' {} a -> s {availabilityZone = a} :: AllocateHosts)

-- | The number of Dedicated Hosts to allocate to your account with these
-- parameters.
allocateHosts_quantity :: Lens.Lens' AllocateHosts Prelude.Int
allocateHosts_quantity = Lens.lens (\AllocateHosts' {quantity} -> quantity) (\s@AllocateHosts' {} a -> s {quantity = a} :: AllocateHosts)

instance Core.AWSRequest AllocateHosts where
  type
    AWSResponse AllocateHosts =
      AllocateHostsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AllocateHostsResponse'
            Prelude.<$> ( x Data..@? "hostIdSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AllocateHosts where
  hashWithSalt _salt AllocateHosts' {..} =
    _salt `Prelude.hashWithSalt` autoPlacement
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` hostRecovery
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceFamily
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData AllocateHosts where
  rnf AllocateHosts' {..} =
    Prelude.rnf autoPlacement
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf hostRecovery
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceFamily
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf quantity

instance Data.ToHeaders AllocateHosts where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AllocateHosts where
  toPath = Prelude.const "/"

instance Data.ToQuery AllocateHosts where
  toQuery AllocateHosts' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AllocateHosts" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AutoPlacement" Data.=: autoPlacement,
        "ClientToken" Data.=: clientToken,
        "OutpostArn" Data.=: outpostArn,
        "HostRecovery" Data.=: hostRecovery,
        "InstanceType" Data.=: instanceType,
        "InstanceFamily" Data.=: instanceFamily,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "AvailabilityZone" Data.=: availabilityZone,
        "Quantity" Data.=: quantity
      ]

-- | Contains the output of AllocateHosts.
--
-- /See:/ 'newAllocateHostsResponse' smart constructor.
data AllocateHostsResponse = AllocateHostsResponse'
  { -- | The ID of the allocated Dedicated Host. This is used to launch an
    -- instance onto a specific host.
    hostIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AllocateHostsResponse
newAllocateHostsResponse pHttpStatus_ =
  AllocateHostsResponse'
    { hostIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the allocated Dedicated Host. This is used to launch an
-- instance onto a specific host.
allocateHostsResponse_hostIds :: Lens.Lens' AllocateHostsResponse (Prelude.Maybe [Prelude.Text])
allocateHostsResponse_hostIds = Lens.lens (\AllocateHostsResponse' {hostIds} -> hostIds) (\s@AllocateHostsResponse' {} a -> s {hostIds = a} :: AllocateHostsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
allocateHostsResponse_httpStatus :: Lens.Lens' AllocateHostsResponse Prelude.Int
allocateHostsResponse_httpStatus = Lens.lens (\AllocateHostsResponse' {httpStatus} -> httpStatus) (\s@AllocateHostsResponse' {} a -> s {httpStatus = a} :: AllocateHostsResponse)

instance Prelude.NFData AllocateHostsResponse where
  rnf AllocateHostsResponse' {..} =
    Prelude.rnf hostIds
      `Prelude.seq` Prelude.rnf httpStatus
