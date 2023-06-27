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
-- Module      : Amazonka.EC2.AssociateAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Elastic IP address, or carrier IP address (for instances
-- that are in subnets in Wavelength Zones) with an instance or a network
-- interface. Before you can use an Elastic IP address, you must allocate
-- it to your account.
--
-- If the Elastic IP address is already associated with a different
-- instance, it is disassociated from that instance and associated with the
-- specified instance. If you associate an Elastic IP address with an
-- instance that has an existing Elastic IP address, the existing address
-- is disassociated from the instance, but remains allocated to your
-- account.
--
-- [Subnets in Wavelength Zones] You can associate an IP address from the
-- telecommunication carrier to the instance or network interface.
--
-- You cannot associate an Elastic IP address with an interface in a
-- different network border group.
--
-- This is an idempotent operation. If you perform the operation more than
-- once, Amazon EC2 doesn\'t return an error, and you may be charged for
-- each time the Elastic IP address is remapped to the same instance. For
-- more information, see the /Elastic IP Addresses/ section of
-- <http://aws.amazon.com/ec2/pricing/ Amazon EC2 Pricing>.
module Amazonka.EC2.AssociateAddress
  ( -- * Creating a Request
    AssociateAddress (..),
    newAssociateAddress,

    -- * Request Lenses
    associateAddress_allocationId,
    associateAddress_allowReassociation,
    associateAddress_dryRun,
    associateAddress_instanceId,
    associateAddress_networkInterfaceId,
    associateAddress_privateIpAddress,
    associateAddress_publicIp,

    -- * Destructuring the Response
    AssociateAddressResponse (..),
    newAssociateAddressResponse,

    -- * Response Lenses
    associateAddressResponse_associationId,
    associateAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateAddress' smart constructor.
data AssociateAddress = AssociateAddress'
  { -- | The allocation ID. This is required.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | Reassociation is automatic, but you can specify false to ensure the
    -- operation fails if the Elastic IP address is already associated with
    -- another resource.
    allowReassociation :: Prelude.Maybe Prelude.Bool,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance. The instance must have exactly one attached
    -- network interface. You can specify either the instance ID or the network
    -- interface ID, but not both.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface. If the instance has more than one
    -- network interface, you must specify a network interface ID.
    --
    -- You can specify either the instance ID or the network interface ID, but
    -- not both.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The primary or secondary private IP address to associate with the
    -- Elastic IP address. If no private IP address is specified, the Elastic
    -- IP address is associated with the primary private IP address.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | Deprecated.
    publicIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationId', 'associateAddress_allocationId' - The allocation ID. This is required.
--
-- 'allowReassociation', 'associateAddress_allowReassociation' - Reassociation is automatic, but you can specify false to ensure the
-- operation fails if the Elastic IP address is already associated with
-- another resource.
--
-- 'dryRun', 'associateAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'associateAddress_instanceId' - The ID of the instance. The instance must have exactly one attached
-- network interface. You can specify either the instance ID or the network
-- interface ID, but not both.
--
-- 'networkInterfaceId', 'associateAddress_networkInterfaceId' - The ID of the network interface. If the instance has more than one
-- network interface, you must specify a network interface ID.
--
-- You can specify either the instance ID or the network interface ID, but
-- not both.
--
-- 'privateIpAddress', 'associateAddress_privateIpAddress' - The primary or secondary private IP address to associate with the
-- Elastic IP address. If no private IP address is specified, the Elastic
-- IP address is associated with the primary private IP address.
--
-- 'publicIp', 'associateAddress_publicIp' - Deprecated.
newAssociateAddress ::
  AssociateAddress
newAssociateAddress =
  AssociateAddress'
    { allocationId = Prelude.Nothing,
      allowReassociation = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      publicIp = Prelude.Nothing
    }

-- | The allocation ID. This is required.
associateAddress_allocationId :: Lens.Lens' AssociateAddress (Prelude.Maybe Prelude.Text)
associateAddress_allocationId = Lens.lens (\AssociateAddress' {allocationId} -> allocationId) (\s@AssociateAddress' {} a -> s {allocationId = a} :: AssociateAddress)

-- | Reassociation is automatic, but you can specify false to ensure the
-- operation fails if the Elastic IP address is already associated with
-- another resource.
associateAddress_allowReassociation :: Lens.Lens' AssociateAddress (Prelude.Maybe Prelude.Bool)
associateAddress_allowReassociation = Lens.lens (\AssociateAddress' {allowReassociation} -> allowReassociation) (\s@AssociateAddress' {} a -> s {allowReassociation = a} :: AssociateAddress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateAddress_dryRun :: Lens.Lens' AssociateAddress (Prelude.Maybe Prelude.Bool)
associateAddress_dryRun = Lens.lens (\AssociateAddress' {dryRun} -> dryRun) (\s@AssociateAddress' {} a -> s {dryRun = a} :: AssociateAddress)

-- | The ID of the instance. The instance must have exactly one attached
-- network interface. You can specify either the instance ID or the network
-- interface ID, but not both.
associateAddress_instanceId :: Lens.Lens' AssociateAddress (Prelude.Maybe Prelude.Text)
associateAddress_instanceId = Lens.lens (\AssociateAddress' {instanceId} -> instanceId) (\s@AssociateAddress' {} a -> s {instanceId = a} :: AssociateAddress)

-- | The ID of the network interface. If the instance has more than one
-- network interface, you must specify a network interface ID.
--
-- You can specify either the instance ID or the network interface ID, but
-- not both.
associateAddress_networkInterfaceId :: Lens.Lens' AssociateAddress (Prelude.Maybe Prelude.Text)
associateAddress_networkInterfaceId = Lens.lens (\AssociateAddress' {networkInterfaceId} -> networkInterfaceId) (\s@AssociateAddress' {} a -> s {networkInterfaceId = a} :: AssociateAddress)

-- | The primary or secondary private IP address to associate with the
-- Elastic IP address. If no private IP address is specified, the Elastic
-- IP address is associated with the primary private IP address.
associateAddress_privateIpAddress :: Lens.Lens' AssociateAddress (Prelude.Maybe Prelude.Text)
associateAddress_privateIpAddress = Lens.lens (\AssociateAddress' {privateIpAddress} -> privateIpAddress) (\s@AssociateAddress' {} a -> s {privateIpAddress = a} :: AssociateAddress)

-- | Deprecated.
associateAddress_publicIp :: Lens.Lens' AssociateAddress (Prelude.Maybe Prelude.Text)
associateAddress_publicIp = Lens.lens (\AssociateAddress' {publicIp} -> publicIp) (\s@AssociateAddress' {} a -> s {publicIp = a} :: AssociateAddress)

instance Core.AWSRequest AssociateAddress where
  type
    AWSResponse AssociateAddress =
      AssociateAddressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateAddressResponse'
            Prelude.<$> (x Data..@? "associationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateAddress where
  hashWithSalt _salt AssociateAddress' {..} =
    _salt
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` allowReassociation
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` publicIp

instance Prelude.NFData AssociateAddress where
  rnf AssociateAddress' {..} =
    Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf allowReassociation
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf publicIp

instance Data.ToHeaders AssociateAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateAddress where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateAddress where
  toQuery AssociateAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssociateAddress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AllocationId" Data.=: allocationId,
        "AllowReassociation" Data.=: allowReassociation,
        "DryRun" Data.=: dryRun,
        "InstanceId" Data.=: instanceId,
        "NetworkInterfaceId" Data.=: networkInterfaceId,
        "PrivateIpAddress" Data.=: privateIpAddress,
        "PublicIp" Data.=: publicIp
      ]

-- | /See:/ 'newAssociateAddressResponse' smart constructor.
data AssociateAddressResponse = AssociateAddressResponse'
  { -- | The ID that represents the association of the Elastic IP address with an
    -- instance.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'associateAddressResponse_associationId' - The ID that represents the association of the Elastic IP address with an
-- instance.
--
-- 'httpStatus', 'associateAddressResponse_httpStatus' - The response's http status code.
newAssociateAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateAddressResponse
newAssociateAddressResponse pHttpStatus_ =
  AssociateAddressResponse'
    { associationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID that represents the association of the Elastic IP address with an
-- instance.
associateAddressResponse_associationId :: Lens.Lens' AssociateAddressResponse (Prelude.Maybe Prelude.Text)
associateAddressResponse_associationId = Lens.lens (\AssociateAddressResponse' {associationId} -> associationId) (\s@AssociateAddressResponse' {} a -> s {associationId = a} :: AssociateAddressResponse)

-- | The response's http status code.
associateAddressResponse_httpStatus :: Lens.Lens' AssociateAddressResponse Prelude.Int
associateAddressResponse_httpStatus = Lens.lens (\AssociateAddressResponse' {httpStatus} -> httpStatus) (\s@AssociateAddressResponse' {} a -> s {httpStatus = a} :: AssociateAddressResponse)

instance Prelude.NFData AssociateAddressResponse where
  rnf AssociateAddressResponse' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf httpStatus
