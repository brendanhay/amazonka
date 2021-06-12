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
-- Module      : Network.AWS.DirectConnect.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a public virtual interface to be owned by the specified AWS
-- account.
--
-- The owner of a connection calls this function to provision a public
-- virtual interface to be owned by the specified AWS account.
--
-- Virtual interfaces created using this function must be confirmed by the
-- owner using ConfirmPublicVirtualInterface. Until this step has been
-- completed, the virtual interface is in the @confirming@ state and is not
-- available to handle traffic.
--
-- When creating an IPv6 public virtual interface, omit the Amazon address
-- and customer address. IPv6 addresses are automatically assigned from the
-- Amazon pool of IPv6 addresses; you cannot specify custom IPv6 addresses.
module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
  ( -- * Creating a Request
    AllocatePublicVirtualInterface (..),
    newAllocatePublicVirtualInterface,

    -- * Request Lenses
    allocatePublicVirtualInterface_connectionId,
    allocatePublicVirtualInterface_ownerAccount,
    allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation,

    -- * Destructuring the Response
    VirtualInterface (..),
    newVirtualInterface,

    -- * Response Lenses
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualInterfaceType,
    virtualInterface_mtu,
    virtualInterface_tags,
    virtualInterface_virtualInterfaceId,
    virtualInterface_amazonSideAsn,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceName,
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_location,
    virtualInterface_vlan,
    virtualInterface_customerAddress,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAllocatePublicVirtualInterface' smart constructor.
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface'
  { -- | The ID of the connection on which the public virtual interface is
    -- provisioned.
    connectionId :: Core.Text,
    -- | The ID of the AWS account that owns the public virtual interface.
    ownerAccount :: Core.Text,
    -- | Information about the public virtual interface.
    newPublicVirtualInterfaceAllocation' :: NewPublicVirtualInterfaceAllocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AllocatePublicVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'allocatePublicVirtualInterface_connectionId' - The ID of the connection on which the public virtual interface is
-- provisioned.
--
-- 'ownerAccount', 'allocatePublicVirtualInterface_ownerAccount' - The ID of the AWS account that owns the public virtual interface.
--
-- 'newPublicVirtualInterfaceAllocation'', 'allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation' - Information about the public virtual interface.
newAllocatePublicVirtualInterface ::
  -- | 'connectionId'
  Core.Text ->
  -- | 'ownerAccount'
  Core.Text ->
  -- | 'newPublicVirtualInterfaceAllocation''
  NewPublicVirtualInterfaceAllocation ->
  AllocatePublicVirtualInterface
newAllocatePublicVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewPublicVirtualInterfaceAllocation_ =
    AllocatePublicVirtualInterface'
      { connectionId =
          pConnectionId_,
        ownerAccount = pOwnerAccount_,
        newPublicVirtualInterfaceAllocation' =
          pNewPublicVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the public virtual interface is
-- provisioned.
allocatePublicVirtualInterface_connectionId :: Lens.Lens' AllocatePublicVirtualInterface Core.Text
allocatePublicVirtualInterface_connectionId = Lens.lens (\AllocatePublicVirtualInterface' {connectionId} -> connectionId) (\s@AllocatePublicVirtualInterface' {} a -> s {connectionId = a} :: AllocatePublicVirtualInterface)

-- | The ID of the AWS account that owns the public virtual interface.
allocatePublicVirtualInterface_ownerAccount :: Lens.Lens' AllocatePublicVirtualInterface Core.Text
allocatePublicVirtualInterface_ownerAccount = Lens.lens (\AllocatePublicVirtualInterface' {ownerAccount} -> ownerAccount) (\s@AllocatePublicVirtualInterface' {} a -> s {ownerAccount = a} :: AllocatePublicVirtualInterface)

-- | Information about the public virtual interface.
allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation :: Lens.Lens' AllocatePublicVirtualInterface NewPublicVirtualInterfaceAllocation
allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation = Lens.lens (\AllocatePublicVirtualInterface' {newPublicVirtualInterfaceAllocation'} -> newPublicVirtualInterfaceAllocation') (\s@AllocatePublicVirtualInterface' {} a -> s {newPublicVirtualInterfaceAllocation' = a} :: AllocatePublicVirtualInterface)

instance
  Core.AWSRequest
    AllocatePublicVirtualInterface
  where
  type
    AWSResponse AllocatePublicVirtualInterface =
      VirtualInterface
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable AllocatePublicVirtualInterface

instance Core.NFData AllocatePublicVirtualInterface

instance
  Core.ToHeaders
    AllocatePublicVirtualInterface
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AllocatePublicVirtualInterface" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AllocatePublicVirtualInterface where
  toJSON AllocatePublicVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just ("ownerAccount" Core..= ownerAccount),
            Core.Just
              ( "newPublicVirtualInterfaceAllocation"
                  Core..= newPublicVirtualInterfaceAllocation'
              )
          ]
      )

instance Core.ToPath AllocatePublicVirtualInterface where
  toPath = Core.const "/"

instance Core.ToQuery AllocatePublicVirtualInterface where
  toQuery = Core.const Core.mempty
