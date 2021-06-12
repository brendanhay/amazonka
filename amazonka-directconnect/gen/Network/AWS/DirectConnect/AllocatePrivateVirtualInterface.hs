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
-- Module      : Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a private virtual interface to be owned by the specified AWS
-- account.
--
-- Virtual interfaces created using this action must be confirmed by the
-- owner using ConfirmPrivateVirtualInterface. Until then, the virtual
-- interface is in the @Confirming@ state and is not available to handle
-- traffic.
module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
  ( -- * Creating a Request
    AllocatePrivateVirtualInterface (..),
    newAllocatePrivateVirtualInterface,

    -- * Request Lenses
    allocatePrivateVirtualInterface_connectionId,
    allocatePrivateVirtualInterface_ownerAccount,
    allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation,

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

-- | /See:/ 'newAllocatePrivateVirtualInterface' smart constructor.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface'
  { -- | The ID of the connection on which the private virtual interface is
    -- provisioned.
    connectionId :: Core.Text,
    -- | The ID of the AWS account that owns the virtual private interface.
    ownerAccount :: Core.Text,
    -- | Information about the private virtual interface.
    newPrivateVirtualInterfaceAllocation' :: NewPrivateVirtualInterfaceAllocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AllocatePrivateVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'allocatePrivateVirtualInterface_connectionId' - The ID of the connection on which the private virtual interface is
-- provisioned.
--
-- 'ownerAccount', 'allocatePrivateVirtualInterface_ownerAccount' - The ID of the AWS account that owns the virtual private interface.
--
-- 'newPrivateVirtualInterfaceAllocation'', 'allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation' - Information about the private virtual interface.
newAllocatePrivateVirtualInterface ::
  -- | 'connectionId'
  Core.Text ->
  -- | 'ownerAccount'
  Core.Text ->
  -- | 'newPrivateVirtualInterfaceAllocation''
  NewPrivateVirtualInterfaceAllocation ->
  AllocatePrivateVirtualInterface
newAllocatePrivateVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewPrivateVirtualInterfaceAllocation_ =
    AllocatePrivateVirtualInterface'
      { connectionId =
          pConnectionId_,
        ownerAccount = pOwnerAccount_,
        newPrivateVirtualInterfaceAllocation' =
          pNewPrivateVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the private virtual interface is
-- provisioned.
allocatePrivateVirtualInterface_connectionId :: Lens.Lens' AllocatePrivateVirtualInterface Core.Text
allocatePrivateVirtualInterface_connectionId = Lens.lens (\AllocatePrivateVirtualInterface' {connectionId} -> connectionId) (\s@AllocatePrivateVirtualInterface' {} a -> s {connectionId = a} :: AllocatePrivateVirtualInterface)

-- | The ID of the AWS account that owns the virtual private interface.
allocatePrivateVirtualInterface_ownerAccount :: Lens.Lens' AllocatePrivateVirtualInterface Core.Text
allocatePrivateVirtualInterface_ownerAccount = Lens.lens (\AllocatePrivateVirtualInterface' {ownerAccount} -> ownerAccount) (\s@AllocatePrivateVirtualInterface' {} a -> s {ownerAccount = a} :: AllocatePrivateVirtualInterface)

-- | Information about the private virtual interface.
allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation :: Lens.Lens' AllocatePrivateVirtualInterface NewPrivateVirtualInterfaceAllocation
allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation = Lens.lens (\AllocatePrivateVirtualInterface' {newPrivateVirtualInterfaceAllocation'} -> newPrivateVirtualInterfaceAllocation') (\s@AllocatePrivateVirtualInterface' {} a -> s {newPrivateVirtualInterfaceAllocation' = a} :: AllocatePrivateVirtualInterface)

instance
  Core.AWSRequest
    AllocatePrivateVirtualInterface
  where
  type
    AWSResponse AllocatePrivateVirtualInterface =
      VirtualInterface
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Core.Hashable
    AllocatePrivateVirtualInterface

instance Core.NFData AllocatePrivateVirtualInterface

instance
  Core.ToHeaders
    AllocatePrivateVirtualInterface
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AllocatePrivateVirtualInterface" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AllocatePrivateVirtualInterface where
  toJSON AllocatePrivateVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just ("ownerAccount" Core..= ownerAccount),
            Core.Just
              ( "newPrivateVirtualInterfaceAllocation"
                  Core..= newPrivateVirtualInterfaceAllocation'
              )
          ]
      )

instance Core.ToPath AllocatePrivateVirtualInterface where
  toPath = Core.const "/"

instance Core.ToQuery AllocatePrivateVirtualInterface where
  toQuery = Core.const Core.mempty
