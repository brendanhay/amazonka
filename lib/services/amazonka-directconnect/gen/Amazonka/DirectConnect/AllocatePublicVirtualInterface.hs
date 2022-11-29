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
-- Module      : Amazonka.DirectConnect.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a public virtual interface to be owned by the specified
-- Amazon Web Services account.
--
-- The owner of a connection calls this function to provision a public
-- virtual interface to be owned by the specified Amazon Web Services
-- account.
--
-- Virtual interfaces created using this function must be confirmed by the
-- owner using ConfirmPublicVirtualInterface. Until this step has been
-- completed, the virtual interface is in the @confirming@ state and is not
-- available to handle traffic.
--
-- When creating an IPv6 public virtual interface, omit the Amazon address
-- and customer address. IPv6 addresses are automatically assigned from the
-- Amazon pool of IPv6 addresses; you cannot specify custom IPv6 addresses.
module Amazonka.DirectConnect.AllocatePublicVirtualInterface
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
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_siteLinkEnabled,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAllocatePublicVirtualInterface' smart constructor.
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface'
  { -- | The ID of the connection on which the public virtual interface is
    -- provisioned.
    connectionId :: Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the public virtual
    -- interface.
    ownerAccount :: Prelude.Text,
    -- | Information about the public virtual interface.
    newPublicVirtualInterfaceAllocation' :: NewPublicVirtualInterfaceAllocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'ownerAccount', 'allocatePublicVirtualInterface_ownerAccount' - The ID of the Amazon Web Services account that owns the public virtual
-- interface.
--
-- 'newPublicVirtualInterfaceAllocation'', 'allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation' - Information about the public virtual interface.
newAllocatePublicVirtualInterface ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'ownerAccount'
  Prelude.Text ->
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
allocatePublicVirtualInterface_connectionId :: Lens.Lens' AllocatePublicVirtualInterface Prelude.Text
allocatePublicVirtualInterface_connectionId = Lens.lens (\AllocatePublicVirtualInterface' {connectionId} -> connectionId) (\s@AllocatePublicVirtualInterface' {} a -> s {connectionId = a} :: AllocatePublicVirtualInterface)

-- | The ID of the Amazon Web Services account that owns the public virtual
-- interface.
allocatePublicVirtualInterface_ownerAccount :: Lens.Lens' AllocatePublicVirtualInterface Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    AllocatePublicVirtualInterface
  where
  hashWithSalt
    _salt
    AllocatePublicVirtualInterface' {..} =
      _salt `Prelude.hashWithSalt` connectionId
        `Prelude.hashWithSalt` ownerAccount
        `Prelude.hashWithSalt` newPublicVirtualInterfaceAllocation'

instance
  Prelude.NFData
    AllocatePublicVirtualInterface
  where
  rnf AllocatePublicVirtualInterface' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf newPublicVirtualInterfaceAllocation'

instance
  Core.ToHeaders
    AllocatePublicVirtualInterface
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AllocatePublicVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AllocatePublicVirtualInterface where
  toJSON AllocatePublicVirtualInterface' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Core..= connectionId),
            Prelude.Just ("ownerAccount" Core..= ownerAccount),
            Prelude.Just
              ( "newPublicVirtualInterfaceAllocation"
                  Core..= newPublicVirtualInterfaceAllocation'
              )
          ]
      )

instance Core.ToPath AllocatePublicVirtualInterface where
  toPath = Prelude.const "/"

instance Core.ToQuery AllocatePublicVirtualInterface where
  toQuery = Prelude.const Prelude.mempty
