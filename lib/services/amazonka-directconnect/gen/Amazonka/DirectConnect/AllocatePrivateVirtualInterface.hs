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
-- Module      : Amazonka.DirectConnect.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a private virtual interface to be owned by the specified
-- Amazon Web Services account.
--
-- Virtual interfaces created using this action must be confirmed by the
-- owner using ConfirmPrivateVirtualInterface. Until then, the virtual
-- interface is in the @Confirming@ state and is not available to handle
-- traffic.
module Amazonka.DirectConnect.AllocatePrivateVirtualInterface
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
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAllocatePrivateVirtualInterface' smart constructor.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface'
  { -- | The ID of the connection on which the private virtual interface is
    -- provisioned.
    connectionId :: Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the virtual private
    -- interface.
    ownerAccount :: Prelude.Text,
    -- | Information about the private virtual interface.
    newPrivateVirtualInterfaceAllocation' :: NewPrivateVirtualInterfaceAllocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'ownerAccount', 'allocatePrivateVirtualInterface_ownerAccount' - The ID of the Amazon Web Services account that owns the virtual private
-- interface.
--
-- 'newPrivateVirtualInterfaceAllocation'', 'allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation' - Information about the private virtual interface.
newAllocatePrivateVirtualInterface ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'ownerAccount'
  Prelude.Text ->
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
allocatePrivateVirtualInterface_connectionId :: Lens.Lens' AllocatePrivateVirtualInterface Prelude.Text
allocatePrivateVirtualInterface_connectionId = Lens.lens (\AllocatePrivateVirtualInterface' {connectionId} -> connectionId) (\s@AllocatePrivateVirtualInterface' {} a -> s {connectionId = a} :: AllocatePrivateVirtualInterface)

-- | The ID of the Amazon Web Services account that owns the virtual private
-- interface.
allocatePrivateVirtualInterface_ownerAccount :: Lens.Lens' AllocatePrivateVirtualInterface Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    AllocatePrivateVirtualInterface
  where
  hashWithSalt
    _salt
    AllocatePrivateVirtualInterface' {..} =
      _salt
        `Prelude.hashWithSalt` connectionId
        `Prelude.hashWithSalt` ownerAccount
        `Prelude.hashWithSalt` newPrivateVirtualInterfaceAllocation'

instance
  Prelude.NFData
    AllocatePrivateVirtualInterface
  where
  rnf AllocatePrivateVirtualInterface' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf newPrivateVirtualInterfaceAllocation'

instance
  Data.ToHeaders
    AllocatePrivateVirtualInterface
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.AllocatePrivateVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AllocatePrivateVirtualInterface where
  toJSON AllocatePrivateVirtualInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Data..= connectionId),
            Prelude.Just ("ownerAccount" Data..= ownerAccount),
            Prelude.Just
              ( "newPrivateVirtualInterfaceAllocation"
                  Data..= newPrivateVirtualInterfaceAllocation'
              )
          ]
      )

instance Data.ToPath AllocatePrivateVirtualInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery AllocatePrivateVirtualInterface where
  toQuery = Prelude.const Prelude.mempty
