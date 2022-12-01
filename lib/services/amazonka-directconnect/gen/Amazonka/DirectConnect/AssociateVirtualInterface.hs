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
-- Module      : Amazonka.DirectConnect.AssociateVirtualInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a virtual interface with a specified link aggregation group
-- (LAG) or connection. Connectivity to Amazon Web Services is temporarily
-- interrupted as the virtual interface is being migrated. If the target
-- connection or LAG has an associated virtual interface with a conflicting
-- VLAN number or a conflicting IP address, the operation fails.
--
-- Virtual interfaces associated with a hosted connection cannot be
-- associated with a LAG; hosted connections must be migrated along with
-- their virtual interfaces using AssociateHostedConnection.
--
-- To reassociate a virtual interface to a new connection or LAG, the
-- requester must own either the virtual interface itself or the connection
-- to which the virtual interface is currently associated. Additionally,
-- the requester must own the connection or LAG for the association.
module Amazonka.DirectConnect.AssociateVirtualInterface
  ( -- * Creating a Request
    AssociateVirtualInterface (..),
    newAssociateVirtualInterface,

    -- * Request Lenses
    associateVirtualInterface_virtualInterfaceId,
    associateVirtualInterface_connectionId,

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

-- | /See:/ 'newAssociateVirtualInterface' smart constructor.
data AssociateVirtualInterface = AssociateVirtualInterface'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Text,
    -- | The ID of the LAG or connection.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceId', 'associateVirtualInterface_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'connectionId', 'associateVirtualInterface_connectionId' - The ID of the LAG or connection.
newAssociateVirtualInterface ::
  -- | 'virtualInterfaceId'
  Prelude.Text ->
  -- | 'connectionId'
  Prelude.Text ->
  AssociateVirtualInterface
newAssociateVirtualInterface
  pVirtualInterfaceId_
  pConnectionId_ =
    AssociateVirtualInterface'
      { virtualInterfaceId =
          pVirtualInterfaceId_,
        connectionId = pConnectionId_
      }

-- | The ID of the virtual interface.
associateVirtualInterface_virtualInterfaceId :: Lens.Lens' AssociateVirtualInterface Prelude.Text
associateVirtualInterface_virtualInterfaceId = Lens.lens (\AssociateVirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@AssociateVirtualInterface' {} a -> s {virtualInterfaceId = a} :: AssociateVirtualInterface)

-- | The ID of the LAG or connection.
associateVirtualInterface_connectionId :: Lens.Lens' AssociateVirtualInterface Prelude.Text
associateVirtualInterface_connectionId = Lens.lens (\AssociateVirtualInterface' {connectionId} -> connectionId) (\s@AssociateVirtualInterface' {} a -> s {connectionId = a} :: AssociateVirtualInterface)

instance Core.AWSRequest AssociateVirtualInterface where
  type
    AWSResponse AssociateVirtualInterface =
      VirtualInterface
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable AssociateVirtualInterface where
  hashWithSalt _salt AssociateVirtualInterface' {..} =
    _salt `Prelude.hashWithSalt` virtualInterfaceId
      `Prelude.hashWithSalt` connectionId

instance Prelude.NFData AssociateVirtualInterface where
  rnf AssociateVirtualInterface' {..} =
    Prelude.rnf virtualInterfaceId
      `Prelude.seq` Prelude.rnf connectionId

instance Core.ToHeaders AssociateVirtualInterface where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AssociateVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateVirtualInterface where
  toJSON AssociateVirtualInterface' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("virtualInterfaceId" Core..= virtualInterfaceId),
            Prelude.Just ("connectionId" Core..= connectionId)
          ]
      )

instance Core.ToPath AssociateVirtualInterface where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateVirtualInterface where
  toQuery = Prelude.const Prelude.mempty
