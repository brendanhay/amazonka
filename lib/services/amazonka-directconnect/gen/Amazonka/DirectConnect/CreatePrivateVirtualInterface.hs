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
-- Module      : Amazonka.DirectConnect.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a private virtual interface. A virtual interface is the VLAN
-- that transports Direct Connect traffic. A private virtual interface can
-- be connected to either a Direct Connect gateway or a Virtual Private
-- Gateway (VGW). Connecting the private virtual interface to a Direct
-- Connect gateway enables the possibility for connecting to multiple VPCs,
-- including VPCs in different Amazon Web Services Regions. Connecting the
-- private virtual interface to a VGW only provides access to a single VPC
-- within the same Region.
--
-- Setting the MTU of a virtual interface to 9001 (jumbo frames) can cause
-- an update to the underlying physical connection if it wasn\'t updated to
-- support jumbo frames. Updating the connection disrupts network
-- connectivity for all virtual interfaces associated with the connection
-- for up to 30 seconds. To check whether your connection supports jumbo
-- frames, call DescribeConnections. To check whether your virtual
-- interface supports jumbo frames, call DescribeVirtualInterfaces.
module Amazonka.DirectConnect.CreatePrivateVirtualInterface
  ( -- * Creating a Request
    CreatePrivateVirtualInterface (..),
    newCreatePrivateVirtualInterface,

    -- * Request Lenses
    createPrivateVirtualInterface_connectionId,
    createPrivateVirtualInterface_newPrivateVirtualInterface,

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

-- | /See:/ 'newCreatePrivateVirtualInterface' smart constructor.
data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface'
  { -- | The ID of the connection.
    connectionId :: Prelude.Text,
    -- | Information about the private virtual interface.
    newPrivateVirtualInterface' :: NewPrivateVirtualInterface
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePrivateVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'createPrivateVirtualInterface_connectionId' - The ID of the connection.
--
-- 'newPrivateVirtualInterface'', 'createPrivateVirtualInterface_newPrivateVirtualInterface' - Information about the private virtual interface.
newCreatePrivateVirtualInterface ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'newPrivateVirtualInterface''
  NewPrivateVirtualInterface ->
  CreatePrivateVirtualInterface
newCreatePrivateVirtualInterface
  pConnectionId_
  pNewPrivateVirtualInterface_ =
    CreatePrivateVirtualInterface'
      { connectionId =
          pConnectionId_,
        newPrivateVirtualInterface' =
          pNewPrivateVirtualInterface_
      }

-- | The ID of the connection.
createPrivateVirtualInterface_connectionId :: Lens.Lens' CreatePrivateVirtualInterface Prelude.Text
createPrivateVirtualInterface_connectionId = Lens.lens (\CreatePrivateVirtualInterface' {connectionId} -> connectionId) (\s@CreatePrivateVirtualInterface' {} a -> s {connectionId = a} :: CreatePrivateVirtualInterface)

-- | Information about the private virtual interface.
createPrivateVirtualInterface_newPrivateVirtualInterface :: Lens.Lens' CreatePrivateVirtualInterface NewPrivateVirtualInterface
createPrivateVirtualInterface_newPrivateVirtualInterface = Lens.lens (\CreatePrivateVirtualInterface' {newPrivateVirtualInterface'} -> newPrivateVirtualInterface') (\s@CreatePrivateVirtualInterface' {} a -> s {newPrivateVirtualInterface' = a} :: CreatePrivateVirtualInterface)

instance
  Core.AWSRequest
    CreatePrivateVirtualInterface
  where
  type
    AWSResponse CreatePrivateVirtualInterface =
      VirtualInterface
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    CreatePrivateVirtualInterface
  where
  hashWithSalt _salt CreatePrivateVirtualInterface' {..} =
    _salt `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` newPrivateVirtualInterface'

instance Prelude.NFData CreatePrivateVirtualInterface where
  rnf CreatePrivateVirtualInterface' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf newPrivateVirtualInterface'

instance Core.ToHeaders CreatePrivateVirtualInterface where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreatePrivateVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePrivateVirtualInterface where
  toJSON CreatePrivateVirtualInterface' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Core..= connectionId),
            Prelude.Just
              ( "newPrivateVirtualInterface"
                  Core..= newPrivateVirtualInterface'
              )
          ]
      )

instance Core.ToPath CreatePrivateVirtualInterface where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePrivateVirtualInterface where
  toQuery = Prelude.const Prelude.mempty
