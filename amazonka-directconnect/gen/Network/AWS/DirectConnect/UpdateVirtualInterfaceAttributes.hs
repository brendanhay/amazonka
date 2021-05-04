{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified virtual private
-- interface.
--
-- Setting the MTU of a virtual interface to 9001 (jumbo frames) can cause
-- an update to the underlying physical connection if it wasn\'t updated to
-- support jumbo frames. Updating the connection disrupts network
-- connectivity for all virtual interfaces associated with the connection
-- for up to 30 seconds. To check whether your connection supports jumbo
-- frames, call DescribeConnections. To check whether your virtual q
-- interface supports jumbo frames, call DescribeVirtualInterfaces.
module Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes
  ( -- * Creating a Request
    UpdateVirtualInterfaceAttributes (..),
    newUpdateVirtualInterfaceAttributes,

    -- * Request Lenses
    updateVirtualInterfaceAttributes_mtu,
    updateVirtualInterfaceAttributes_virtualInterfaceId,

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

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateVirtualInterfaceAttributes' smart constructor.
data UpdateVirtualInterfaceAttributes = UpdateVirtualInterfaceAttributes'
  { -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Prelude.Maybe Prelude.Int,
    -- | The ID of the virtual private interface.
    virtualInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateVirtualInterfaceAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mtu', 'updateVirtualInterfaceAttributes_mtu' - The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
--
-- 'virtualInterfaceId', 'updateVirtualInterfaceAttributes_virtualInterfaceId' - The ID of the virtual private interface.
newUpdateVirtualInterfaceAttributes ::
  -- | 'virtualInterfaceId'
  Prelude.Text ->
  UpdateVirtualInterfaceAttributes
newUpdateVirtualInterfaceAttributes
  pVirtualInterfaceId_ =
    UpdateVirtualInterfaceAttributes'
      { mtu =
          Prelude.Nothing,
        virtualInterfaceId = pVirtualInterfaceId_
      }

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
updateVirtualInterfaceAttributes_mtu :: Lens.Lens' UpdateVirtualInterfaceAttributes (Prelude.Maybe Prelude.Int)
updateVirtualInterfaceAttributes_mtu = Lens.lens (\UpdateVirtualInterfaceAttributes' {mtu} -> mtu) (\s@UpdateVirtualInterfaceAttributes' {} a -> s {mtu = a} :: UpdateVirtualInterfaceAttributes)

-- | The ID of the virtual private interface.
updateVirtualInterfaceAttributes_virtualInterfaceId :: Lens.Lens' UpdateVirtualInterfaceAttributes Prelude.Text
updateVirtualInterfaceAttributes_virtualInterfaceId = Lens.lens (\UpdateVirtualInterfaceAttributes' {virtualInterfaceId} -> virtualInterfaceId) (\s@UpdateVirtualInterfaceAttributes' {} a -> s {virtualInterfaceId = a} :: UpdateVirtualInterfaceAttributes)

instance
  Prelude.AWSRequest
    UpdateVirtualInterfaceAttributes
  where
  type
    Rs UpdateVirtualInterfaceAttributes =
      VirtualInterface
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance
  Prelude.Hashable
    UpdateVirtualInterfaceAttributes

instance
  Prelude.NFData
    UpdateVirtualInterfaceAttributes

instance
  Prelude.ToHeaders
    UpdateVirtualInterfaceAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OvertureService.UpdateVirtualInterfaceAttributes" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    UpdateVirtualInterfaceAttributes
  where
  toJSON UpdateVirtualInterfaceAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("mtu" Prelude..=) Prelude.<$> mtu,
            Prelude.Just
              ( "virtualInterfaceId"
                  Prelude..= virtualInterfaceId
              )
          ]
      )

instance
  Prelude.ToPath
    UpdateVirtualInterfaceAttributes
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    UpdateVirtualInterfaceAttributes
  where
  toQuery = Prelude.const Prelude.mempty
