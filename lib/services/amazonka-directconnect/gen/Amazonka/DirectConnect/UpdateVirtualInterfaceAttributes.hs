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
-- Module      : Amazonka.DirectConnect.UpdateVirtualInterfaceAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.DirectConnect.UpdateVirtualInterfaceAttributes
  ( -- * Creating a Request
    UpdateVirtualInterfaceAttributes (..),
    newUpdateVirtualInterfaceAttributes,

    -- * Request Lenses
    updateVirtualInterfaceAttributes_mtu,
    updateVirtualInterfaceAttributes_enableSiteLink,
    updateVirtualInterfaceAttributes_virtualInterfaceName,
    updateVirtualInterfaceAttributes_virtualInterfaceId,

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

-- | /See:/ 'newUpdateVirtualInterfaceAttributes' smart constructor.
data UpdateVirtualInterfaceAttributes = UpdateVirtualInterfaceAttributes'
  { -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to enable or disable SiteLink.
    enableSiteLink :: Prelude.Maybe Prelude.Bool,
    -- | The name of the virtual private interface.
    virtualInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private interface.
    virtualInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'enableSiteLink', 'updateVirtualInterfaceAttributes_enableSiteLink' - Indicates whether to enable or disable SiteLink.
--
-- 'virtualInterfaceName', 'updateVirtualInterfaceAttributes_virtualInterfaceName' - The name of the virtual private interface.
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
        enableSiteLink = Prelude.Nothing,
        virtualInterfaceName = Prelude.Nothing,
        virtualInterfaceId = pVirtualInterfaceId_
      }

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
updateVirtualInterfaceAttributes_mtu :: Lens.Lens' UpdateVirtualInterfaceAttributes (Prelude.Maybe Prelude.Int)
updateVirtualInterfaceAttributes_mtu = Lens.lens (\UpdateVirtualInterfaceAttributes' {mtu} -> mtu) (\s@UpdateVirtualInterfaceAttributes' {} a -> s {mtu = a} :: UpdateVirtualInterfaceAttributes)

-- | Indicates whether to enable or disable SiteLink.
updateVirtualInterfaceAttributes_enableSiteLink :: Lens.Lens' UpdateVirtualInterfaceAttributes (Prelude.Maybe Prelude.Bool)
updateVirtualInterfaceAttributes_enableSiteLink = Lens.lens (\UpdateVirtualInterfaceAttributes' {enableSiteLink} -> enableSiteLink) (\s@UpdateVirtualInterfaceAttributes' {} a -> s {enableSiteLink = a} :: UpdateVirtualInterfaceAttributes)

-- | The name of the virtual private interface.
updateVirtualInterfaceAttributes_virtualInterfaceName :: Lens.Lens' UpdateVirtualInterfaceAttributes (Prelude.Maybe Prelude.Text)
updateVirtualInterfaceAttributes_virtualInterfaceName = Lens.lens (\UpdateVirtualInterfaceAttributes' {virtualInterfaceName} -> virtualInterfaceName) (\s@UpdateVirtualInterfaceAttributes' {} a -> s {virtualInterfaceName = a} :: UpdateVirtualInterfaceAttributes)

-- | The ID of the virtual private interface.
updateVirtualInterfaceAttributes_virtualInterfaceId :: Lens.Lens' UpdateVirtualInterfaceAttributes Prelude.Text
updateVirtualInterfaceAttributes_virtualInterfaceId = Lens.lens (\UpdateVirtualInterfaceAttributes' {virtualInterfaceId} -> virtualInterfaceId) (\s@UpdateVirtualInterfaceAttributes' {} a -> s {virtualInterfaceId = a} :: UpdateVirtualInterfaceAttributes)

instance
  Core.AWSRequest
    UpdateVirtualInterfaceAttributes
  where
  type
    AWSResponse UpdateVirtualInterfaceAttributes =
      VirtualInterface
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    UpdateVirtualInterfaceAttributes
  where
  hashWithSalt
    _salt
    UpdateVirtualInterfaceAttributes' {..} =
      _salt `Prelude.hashWithSalt` mtu
        `Prelude.hashWithSalt` enableSiteLink
        `Prelude.hashWithSalt` virtualInterfaceName
        `Prelude.hashWithSalt` virtualInterfaceId

instance
  Prelude.NFData
    UpdateVirtualInterfaceAttributes
  where
  rnf UpdateVirtualInterfaceAttributes' {..} =
    Prelude.rnf mtu
      `Prelude.seq` Prelude.rnf enableSiteLink
      `Prelude.seq` Prelude.rnf virtualInterfaceName
      `Prelude.seq` Prelude.rnf virtualInterfaceId

instance
  Core.ToHeaders
    UpdateVirtualInterfaceAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.UpdateVirtualInterfaceAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateVirtualInterfaceAttributes where
  toJSON UpdateVirtualInterfaceAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("mtu" Core..=) Prelude.<$> mtu,
            ("enableSiteLink" Core..=)
              Prelude.<$> enableSiteLink,
            ("virtualInterfaceName" Core..=)
              Prelude.<$> virtualInterfaceName,
            Prelude.Just
              ("virtualInterfaceId" Core..= virtualInterfaceId)
          ]
      )

instance Core.ToPath UpdateVirtualInterfaceAttributes where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateVirtualInterfaceAttributes
  where
  toQuery = Prelude.const Prelude.mempty
