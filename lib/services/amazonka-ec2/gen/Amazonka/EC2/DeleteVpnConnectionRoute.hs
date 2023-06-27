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
-- Module      : Amazonka.EC2.DeleteVpnConnectionRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified static route associated with a VPN connection
-- between an existing virtual private gateway and a VPN customer gateway.
-- The static route allows traffic to be routed from the virtual private
-- gateway to the VPN customer gateway.
module Amazonka.EC2.DeleteVpnConnectionRoute
  ( -- * Creating a Request
    DeleteVpnConnectionRoute (..),
    newDeleteVpnConnectionRoute,

    -- * Request Lenses
    deleteVpnConnectionRoute_destinationCidrBlock,
    deleteVpnConnectionRoute_vpnConnectionId,

    -- * Destructuring the Response
    DeleteVpnConnectionRouteResponse (..),
    newDeleteVpnConnectionRouteResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DeleteVpnConnectionRoute.
--
-- /See:/ 'newDeleteVpnConnectionRoute' smart constructor.
data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute'
  { -- | The CIDR block associated with the local subnet of the customer network.
    destinationCidrBlock :: Prelude.Text,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpnConnectionRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCidrBlock', 'deleteVpnConnectionRoute_destinationCidrBlock' - The CIDR block associated with the local subnet of the customer network.
--
-- 'vpnConnectionId', 'deleteVpnConnectionRoute_vpnConnectionId' - The ID of the VPN connection.
newDeleteVpnConnectionRoute ::
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  -- | 'vpnConnectionId'
  Prelude.Text ->
  DeleteVpnConnectionRoute
newDeleteVpnConnectionRoute
  pDestinationCidrBlock_
  pVpnConnectionId_ =
    DeleteVpnConnectionRoute'
      { destinationCidrBlock =
          pDestinationCidrBlock_,
        vpnConnectionId = pVpnConnectionId_
      }

-- | The CIDR block associated with the local subnet of the customer network.
deleteVpnConnectionRoute_destinationCidrBlock :: Lens.Lens' DeleteVpnConnectionRoute Prelude.Text
deleteVpnConnectionRoute_destinationCidrBlock = Lens.lens (\DeleteVpnConnectionRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteVpnConnectionRoute' {} a -> s {destinationCidrBlock = a} :: DeleteVpnConnectionRoute)

-- | The ID of the VPN connection.
deleteVpnConnectionRoute_vpnConnectionId :: Lens.Lens' DeleteVpnConnectionRoute Prelude.Text
deleteVpnConnectionRoute_vpnConnectionId = Lens.lens (\DeleteVpnConnectionRoute' {vpnConnectionId} -> vpnConnectionId) (\s@DeleteVpnConnectionRoute' {} a -> s {vpnConnectionId = a} :: DeleteVpnConnectionRoute)

instance Core.AWSRequest DeleteVpnConnectionRoute where
  type
    AWSResponse DeleteVpnConnectionRoute =
      DeleteVpnConnectionRouteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVpnConnectionRouteResponse'

instance Prelude.Hashable DeleteVpnConnectionRoute where
  hashWithSalt _salt DeleteVpnConnectionRoute' {..} =
    _salt
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` vpnConnectionId

instance Prelude.NFData DeleteVpnConnectionRoute where
  rnf DeleteVpnConnectionRoute' {..} =
    Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf vpnConnectionId

instance Data.ToHeaders DeleteVpnConnectionRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVpnConnectionRoute where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVpnConnectionRoute where
  toQuery DeleteVpnConnectionRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteVpnConnectionRoute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DestinationCidrBlock" Data.=: destinationCidrBlock,
        "VpnConnectionId" Data.=: vpnConnectionId
      ]

-- | /See:/ 'newDeleteVpnConnectionRouteResponse' smart constructor.
data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpnConnectionRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVpnConnectionRouteResponse ::
  DeleteVpnConnectionRouteResponse
newDeleteVpnConnectionRouteResponse =
  DeleteVpnConnectionRouteResponse'

instance
  Prelude.NFData
    DeleteVpnConnectionRouteResponse
  where
  rnf _ = ()
