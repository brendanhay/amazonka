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
-- Module      : Amazonka.EC2.CreateVpnConnectionRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route associated with a VPN connection between an
-- existing virtual private gateway and a VPN customer gateway. The static
-- route allows traffic to be routed from the virtual private gateway to
-- the VPN customer gateway.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html Amazon Web Services Site-to-Site VPN>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
module Amazonka.EC2.CreateVpnConnectionRoute
  ( -- * Creating a Request
    CreateVpnConnectionRoute (..),
    newCreateVpnConnectionRoute,

    -- * Request Lenses
    createVpnConnectionRoute_destinationCidrBlock,
    createVpnConnectionRoute_vpnConnectionId,

    -- * Destructuring the Response
    CreateVpnConnectionRouteResponse (..),
    newCreateVpnConnectionRouteResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CreateVpnConnectionRoute.
--
-- /See:/ 'newCreateVpnConnectionRoute' smart constructor.
data CreateVpnConnectionRoute = CreateVpnConnectionRoute'
  { -- | The CIDR block associated with the local subnet of the customer network.
    destinationCidrBlock :: Prelude.Text,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpnConnectionRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCidrBlock', 'createVpnConnectionRoute_destinationCidrBlock' - The CIDR block associated with the local subnet of the customer network.
--
-- 'vpnConnectionId', 'createVpnConnectionRoute_vpnConnectionId' - The ID of the VPN connection.
newCreateVpnConnectionRoute ::
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  -- | 'vpnConnectionId'
  Prelude.Text ->
  CreateVpnConnectionRoute
newCreateVpnConnectionRoute
  pDestinationCidrBlock_
  pVpnConnectionId_ =
    CreateVpnConnectionRoute'
      { destinationCidrBlock =
          pDestinationCidrBlock_,
        vpnConnectionId = pVpnConnectionId_
      }

-- | The CIDR block associated with the local subnet of the customer network.
createVpnConnectionRoute_destinationCidrBlock :: Lens.Lens' CreateVpnConnectionRoute Prelude.Text
createVpnConnectionRoute_destinationCidrBlock = Lens.lens (\CreateVpnConnectionRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@CreateVpnConnectionRoute' {} a -> s {destinationCidrBlock = a} :: CreateVpnConnectionRoute)

-- | The ID of the VPN connection.
createVpnConnectionRoute_vpnConnectionId :: Lens.Lens' CreateVpnConnectionRoute Prelude.Text
createVpnConnectionRoute_vpnConnectionId = Lens.lens (\CreateVpnConnectionRoute' {vpnConnectionId} -> vpnConnectionId) (\s@CreateVpnConnectionRoute' {} a -> s {vpnConnectionId = a} :: CreateVpnConnectionRoute)

instance Core.AWSRequest CreateVpnConnectionRoute where
  type
    AWSResponse CreateVpnConnectionRoute =
      CreateVpnConnectionRouteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      CreateVpnConnectionRouteResponse'

instance Prelude.Hashable CreateVpnConnectionRoute where
  hashWithSalt _salt CreateVpnConnectionRoute' {..} =
    _salt
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` vpnConnectionId

instance Prelude.NFData CreateVpnConnectionRoute where
  rnf CreateVpnConnectionRoute' {..} =
    Prelude.rnf destinationCidrBlock `Prelude.seq`
      Prelude.rnf vpnConnectionId

instance Data.ToHeaders CreateVpnConnectionRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateVpnConnectionRoute where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVpnConnectionRoute where
  toQuery CreateVpnConnectionRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateVpnConnectionRoute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DestinationCidrBlock" Data.=: destinationCidrBlock,
        "VpnConnectionId" Data.=: vpnConnectionId
      ]

-- | /See:/ 'newCreateVpnConnectionRouteResponse' smart constructor.
data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpnConnectionRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateVpnConnectionRouteResponse ::
  CreateVpnConnectionRouteResponse
newCreateVpnConnectionRouteResponse =
  CreateVpnConnectionRouteResponse'

instance
  Prelude.NFData
    CreateVpnConnectionRouteResponse
  where
  rnf _ = ()
