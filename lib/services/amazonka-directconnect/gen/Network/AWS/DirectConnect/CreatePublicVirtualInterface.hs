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
-- Module      : Network.AWS.DirectConnect.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public virtual interface. A virtual interface is the VLAN that
-- transports Direct Connect traffic. A public virtual interface supports
-- sending traffic to public services of Amazon Web Services such as Amazon
-- S3.
--
-- When creating an IPv6 public virtual interface (@addressFamily@ is
-- @ipv6@), leave the @customer@ and @amazon@ address fields blank to use
-- auto-assigned IPv6 space. Custom IPv6 addresses are not supported.
module Network.AWS.DirectConnect.CreatePublicVirtualInterface
  ( -- * Creating a Request
    CreatePublicVirtualInterface (..),
    newCreatePublicVirtualInterface,

    -- * Request Lenses
    createPublicVirtualInterface_connectionId,
    createPublicVirtualInterface_newPublicVirtualInterface,

    -- * Destructuring the Response
    VirtualInterface (..),
    newVirtualInterface,

    -- * Response Lenses
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_mtu,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_customerAddress,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_virtualInterfaceState,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_directConnectGatewayId,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceType,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_jumboFrameCapable,
    virtualInterface_customerRouterConfig,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePublicVirtualInterface' smart constructor.
data CreatePublicVirtualInterface = CreatePublicVirtualInterface'
  { -- | The ID of the connection.
    connectionId :: Prelude.Text,
    -- | Information about the public virtual interface.
    newPublicVirtualInterface' :: NewPublicVirtualInterface
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublicVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'createPublicVirtualInterface_connectionId' - The ID of the connection.
--
-- 'newPublicVirtualInterface'', 'createPublicVirtualInterface_newPublicVirtualInterface' - Information about the public virtual interface.
newCreatePublicVirtualInterface ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'newPublicVirtualInterface''
  NewPublicVirtualInterface ->
  CreatePublicVirtualInterface
newCreatePublicVirtualInterface
  pConnectionId_
  pNewPublicVirtualInterface_ =
    CreatePublicVirtualInterface'
      { connectionId =
          pConnectionId_,
        newPublicVirtualInterface' =
          pNewPublicVirtualInterface_
      }

-- | The ID of the connection.
createPublicVirtualInterface_connectionId :: Lens.Lens' CreatePublicVirtualInterface Prelude.Text
createPublicVirtualInterface_connectionId = Lens.lens (\CreatePublicVirtualInterface' {connectionId} -> connectionId) (\s@CreatePublicVirtualInterface' {} a -> s {connectionId = a} :: CreatePublicVirtualInterface)

-- | Information about the public virtual interface.
createPublicVirtualInterface_newPublicVirtualInterface :: Lens.Lens' CreatePublicVirtualInterface NewPublicVirtualInterface
createPublicVirtualInterface_newPublicVirtualInterface = Lens.lens (\CreatePublicVirtualInterface' {newPublicVirtualInterface'} -> newPublicVirtualInterface') (\s@CreatePublicVirtualInterface' {} a -> s {newPublicVirtualInterface' = a} :: CreatePublicVirtualInterface)

instance Core.AWSRequest CreatePublicVirtualInterface where
  type
    AWSResponse CreatePublicVirtualInterface =
      VirtualInterface
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    CreatePublicVirtualInterface

instance Prelude.NFData CreatePublicVirtualInterface

instance Core.ToHeaders CreatePublicVirtualInterface where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreatePublicVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePublicVirtualInterface where
  toJSON CreatePublicVirtualInterface' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Core..= connectionId),
            Prelude.Just
              ( "newPublicVirtualInterface"
                  Core..= newPublicVirtualInterface'
              )
          ]
      )

instance Core.ToPath CreatePublicVirtualInterface where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePublicVirtualInterface where
  toQuery = Prelude.const Prelude.mempty
