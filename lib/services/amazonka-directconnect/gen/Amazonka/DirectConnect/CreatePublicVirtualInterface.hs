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
-- Module      : Amazonka.DirectConnect.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.DirectConnect.CreatePublicVirtualInterface
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    CreatePublicVirtualInterface
  where
  hashWithSalt _salt CreatePublicVirtualInterface' {..} =
    _salt
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` newPublicVirtualInterface'

instance Prelude.NFData CreatePublicVirtualInterface where
  rnf CreatePublicVirtualInterface' {..} =
    Prelude.rnf connectionId `Prelude.seq`
      Prelude.rnf newPublicVirtualInterface'

instance Data.ToHeaders CreatePublicVirtualInterface where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.CreatePublicVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePublicVirtualInterface where
  toJSON CreatePublicVirtualInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Data..= connectionId),
            Prelude.Just
              ( "newPublicVirtualInterface"
                  Data..= newPublicVirtualInterface'
              )
          ]
      )

instance Data.ToPath CreatePublicVirtualInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePublicVirtualInterface where
  toQuery = Prelude.const Prelude.mempty
