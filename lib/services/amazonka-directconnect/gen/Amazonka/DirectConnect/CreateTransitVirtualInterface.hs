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
-- Module      : Amazonka.DirectConnect.CreateTransitVirtualInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit virtual interface. A transit virtual interface should
-- be used to access one or more transit gateways associated with Direct
-- Connect gateways. A transit virtual interface enables the connection of
-- multiple VPCs attached to a transit gateway to a Direct Connect gateway.
--
-- If you associate your transit gateway with one or more Direct Connect
-- gateways, the Autonomous System Number (ASN) used by the transit gateway
-- and the Direct Connect gateway must be different. For example, if you
-- use the default ASN 64512 for both your the transit gateway and Direct
-- Connect gateway, the association request fails.
--
-- Setting the MTU of a virtual interface to 8500 (jumbo frames) can cause
-- an update to the underlying physical connection if it wasn\'t updated to
-- support jumbo frames. Updating the connection disrupts network
-- connectivity for all virtual interfaces associated with the connection
-- for up to 30 seconds. To check whether your connection supports jumbo
-- frames, call DescribeConnections. To check whether your virtual
-- interface supports jumbo frames, call DescribeVirtualInterfaces.
module Amazonka.DirectConnect.CreateTransitVirtualInterface
  ( -- * Creating a Request
    CreateTransitVirtualInterface (..),
    newCreateTransitVirtualInterface,

    -- * Request Lenses
    createTransitVirtualInterface_connectionId,
    createTransitVirtualInterface_newTransitVirtualInterface,

    -- * Destructuring the Response
    CreateTransitVirtualInterfaceResponse (..),
    newCreateTransitVirtualInterfaceResponse,

    -- * Response Lenses
    createTransitVirtualInterfaceResponse_virtualInterface,
    createTransitVirtualInterfaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitVirtualInterface' smart constructor.
data CreateTransitVirtualInterface = CreateTransitVirtualInterface'
  { -- | The ID of the connection.
    connectionId :: Prelude.Text,
    -- | Information about the transit virtual interface.
    newTransitVirtualInterface' :: NewTransitVirtualInterface
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'createTransitVirtualInterface_connectionId' - The ID of the connection.
--
-- 'newTransitVirtualInterface'', 'createTransitVirtualInterface_newTransitVirtualInterface' - Information about the transit virtual interface.
newCreateTransitVirtualInterface ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'newTransitVirtualInterface''
  NewTransitVirtualInterface ->
  CreateTransitVirtualInterface
newCreateTransitVirtualInterface
  pConnectionId_
  pNewTransitVirtualInterface_ =
    CreateTransitVirtualInterface'
      { connectionId =
          pConnectionId_,
        newTransitVirtualInterface' =
          pNewTransitVirtualInterface_
      }

-- | The ID of the connection.
createTransitVirtualInterface_connectionId :: Lens.Lens' CreateTransitVirtualInterface Prelude.Text
createTransitVirtualInterface_connectionId = Lens.lens (\CreateTransitVirtualInterface' {connectionId} -> connectionId) (\s@CreateTransitVirtualInterface' {} a -> s {connectionId = a} :: CreateTransitVirtualInterface)

-- | Information about the transit virtual interface.
createTransitVirtualInterface_newTransitVirtualInterface :: Lens.Lens' CreateTransitVirtualInterface NewTransitVirtualInterface
createTransitVirtualInterface_newTransitVirtualInterface = Lens.lens (\CreateTransitVirtualInterface' {newTransitVirtualInterface'} -> newTransitVirtualInterface') (\s@CreateTransitVirtualInterface' {} a -> s {newTransitVirtualInterface' = a} :: CreateTransitVirtualInterface)

instance
  Core.AWSRequest
    CreateTransitVirtualInterface
  where
  type
    AWSResponse CreateTransitVirtualInterface =
      CreateTransitVirtualInterfaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTransitVirtualInterfaceResponse'
            Prelude.<$> (x Data..?> "virtualInterface")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitVirtualInterface
  where
  hashWithSalt _salt CreateTransitVirtualInterface' {..} =
    _salt
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` newTransitVirtualInterface'

instance Prelude.NFData CreateTransitVirtualInterface where
  rnf CreateTransitVirtualInterface' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf newTransitVirtualInterface'

instance Data.ToHeaders CreateTransitVirtualInterface where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.CreateTransitVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTransitVirtualInterface where
  toJSON CreateTransitVirtualInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Data..= connectionId),
            Prelude.Just
              ( "newTransitVirtualInterface"
                  Data..= newTransitVirtualInterface'
              )
          ]
      )

instance Data.ToPath CreateTransitVirtualInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTransitVirtualInterface where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTransitVirtualInterfaceResponse' smart constructor.
data CreateTransitVirtualInterfaceResponse = CreateTransitVirtualInterfaceResponse'
  { virtualInterface :: Prelude.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitVirtualInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterface', 'createTransitVirtualInterfaceResponse_virtualInterface' - Undocumented member.
--
-- 'httpStatus', 'createTransitVirtualInterfaceResponse_httpStatus' - The response's http status code.
newCreateTransitVirtualInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitVirtualInterfaceResponse
newCreateTransitVirtualInterfaceResponse pHttpStatus_ =
  CreateTransitVirtualInterfaceResponse'
    { virtualInterface =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createTransitVirtualInterfaceResponse_virtualInterface :: Lens.Lens' CreateTransitVirtualInterfaceResponse (Prelude.Maybe VirtualInterface)
createTransitVirtualInterfaceResponse_virtualInterface = Lens.lens (\CreateTransitVirtualInterfaceResponse' {virtualInterface} -> virtualInterface) (\s@CreateTransitVirtualInterfaceResponse' {} a -> s {virtualInterface = a} :: CreateTransitVirtualInterfaceResponse)

-- | The response's http status code.
createTransitVirtualInterfaceResponse_httpStatus :: Lens.Lens' CreateTransitVirtualInterfaceResponse Prelude.Int
createTransitVirtualInterfaceResponse_httpStatus = Lens.lens (\CreateTransitVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@CreateTransitVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: CreateTransitVirtualInterfaceResponse)

instance
  Prelude.NFData
    CreateTransitVirtualInterfaceResponse
  where
  rnf CreateTransitVirtualInterfaceResponse' {..} =
    Prelude.rnf virtualInterface
      `Prelude.seq` Prelude.rnf httpStatus
