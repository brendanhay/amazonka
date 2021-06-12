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
-- Module      : Network.AWS.DirectConnect.CreateTransitVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DirectConnect.CreateTransitVirtualInterface
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTransitVirtualInterface' smart constructor.
data CreateTransitVirtualInterface = CreateTransitVirtualInterface'
  { -- | The ID of the connection.
    connectionId :: Core.Text,
    -- | Information about the transit virtual interface.
    newTransitVirtualInterface' :: NewTransitVirtualInterface
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
createTransitVirtualInterface_connectionId :: Lens.Lens' CreateTransitVirtualInterface Core.Text
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTransitVirtualInterfaceResponse'
            Core.<$> (x Core..?> "virtualInterface")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTransitVirtualInterface

instance Core.NFData CreateTransitVirtualInterface

instance Core.ToHeaders CreateTransitVirtualInterface where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreateTransitVirtualInterface" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTransitVirtualInterface where
  toJSON CreateTransitVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just
              ( "newTransitVirtualInterface"
                  Core..= newTransitVirtualInterface'
              )
          ]
      )

instance Core.ToPath CreateTransitVirtualInterface where
  toPath = Core.const "/"

instance Core.ToQuery CreateTransitVirtualInterface where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTransitVirtualInterfaceResponse' smart constructor.
data CreateTransitVirtualInterfaceResponse = CreateTransitVirtualInterfaceResponse'
  { virtualInterface :: Core.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateTransitVirtualInterfaceResponse
newCreateTransitVirtualInterfaceResponse pHttpStatus_ =
  CreateTransitVirtualInterfaceResponse'
    { virtualInterface =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createTransitVirtualInterfaceResponse_virtualInterface :: Lens.Lens' CreateTransitVirtualInterfaceResponse (Core.Maybe VirtualInterface)
createTransitVirtualInterfaceResponse_virtualInterface = Lens.lens (\CreateTransitVirtualInterfaceResponse' {virtualInterface} -> virtualInterface) (\s@CreateTransitVirtualInterfaceResponse' {} a -> s {virtualInterface = a} :: CreateTransitVirtualInterfaceResponse)

-- | The response's http status code.
createTransitVirtualInterfaceResponse_httpStatus :: Lens.Lens' CreateTransitVirtualInterfaceResponse Core.Int
createTransitVirtualInterfaceResponse_httpStatus = Lens.lens (\CreateTransitVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@CreateTransitVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: CreateTransitVirtualInterfaceResponse)

instance
  Core.NFData
    CreateTransitVirtualInterfaceResponse
