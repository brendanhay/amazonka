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
-- Module      : Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a transit virtual interface created by another AWS
-- account.
--
-- After the owner of the transit virtual interface makes this call, the
-- specified transit virtual interface is created and made available to
-- handle traffic.
module Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
  ( -- * Creating a Request
    ConfirmTransitVirtualInterface (..),
    newConfirmTransitVirtualInterface,

    -- * Request Lenses
    confirmTransitVirtualInterface_virtualInterfaceId,
    confirmTransitVirtualInterface_directConnectGatewayId,

    -- * Destructuring the Response
    ConfirmTransitVirtualInterfaceResponse (..),
    newConfirmTransitVirtualInterfaceResponse,

    -- * Response Lenses
    confirmTransitVirtualInterfaceResponse_virtualInterfaceState,
    confirmTransitVirtualInterfaceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newConfirmTransitVirtualInterface' smart constructor.
data ConfirmTransitVirtualInterface = ConfirmTransitVirtualInterface'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfirmTransitVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceId', 'confirmTransitVirtualInterface_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'directConnectGatewayId', 'confirmTransitVirtualInterface_directConnectGatewayId' - The ID of the Direct Connect gateway.
newConfirmTransitVirtualInterface ::
  -- | 'virtualInterfaceId'
  Core.Text ->
  -- | 'directConnectGatewayId'
  Core.Text ->
  ConfirmTransitVirtualInterface
newConfirmTransitVirtualInterface
  pVirtualInterfaceId_
  pDirectConnectGatewayId_ =
    ConfirmTransitVirtualInterface'
      { virtualInterfaceId =
          pVirtualInterfaceId_,
        directConnectGatewayId =
          pDirectConnectGatewayId_
      }

-- | The ID of the virtual interface.
confirmTransitVirtualInterface_virtualInterfaceId :: Lens.Lens' ConfirmTransitVirtualInterface Core.Text
confirmTransitVirtualInterface_virtualInterfaceId = Lens.lens (\ConfirmTransitVirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@ConfirmTransitVirtualInterface' {} a -> s {virtualInterfaceId = a} :: ConfirmTransitVirtualInterface)

-- | The ID of the Direct Connect gateway.
confirmTransitVirtualInterface_directConnectGatewayId :: Lens.Lens' ConfirmTransitVirtualInterface Core.Text
confirmTransitVirtualInterface_directConnectGatewayId = Lens.lens (\ConfirmTransitVirtualInterface' {directConnectGatewayId} -> directConnectGatewayId) (\s@ConfirmTransitVirtualInterface' {} a -> s {directConnectGatewayId = a} :: ConfirmTransitVirtualInterface)

instance
  Core.AWSRequest
    ConfirmTransitVirtualInterface
  where
  type
    AWSResponse ConfirmTransitVirtualInterface =
      ConfirmTransitVirtualInterfaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmTransitVirtualInterfaceResponse'
            Core.<$> (x Core..?> "virtualInterfaceState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ConfirmTransitVirtualInterface

instance Core.NFData ConfirmTransitVirtualInterface

instance
  Core.ToHeaders
    ConfirmTransitVirtualInterface
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.ConfirmTransitVirtualInterface" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ConfirmTransitVirtualInterface where
  toJSON ConfirmTransitVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("virtualInterfaceId" Core..= virtualInterfaceId),
            Core.Just
              ( "directConnectGatewayId"
                  Core..= directConnectGatewayId
              )
          ]
      )

instance Core.ToPath ConfirmTransitVirtualInterface where
  toPath = Core.const "/"

instance Core.ToQuery ConfirmTransitVirtualInterface where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newConfirmTransitVirtualInterfaceResponse' smart constructor.
data ConfirmTransitVirtualInterfaceResponse = ConfirmTransitVirtualInterfaceResponse'
  { -- | The state of the virtual interface. The following are the possible
    -- values:
    --
    -- -   @confirming@: The creation of the virtual interface is pending
    --     confirmation from the virtual interface owner. If the owner of the
    --     virtual interface is different from the owner of the connection on
    --     which it is provisioned, then the virtual interface will remain in
    --     this state until it is confirmed by the virtual interface owner.
    --
    -- -   @verifying@: This state only applies to public virtual interfaces.
    --     Each public virtual interface needs validation before the virtual
    --     interface can be created.
    --
    -- -   @pending@: A virtual interface is in this state from the time that
    --     it is created until the virtual interface is ready to forward
    --     traffic.
    --
    -- -   @available@: A virtual interface that is able to forward traffic.
    --
    -- -   @down@: A virtual interface that is BGP down.
    --
    -- -   @deleting@: A virtual interface is in this state immediately after
    --     calling DeleteVirtualInterface until it can no longer forward
    --     traffic.
    --
    -- -   @deleted@: A virtual interface that cannot forward traffic.
    --
    -- -   @rejected@: The virtual interface owner has declined creation of the
    --     virtual interface. If a virtual interface in the @Confirming@ state
    --     is deleted by the virtual interface owner, the virtual interface
    --     enters the @Rejected@ state.
    --
    -- -   @unknown@: The state of the virtual interface is not available.
    virtualInterfaceState :: Core.Maybe VirtualInterfaceState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfirmTransitVirtualInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceState', 'confirmTransitVirtualInterfaceResponse_virtualInterfaceState' - The state of the virtual interface. The following are the possible
-- values:
--
-- -   @confirming@: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
--
-- -   @verifying@: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
--
-- -   @pending@: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
--
-- -   @available@: A virtual interface that is able to forward traffic.
--
-- -   @down@: A virtual interface that is BGP down.
--
-- -   @deleting@: A virtual interface is in this state immediately after
--     calling DeleteVirtualInterface until it can no longer forward
--     traffic.
--
-- -   @deleted@: A virtual interface that cannot forward traffic.
--
-- -   @rejected@: The virtual interface owner has declined creation of the
--     virtual interface. If a virtual interface in the @Confirming@ state
--     is deleted by the virtual interface owner, the virtual interface
--     enters the @Rejected@ state.
--
-- -   @unknown@: The state of the virtual interface is not available.
--
-- 'httpStatus', 'confirmTransitVirtualInterfaceResponse_httpStatus' - The response's http status code.
newConfirmTransitVirtualInterfaceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ConfirmTransitVirtualInterfaceResponse
newConfirmTransitVirtualInterfaceResponse
  pHttpStatus_ =
    ConfirmTransitVirtualInterfaceResponse'
      { virtualInterfaceState =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The state of the virtual interface. The following are the possible
-- values:
--
-- -   @confirming@: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
--
-- -   @verifying@: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
--
-- -   @pending@: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
--
-- -   @available@: A virtual interface that is able to forward traffic.
--
-- -   @down@: A virtual interface that is BGP down.
--
-- -   @deleting@: A virtual interface is in this state immediately after
--     calling DeleteVirtualInterface until it can no longer forward
--     traffic.
--
-- -   @deleted@: A virtual interface that cannot forward traffic.
--
-- -   @rejected@: The virtual interface owner has declined creation of the
--     virtual interface. If a virtual interface in the @Confirming@ state
--     is deleted by the virtual interface owner, the virtual interface
--     enters the @Rejected@ state.
--
-- -   @unknown@: The state of the virtual interface is not available.
confirmTransitVirtualInterfaceResponse_virtualInterfaceState :: Lens.Lens' ConfirmTransitVirtualInterfaceResponse (Core.Maybe VirtualInterfaceState)
confirmTransitVirtualInterfaceResponse_virtualInterfaceState = Lens.lens (\ConfirmTransitVirtualInterfaceResponse' {virtualInterfaceState} -> virtualInterfaceState) (\s@ConfirmTransitVirtualInterfaceResponse' {} a -> s {virtualInterfaceState = a} :: ConfirmTransitVirtualInterfaceResponse)

-- | The response's http status code.
confirmTransitVirtualInterfaceResponse_httpStatus :: Lens.Lens' ConfirmTransitVirtualInterfaceResponse Core.Int
confirmTransitVirtualInterfaceResponse_httpStatus = Lens.lens (\ConfirmTransitVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@ConfirmTransitVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: ConfirmTransitVirtualInterfaceResponse)

instance
  Core.NFData
    ConfirmTransitVirtualInterfaceResponse
