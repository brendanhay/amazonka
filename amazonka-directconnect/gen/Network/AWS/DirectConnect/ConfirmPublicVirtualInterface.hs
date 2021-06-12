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
-- Module      : Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a public virtual interface created by another AWS
-- account.
--
-- After the virtual interface owner makes this call, the specified virtual
-- interface is created and made available to handle traffic.
module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
  ( -- * Creating a Request
    ConfirmPublicVirtualInterface (..),
    newConfirmPublicVirtualInterface,

    -- * Request Lenses
    confirmPublicVirtualInterface_virtualInterfaceId,

    -- * Destructuring the Response
    ConfirmPublicVirtualInterfaceResponse (..),
    newConfirmPublicVirtualInterfaceResponse,

    -- * Response Lenses
    confirmPublicVirtualInterfaceResponse_virtualInterfaceState,
    confirmPublicVirtualInterfaceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newConfirmPublicVirtualInterface' smart constructor.
data ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfirmPublicVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceId', 'confirmPublicVirtualInterface_virtualInterfaceId' - The ID of the virtual interface.
newConfirmPublicVirtualInterface ::
  -- | 'virtualInterfaceId'
  Core.Text ->
  ConfirmPublicVirtualInterface
newConfirmPublicVirtualInterface pVirtualInterfaceId_ =
  ConfirmPublicVirtualInterface'
    { virtualInterfaceId =
        pVirtualInterfaceId_
    }

-- | The ID of the virtual interface.
confirmPublicVirtualInterface_virtualInterfaceId :: Lens.Lens' ConfirmPublicVirtualInterface Core.Text
confirmPublicVirtualInterface_virtualInterfaceId = Lens.lens (\ConfirmPublicVirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@ConfirmPublicVirtualInterface' {} a -> s {virtualInterfaceId = a} :: ConfirmPublicVirtualInterface)

instance
  Core.AWSRequest
    ConfirmPublicVirtualInterface
  where
  type
    AWSResponse ConfirmPublicVirtualInterface =
      ConfirmPublicVirtualInterfaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmPublicVirtualInterfaceResponse'
            Core.<$> (x Core..?> "virtualInterfaceState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ConfirmPublicVirtualInterface

instance Core.NFData ConfirmPublicVirtualInterface

instance Core.ToHeaders ConfirmPublicVirtualInterface where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.ConfirmPublicVirtualInterface" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ConfirmPublicVirtualInterface where
  toJSON ConfirmPublicVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("virtualInterfaceId" Core..= virtualInterfaceId)
          ]
      )

instance Core.ToPath ConfirmPublicVirtualInterface where
  toPath = Core.const "/"

instance Core.ToQuery ConfirmPublicVirtualInterface where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newConfirmPublicVirtualInterfaceResponse' smart constructor.
data ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse'
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
-- Create a value of 'ConfirmPublicVirtualInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceState', 'confirmPublicVirtualInterfaceResponse_virtualInterfaceState' - The state of the virtual interface. The following are the possible
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
-- 'httpStatus', 'confirmPublicVirtualInterfaceResponse_httpStatus' - The response's http status code.
newConfirmPublicVirtualInterfaceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ConfirmPublicVirtualInterfaceResponse
newConfirmPublicVirtualInterfaceResponse pHttpStatus_ =
  ConfirmPublicVirtualInterfaceResponse'
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
confirmPublicVirtualInterfaceResponse_virtualInterfaceState :: Lens.Lens' ConfirmPublicVirtualInterfaceResponse (Core.Maybe VirtualInterfaceState)
confirmPublicVirtualInterfaceResponse_virtualInterfaceState = Lens.lens (\ConfirmPublicVirtualInterfaceResponse' {virtualInterfaceState} -> virtualInterfaceState) (\s@ConfirmPublicVirtualInterfaceResponse' {} a -> s {virtualInterfaceState = a} :: ConfirmPublicVirtualInterfaceResponse)

-- | The response's http status code.
confirmPublicVirtualInterfaceResponse_httpStatus :: Lens.Lens' ConfirmPublicVirtualInterfaceResponse Core.Int
confirmPublicVirtualInterfaceResponse_httpStatus = Lens.lens (\ConfirmPublicVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@ConfirmPublicVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: ConfirmPublicVirtualInterfaceResponse)

instance
  Core.NFData
    ConfirmPublicVirtualInterfaceResponse
