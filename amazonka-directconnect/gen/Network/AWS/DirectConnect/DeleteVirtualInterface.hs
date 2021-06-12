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
-- Module      : Network.AWS.DirectConnect.DeleteVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual interface.
module Network.AWS.DirectConnect.DeleteVirtualInterface
  ( -- * Creating a Request
    DeleteVirtualInterface (..),
    newDeleteVirtualInterface,

    -- * Request Lenses
    deleteVirtualInterface_virtualInterfaceId,

    -- * Destructuring the Response
    DeleteVirtualInterfaceResponse (..),
    newDeleteVirtualInterfaceResponse,

    -- * Response Lenses
    deleteVirtualInterfaceResponse_virtualInterfaceState,
    deleteVirtualInterfaceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVirtualInterface' smart constructor.
data DeleteVirtualInterface = DeleteVirtualInterface'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceId', 'deleteVirtualInterface_virtualInterfaceId' - The ID of the virtual interface.
newDeleteVirtualInterface ::
  -- | 'virtualInterfaceId'
  Core.Text ->
  DeleteVirtualInterface
newDeleteVirtualInterface pVirtualInterfaceId_ =
  DeleteVirtualInterface'
    { virtualInterfaceId =
        pVirtualInterfaceId_
    }

-- | The ID of the virtual interface.
deleteVirtualInterface_virtualInterfaceId :: Lens.Lens' DeleteVirtualInterface Core.Text
deleteVirtualInterface_virtualInterfaceId = Lens.lens (\DeleteVirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@DeleteVirtualInterface' {} a -> s {virtualInterfaceId = a} :: DeleteVirtualInterface)

instance Core.AWSRequest DeleteVirtualInterface where
  type
    AWSResponse DeleteVirtualInterface =
      DeleteVirtualInterfaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVirtualInterfaceResponse'
            Core.<$> (x Core..?> "virtualInterfaceState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteVirtualInterface

instance Core.NFData DeleteVirtualInterface

instance Core.ToHeaders DeleteVirtualInterface where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DeleteVirtualInterface" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteVirtualInterface where
  toJSON DeleteVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("virtualInterfaceId" Core..= virtualInterfaceId)
          ]
      )

instance Core.ToPath DeleteVirtualInterface where
  toPath = Core.const "/"

instance Core.ToQuery DeleteVirtualInterface where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteVirtualInterfaceResponse' smart constructor.
data DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse'
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
-- Create a value of 'DeleteVirtualInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceState', 'deleteVirtualInterfaceResponse_virtualInterfaceState' - The state of the virtual interface. The following are the possible
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
-- 'httpStatus', 'deleteVirtualInterfaceResponse_httpStatus' - The response's http status code.
newDeleteVirtualInterfaceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteVirtualInterfaceResponse
newDeleteVirtualInterfaceResponse pHttpStatus_ =
  DeleteVirtualInterfaceResponse'
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
deleteVirtualInterfaceResponse_virtualInterfaceState :: Lens.Lens' DeleteVirtualInterfaceResponse (Core.Maybe VirtualInterfaceState)
deleteVirtualInterfaceResponse_virtualInterfaceState = Lens.lens (\DeleteVirtualInterfaceResponse' {virtualInterfaceState} -> virtualInterfaceState) (\s@DeleteVirtualInterfaceResponse' {} a -> s {virtualInterfaceState = a} :: DeleteVirtualInterfaceResponse)

-- | The response's http status code.
deleteVirtualInterfaceResponse_httpStatus :: Lens.Lens' DeleteVirtualInterfaceResponse Core.Int
deleteVirtualInterfaceResponse_httpStatus = Lens.lens (\DeleteVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@DeleteVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: DeleteVirtualInterfaceResponse)

instance Core.NFData DeleteVirtualInterfaceResponse
