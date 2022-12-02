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
-- Module      : Amazonka.DirectConnect.DeleteVirtualInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual interface.
module Amazonka.DirectConnect.DeleteVirtualInterface
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVirtualInterface' smart constructor.
data DeleteVirtualInterface = DeleteVirtualInterface'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteVirtualInterface
newDeleteVirtualInterface pVirtualInterfaceId_ =
  DeleteVirtualInterface'
    { virtualInterfaceId =
        pVirtualInterfaceId_
    }

-- | The ID of the virtual interface.
deleteVirtualInterface_virtualInterfaceId :: Lens.Lens' DeleteVirtualInterface Prelude.Text
deleteVirtualInterface_virtualInterfaceId = Lens.lens (\DeleteVirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@DeleteVirtualInterface' {} a -> s {virtualInterfaceId = a} :: DeleteVirtualInterface)

instance Core.AWSRequest DeleteVirtualInterface where
  type
    AWSResponse DeleteVirtualInterface =
      DeleteVirtualInterfaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVirtualInterfaceResponse'
            Prelude.<$> (x Data..?> "virtualInterfaceState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVirtualInterface where
  hashWithSalt _salt DeleteVirtualInterface' {..} =
    _salt `Prelude.hashWithSalt` virtualInterfaceId

instance Prelude.NFData DeleteVirtualInterface where
  rnf DeleteVirtualInterface' {..} =
    Prelude.rnf virtualInterfaceId

instance Data.ToHeaders DeleteVirtualInterface where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DeleteVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteVirtualInterface where
  toJSON DeleteVirtualInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("virtualInterfaceId" Data..= virtualInterfaceId)
          ]
      )

instance Data.ToPath DeleteVirtualInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVirtualInterface where
  toQuery = Prelude.const Prelude.mempty

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
    virtualInterfaceState :: Prelude.Maybe VirtualInterfaceState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteVirtualInterfaceResponse
newDeleteVirtualInterfaceResponse pHttpStatus_ =
  DeleteVirtualInterfaceResponse'
    { virtualInterfaceState =
        Prelude.Nothing,
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
deleteVirtualInterfaceResponse_virtualInterfaceState :: Lens.Lens' DeleteVirtualInterfaceResponse (Prelude.Maybe VirtualInterfaceState)
deleteVirtualInterfaceResponse_virtualInterfaceState = Lens.lens (\DeleteVirtualInterfaceResponse' {virtualInterfaceState} -> virtualInterfaceState) (\s@DeleteVirtualInterfaceResponse' {} a -> s {virtualInterfaceState = a} :: DeleteVirtualInterfaceResponse)

-- | The response's http status code.
deleteVirtualInterfaceResponse_httpStatus :: Lens.Lens' DeleteVirtualInterfaceResponse Prelude.Int
deleteVirtualInterfaceResponse_httpStatus = Lens.lens (\DeleteVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@DeleteVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: DeleteVirtualInterfaceResponse)

instance
  Prelude.NFData
    DeleteVirtualInterfaceResponse
  where
  rnf DeleteVirtualInterfaceResponse' {..} =
    Prelude.rnf virtualInterfaceState
      `Prelude.seq` Prelude.rnf httpStatus
