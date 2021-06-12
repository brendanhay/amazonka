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
-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified interconnect.
--
-- Intended for use by AWS Direct Connect Partners only.
module Network.AWS.DirectConnect.DeleteInterconnect
  ( -- * Creating a Request
    DeleteInterconnect (..),
    newDeleteInterconnect,

    -- * Request Lenses
    deleteInterconnect_interconnectId,

    -- * Destructuring the Response
    DeleteInterconnectResponse (..),
    newDeleteInterconnectResponse,

    -- * Response Lenses
    deleteInterconnectResponse_interconnectState,
    deleteInterconnectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInterconnect' smart constructor.
data DeleteInterconnect = DeleteInterconnect'
  { -- | The ID of the interconnect.
    interconnectId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInterconnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interconnectId', 'deleteInterconnect_interconnectId' - The ID of the interconnect.
newDeleteInterconnect ::
  -- | 'interconnectId'
  Core.Text ->
  DeleteInterconnect
newDeleteInterconnect pInterconnectId_ =
  DeleteInterconnect'
    { interconnectId =
        pInterconnectId_
    }

-- | The ID of the interconnect.
deleteInterconnect_interconnectId :: Lens.Lens' DeleteInterconnect Core.Text
deleteInterconnect_interconnectId = Lens.lens (\DeleteInterconnect' {interconnectId} -> interconnectId) (\s@DeleteInterconnect' {} a -> s {interconnectId = a} :: DeleteInterconnect)

instance Core.AWSRequest DeleteInterconnect where
  type
    AWSResponse DeleteInterconnect =
      DeleteInterconnectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInterconnectResponse'
            Core.<$> (x Core..?> "interconnectState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteInterconnect

instance Core.NFData DeleteInterconnect

instance Core.ToHeaders DeleteInterconnect where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DeleteInterconnect" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteInterconnect where
  toJSON DeleteInterconnect' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("interconnectId" Core..= interconnectId)
          ]
      )

instance Core.ToPath DeleteInterconnect where
  toPath = Core.const "/"

instance Core.ToQuery DeleteInterconnect where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteInterconnectResponse' smart constructor.
data DeleteInterconnectResponse = DeleteInterconnectResponse'
  { -- | The state of the interconnect. The following are the possible values:
    --
    -- -   @requested@: The initial state of an interconnect. The interconnect
    --     stays in the requested state until the Letter of Authorization (LOA)
    --     is sent to the customer.
    --
    -- -   @pending@: The interconnect is approved, and is being initialized.
    --
    -- -   @available@: The network link is up, and the interconnect is ready
    --     for use.
    --
    -- -   @down@: The network link is down.
    --
    -- -   @deleting@: The interconnect is being deleted.
    --
    -- -   @deleted@: The interconnect is deleted.
    --
    -- -   @unknown@: The state of the interconnect is not available.
    interconnectState :: Core.Maybe InterconnectState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInterconnectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interconnectState', 'deleteInterconnectResponse_interconnectState' - The state of the interconnect. The following are the possible values:
--
-- -   @requested@: The initial state of an interconnect. The interconnect
--     stays in the requested state until the Letter of Authorization (LOA)
--     is sent to the customer.
--
-- -   @pending@: The interconnect is approved, and is being initialized.
--
-- -   @available@: The network link is up, and the interconnect is ready
--     for use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The interconnect is being deleted.
--
-- -   @deleted@: The interconnect is deleted.
--
-- -   @unknown@: The state of the interconnect is not available.
--
-- 'httpStatus', 'deleteInterconnectResponse_httpStatus' - The response's http status code.
newDeleteInterconnectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteInterconnectResponse
newDeleteInterconnectResponse pHttpStatus_ =
  DeleteInterconnectResponse'
    { interconnectState =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the interconnect. The following are the possible values:
--
-- -   @requested@: The initial state of an interconnect. The interconnect
--     stays in the requested state until the Letter of Authorization (LOA)
--     is sent to the customer.
--
-- -   @pending@: The interconnect is approved, and is being initialized.
--
-- -   @available@: The network link is up, and the interconnect is ready
--     for use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The interconnect is being deleted.
--
-- -   @deleted@: The interconnect is deleted.
--
-- -   @unknown@: The state of the interconnect is not available.
deleteInterconnectResponse_interconnectState :: Lens.Lens' DeleteInterconnectResponse (Core.Maybe InterconnectState)
deleteInterconnectResponse_interconnectState = Lens.lens (\DeleteInterconnectResponse' {interconnectState} -> interconnectState) (\s@DeleteInterconnectResponse' {} a -> s {interconnectState = a} :: DeleteInterconnectResponse)

-- | The response's http status code.
deleteInterconnectResponse_httpStatus :: Lens.Lens' DeleteInterconnectResponse Core.Int
deleteInterconnectResponse_httpStatus = Lens.lens (\DeleteInterconnectResponse' {httpStatus} -> httpStatus) (\s@DeleteInterconnectResponse' {} a -> s {httpStatus = a} :: DeleteInterconnectResponse)

instance Core.NFData DeleteInterconnectResponse
