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
-- Module      : Network.AWS.StorageGateway.DeleteTapePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom tape pool. A custom tape pool can only be deleted if
-- there are no tapes in the pool and if there are no automatic tape
-- creation policies that reference the custom tape pool.
module Network.AWS.StorageGateway.DeleteTapePool
  ( -- * Creating a Request
    DeleteTapePool (..),
    newDeleteTapePool,

    -- * Request Lenses
    deleteTapePool_poolARN,

    -- * Destructuring the Response
    DeleteTapePoolResponse (..),
    newDeleteTapePoolResponse,

    -- * Response Lenses
    deleteTapePoolResponse_poolARN,
    deleteTapePoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDeleteTapePool' smart constructor.
data DeleteTapePool = DeleteTapePool'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
    poolARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTapePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolARN', 'deleteTapePool_poolARN' - The Amazon Resource Name (ARN) of the custom tape pool to delete.
newDeleteTapePool ::
  -- | 'poolARN'
  Core.Text ->
  DeleteTapePool
newDeleteTapePool pPoolARN_ =
  DeleteTapePool' {poolARN = pPoolARN_}

-- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
deleteTapePool_poolARN :: Lens.Lens' DeleteTapePool Core.Text
deleteTapePool_poolARN = Lens.lens (\DeleteTapePool' {poolARN} -> poolARN) (\s@DeleteTapePool' {} a -> s {poolARN = a} :: DeleteTapePool)

instance Core.AWSRequest DeleteTapePool where
  type
    AWSResponse DeleteTapePool =
      DeleteTapePoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapePoolResponse'
            Core.<$> (x Core..?> "PoolARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTapePool

instance Core.NFData DeleteTapePool

instance Core.ToHeaders DeleteTapePool where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DeleteTapePool" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteTapePool where
  toJSON DeleteTapePool' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("PoolARN" Core..= poolARN)]
      )

instance Core.ToPath DeleteTapePool where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTapePool where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteTapePoolResponse' smart constructor.
data DeleteTapePoolResponse = DeleteTapePoolResponse'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
    poolARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTapePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolARN', 'deleteTapePoolResponse_poolARN' - The Amazon Resource Name (ARN) of the custom tape pool being deleted.
--
-- 'httpStatus', 'deleteTapePoolResponse_httpStatus' - The response's http status code.
newDeleteTapePoolResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTapePoolResponse
newDeleteTapePoolResponse pHttpStatus_ =
  DeleteTapePoolResponse'
    { poolARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
deleteTapePoolResponse_poolARN :: Lens.Lens' DeleteTapePoolResponse (Core.Maybe Core.Text)
deleteTapePoolResponse_poolARN = Lens.lens (\DeleteTapePoolResponse' {poolARN} -> poolARN) (\s@DeleteTapePoolResponse' {} a -> s {poolARN = a} :: DeleteTapePoolResponse)

-- | The response's http status code.
deleteTapePoolResponse_httpStatus :: Lens.Lens' DeleteTapePoolResponse Core.Int
deleteTapePoolResponse_httpStatus = Lens.lens (\DeleteTapePoolResponse' {httpStatus} -> httpStatus) (\s@DeleteTapePoolResponse' {} a -> s {httpStatus = a} :: DeleteTapePoolResponse)

instance Core.NFData DeleteTapePoolResponse
