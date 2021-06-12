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
-- Module      : Network.AWS.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape from the virtual tape shelf (VTS).
-- This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.DeleteTapeArchive
  ( -- * Creating a Request
    DeleteTapeArchive (..),
    newDeleteTapeArchive,

    -- * Request Lenses
    deleteTapeArchive_bypassGovernanceRetention,
    deleteTapeArchive_tapeARN,

    -- * Destructuring the Response
    DeleteTapeArchiveResponse (..),
    newDeleteTapeArchiveResponse,

    -- * Response Lenses
    deleteTapeArchiveResponse_tapeARN,
    deleteTapeArchiveResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DeleteTapeArchiveInput
--
-- /See:/ 'newDeleteTapeArchive' smart constructor.
data DeleteTapeArchive = DeleteTapeArchive'
  { -- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool
    -- with tape retention lock. Only archived tapes with tape retention lock
    -- set to @governance@ can be deleted. Archived tapes with tape retention
    -- lock set to @compliance@ can\'t be deleted.
    bypassGovernanceRetention :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the virtual tape to delete from the
    -- virtual tape shelf (VTS).
    tapeARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTapeArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassGovernanceRetention', 'deleteTapeArchive_bypassGovernanceRetention' - Set to @TRUE@ to delete an archived tape that belongs to a custom pool
-- with tape retention lock. Only archived tapes with tape retention lock
-- set to @governance@ can be deleted. Archived tapes with tape retention
-- lock set to @compliance@ can\'t be deleted.
--
-- 'tapeARN', 'deleteTapeArchive_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape to delete from the
-- virtual tape shelf (VTS).
newDeleteTapeArchive ::
  -- | 'tapeARN'
  Core.Text ->
  DeleteTapeArchive
newDeleteTapeArchive pTapeARN_ =
  DeleteTapeArchive'
    { bypassGovernanceRetention =
        Core.Nothing,
      tapeARN = pTapeARN_
    }

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool
-- with tape retention lock. Only archived tapes with tape retention lock
-- set to @governance@ can be deleted. Archived tapes with tape retention
-- lock set to @compliance@ can\'t be deleted.
deleteTapeArchive_bypassGovernanceRetention :: Lens.Lens' DeleteTapeArchive (Core.Maybe Core.Bool)
deleteTapeArchive_bypassGovernanceRetention = Lens.lens (\DeleteTapeArchive' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@DeleteTapeArchive' {} a -> s {bypassGovernanceRetention = a} :: DeleteTapeArchive)

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the
-- virtual tape shelf (VTS).
deleteTapeArchive_tapeARN :: Lens.Lens' DeleteTapeArchive Core.Text
deleteTapeArchive_tapeARN = Lens.lens (\DeleteTapeArchive' {tapeARN} -> tapeARN) (\s@DeleteTapeArchive' {} a -> s {tapeARN = a} :: DeleteTapeArchive)

instance Core.AWSRequest DeleteTapeArchive where
  type
    AWSResponse DeleteTapeArchive =
      DeleteTapeArchiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapeArchiveResponse'
            Core.<$> (x Core..?> "TapeARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTapeArchive

instance Core.NFData DeleteTapeArchive

instance Core.ToHeaders DeleteTapeArchive where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DeleteTapeArchive" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteTapeArchive where
  toJSON DeleteTapeArchive' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BypassGovernanceRetention" Core..=)
              Core.<$> bypassGovernanceRetention,
            Core.Just ("TapeARN" Core..= tapeARN)
          ]
      )

instance Core.ToPath DeleteTapeArchive where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTapeArchive where
  toQuery = Core.const Core.mempty

-- | DeleteTapeArchiveOutput
--
-- /See:/ 'newDeleteTapeArchiveResponse' smart constructor.
data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
    -- the virtual tape shelf (VTS).
    tapeARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTapeArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'deleteTapeArchiveResponse_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape that was deleted from
-- the virtual tape shelf (VTS).
--
-- 'httpStatus', 'deleteTapeArchiveResponse_httpStatus' - The response's http status code.
newDeleteTapeArchiveResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTapeArchiveResponse
newDeleteTapeArchiveResponse pHttpStatus_ =
  DeleteTapeArchiveResponse'
    { tapeARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
-- the virtual tape shelf (VTS).
deleteTapeArchiveResponse_tapeARN :: Lens.Lens' DeleteTapeArchiveResponse (Core.Maybe Core.Text)
deleteTapeArchiveResponse_tapeARN = Lens.lens (\DeleteTapeArchiveResponse' {tapeARN} -> tapeARN) (\s@DeleteTapeArchiveResponse' {} a -> s {tapeARN = a} :: DeleteTapeArchiveResponse)

-- | The response's http status code.
deleteTapeArchiveResponse_httpStatus :: Lens.Lens' DeleteTapeArchiveResponse Core.Int
deleteTapeArchiveResponse_httpStatus = Lens.lens (\DeleteTapeArchiveResponse' {httpStatus} -> httpStatus) (\s@DeleteTapeArchiveResponse' {} a -> s {httpStatus = a} :: DeleteTapeArchiveResponse)

instance Core.NFData DeleteTapeArchiveResponse
