{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    bypassGovernanceRetention :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the virtual tape to delete from the
    -- virtual tape shelf (VTS).
    tapeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteTapeArchive
newDeleteTapeArchive pTapeARN_ =
  DeleteTapeArchive'
    { bypassGovernanceRetention =
        Prelude.Nothing,
      tapeARN = pTapeARN_
    }

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool
-- with tape retention lock. Only archived tapes with tape retention lock
-- set to @governance@ can be deleted. Archived tapes with tape retention
-- lock set to @compliance@ can\'t be deleted.
deleteTapeArchive_bypassGovernanceRetention :: Lens.Lens' DeleteTapeArchive (Prelude.Maybe Prelude.Bool)
deleteTapeArchive_bypassGovernanceRetention = Lens.lens (\DeleteTapeArchive' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@DeleteTapeArchive' {} a -> s {bypassGovernanceRetention = a} :: DeleteTapeArchive)

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the
-- virtual tape shelf (VTS).
deleteTapeArchive_tapeARN :: Lens.Lens' DeleteTapeArchive Prelude.Text
deleteTapeArchive_tapeARN = Lens.lens (\DeleteTapeArchive' {tapeARN} -> tapeARN) (\s@DeleteTapeArchive' {} a -> s {tapeARN = a} :: DeleteTapeArchive)

instance Prelude.AWSRequest DeleteTapeArchive where
  type Rs DeleteTapeArchive = DeleteTapeArchiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapeArchiveResponse'
            Prelude.<$> (x Prelude..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTapeArchive

instance Prelude.NFData DeleteTapeArchive

instance Prelude.ToHeaders DeleteTapeArchive where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DeleteTapeArchive" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTapeArchive where
  toJSON DeleteTapeArchive' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("BypassGovernanceRetention" Prelude..=)
              Prelude.<$> bypassGovernanceRetention,
            Prelude.Just ("TapeARN" Prelude..= tapeARN)
          ]
      )

instance Prelude.ToPath DeleteTapeArchive where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTapeArchive where
  toQuery = Prelude.const Prelude.mempty

-- | DeleteTapeArchiveOutput
--
-- /See:/ 'newDeleteTapeArchiveResponse' smart constructor.
data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
    -- the virtual tape shelf (VTS).
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteTapeArchiveResponse
newDeleteTapeArchiveResponse pHttpStatus_ =
  DeleteTapeArchiveResponse'
    { tapeARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
-- the virtual tape shelf (VTS).
deleteTapeArchiveResponse_tapeARN :: Lens.Lens' DeleteTapeArchiveResponse (Prelude.Maybe Prelude.Text)
deleteTapeArchiveResponse_tapeARN = Lens.lens (\DeleteTapeArchiveResponse' {tapeARN} -> tapeARN) (\s@DeleteTapeArchiveResponse' {} a -> s {tapeARN = a} :: DeleteTapeArchiveResponse)

-- | The response's http status code.
deleteTapeArchiveResponse_httpStatus :: Lens.Lens' DeleteTapeArchiveResponse Prelude.Int
deleteTapeArchiveResponse_httpStatus = Lens.lens (\DeleteTapeArchiveResponse' {httpStatus} -> httpStatus) (\s@DeleteTapeArchiveResponse' {} a -> s {httpStatus = a} :: DeleteTapeArchiveResponse)

instance Prelude.NFData DeleteTapeArchiveResponse
