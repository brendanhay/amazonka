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
-- Module      : Amazonka.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape from the virtual tape shelf (VTS).
-- This operation is only supported in the tape gateway type.
module Amazonka.StorageGateway.DeleteTapeArchive
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteTapeArchive where
  type
    AWSResponse DeleteTapeArchive =
      DeleteTapeArchiveResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapeArchiveResponse'
            Prelude.<$> (x Data..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTapeArchive where
  hashWithSalt _salt DeleteTapeArchive' {..} =
    _salt
      `Prelude.hashWithSalt` bypassGovernanceRetention
      `Prelude.hashWithSalt` tapeARN

instance Prelude.NFData DeleteTapeArchive where
  rnf DeleteTapeArchive' {..} =
    Prelude.rnf bypassGovernanceRetention
      `Prelude.seq` Prelude.rnf tapeARN

instance Data.ToHeaders DeleteTapeArchive where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DeleteTapeArchive" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTapeArchive where
  toJSON DeleteTapeArchive' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BypassGovernanceRetention" Data..=)
              Prelude.<$> bypassGovernanceRetention,
            Prelude.Just ("TapeARN" Data..= tapeARN)
          ]
      )

instance Data.ToPath DeleteTapeArchive where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTapeArchive where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteTapeArchiveResponse where
  rnf DeleteTapeArchiveResponse' {..} =
    Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf httpStatus
