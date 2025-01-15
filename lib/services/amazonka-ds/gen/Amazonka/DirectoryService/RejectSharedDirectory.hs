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
-- Module      : Amazonka.DirectoryService.RejectSharedDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a directory sharing request that was sent from the directory
-- owner account.
module Amazonka.DirectoryService.RejectSharedDirectory
  ( -- * Creating a Request
    RejectSharedDirectory (..),
    newRejectSharedDirectory,

    -- * Request Lenses
    rejectSharedDirectory_sharedDirectoryId,

    -- * Destructuring the Response
    RejectSharedDirectoryResponse (..),
    newRejectSharedDirectoryResponse,

    -- * Response Lenses
    rejectSharedDirectoryResponse_sharedDirectoryId,
    rejectSharedDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectSharedDirectory' smart constructor.
data RejectSharedDirectory = RejectSharedDirectory'
  { -- | Identifier of the shared directory in the directory consumer account.
    -- This identifier is different for each directory owner account.
    sharedDirectoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectSharedDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedDirectoryId', 'rejectSharedDirectory_sharedDirectoryId' - Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
newRejectSharedDirectory ::
  -- | 'sharedDirectoryId'
  Prelude.Text ->
  RejectSharedDirectory
newRejectSharedDirectory pSharedDirectoryId_ =
  RejectSharedDirectory'
    { sharedDirectoryId =
        pSharedDirectoryId_
    }

-- | Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
rejectSharedDirectory_sharedDirectoryId :: Lens.Lens' RejectSharedDirectory Prelude.Text
rejectSharedDirectory_sharedDirectoryId = Lens.lens (\RejectSharedDirectory' {sharedDirectoryId} -> sharedDirectoryId) (\s@RejectSharedDirectory' {} a -> s {sharedDirectoryId = a} :: RejectSharedDirectory)

instance Core.AWSRequest RejectSharedDirectory where
  type
    AWSResponse RejectSharedDirectory =
      RejectSharedDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectSharedDirectoryResponse'
            Prelude.<$> (x Data..?> "SharedDirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectSharedDirectory where
  hashWithSalt _salt RejectSharedDirectory' {..} =
    _salt `Prelude.hashWithSalt` sharedDirectoryId

instance Prelude.NFData RejectSharedDirectory where
  rnf RejectSharedDirectory' {..} =
    Prelude.rnf sharedDirectoryId

instance Data.ToHeaders RejectSharedDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.RejectSharedDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RejectSharedDirectory where
  toJSON RejectSharedDirectory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SharedDirectoryId" Data..= sharedDirectoryId)
          ]
      )

instance Data.ToPath RejectSharedDirectory where
  toPath = Prelude.const "/"

instance Data.ToQuery RejectSharedDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectSharedDirectoryResponse' smart constructor.
data RejectSharedDirectoryResponse = RejectSharedDirectoryResponse'
  { -- | Identifier of the shared directory in the directory consumer account.
    sharedDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectSharedDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedDirectoryId', 'rejectSharedDirectoryResponse_sharedDirectoryId' - Identifier of the shared directory in the directory consumer account.
--
-- 'httpStatus', 'rejectSharedDirectoryResponse_httpStatus' - The response's http status code.
newRejectSharedDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectSharedDirectoryResponse
newRejectSharedDirectoryResponse pHttpStatus_ =
  RejectSharedDirectoryResponse'
    { sharedDirectoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier of the shared directory in the directory consumer account.
rejectSharedDirectoryResponse_sharedDirectoryId :: Lens.Lens' RejectSharedDirectoryResponse (Prelude.Maybe Prelude.Text)
rejectSharedDirectoryResponse_sharedDirectoryId = Lens.lens (\RejectSharedDirectoryResponse' {sharedDirectoryId} -> sharedDirectoryId) (\s@RejectSharedDirectoryResponse' {} a -> s {sharedDirectoryId = a} :: RejectSharedDirectoryResponse)

-- | The response's http status code.
rejectSharedDirectoryResponse_httpStatus :: Lens.Lens' RejectSharedDirectoryResponse Prelude.Int
rejectSharedDirectoryResponse_httpStatus = Lens.lens (\RejectSharedDirectoryResponse' {httpStatus} -> httpStatus) (\s@RejectSharedDirectoryResponse' {} a -> s {httpStatus = a} :: RejectSharedDirectoryResponse)

instance Prelude.NFData RejectSharedDirectoryResponse where
  rnf RejectSharedDirectoryResponse' {..} =
    Prelude.rnf sharedDirectoryId `Prelude.seq`
      Prelude.rnf httpStatus
