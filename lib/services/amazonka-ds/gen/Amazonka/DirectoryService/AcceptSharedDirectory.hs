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
-- Module      : Amazonka.DirectoryService.AcceptSharedDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a directory sharing request that was sent from the directory
-- owner account.
module Amazonka.DirectoryService.AcceptSharedDirectory
  ( -- * Creating a Request
    AcceptSharedDirectory (..),
    newAcceptSharedDirectory,

    -- * Request Lenses
    acceptSharedDirectory_sharedDirectoryId,

    -- * Destructuring the Response
    AcceptSharedDirectoryResponse (..),
    newAcceptSharedDirectoryResponse,

    -- * Response Lenses
    acceptSharedDirectoryResponse_sharedDirectory,
    acceptSharedDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptSharedDirectory' smart constructor.
data AcceptSharedDirectory = AcceptSharedDirectory'
  { -- | Identifier of the shared directory in the directory consumer account.
    -- This identifier is different for each directory owner account.
    sharedDirectoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptSharedDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedDirectoryId', 'acceptSharedDirectory_sharedDirectoryId' - Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
newAcceptSharedDirectory ::
  -- | 'sharedDirectoryId'
  Prelude.Text ->
  AcceptSharedDirectory
newAcceptSharedDirectory pSharedDirectoryId_ =
  AcceptSharedDirectory'
    { sharedDirectoryId =
        pSharedDirectoryId_
    }

-- | Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
acceptSharedDirectory_sharedDirectoryId :: Lens.Lens' AcceptSharedDirectory Prelude.Text
acceptSharedDirectory_sharedDirectoryId = Lens.lens (\AcceptSharedDirectory' {sharedDirectoryId} -> sharedDirectoryId) (\s@AcceptSharedDirectory' {} a -> s {sharedDirectoryId = a} :: AcceptSharedDirectory)

instance Core.AWSRequest AcceptSharedDirectory where
  type
    AWSResponse AcceptSharedDirectory =
      AcceptSharedDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptSharedDirectoryResponse'
            Prelude.<$> (x Data..?> "SharedDirectory")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptSharedDirectory where
  hashWithSalt _salt AcceptSharedDirectory' {..} =
    _salt `Prelude.hashWithSalt` sharedDirectoryId

instance Prelude.NFData AcceptSharedDirectory where
  rnf AcceptSharedDirectory' {..} =
    Prelude.rnf sharedDirectoryId

instance Data.ToHeaders AcceptSharedDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.AcceptSharedDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AcceptSharedDirectory where
  toJSON AcceptSharedDirectory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SharedDirectoryId" Data..= sharedDirectoryId)
          ]
      )

instance Data.ToPath AcceptSharedDirectory where
  toPath = Prelude.const "/"

instance Data.ToQuery AcceptSharedDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptSharedDirectoryResponse' smart constructor.
data AcceptSharedDirectoryResponse = AcceptSharedDirectoryResponse'
  { -- | The shared directory in the directory consumer account.
    sharedDirectory :: Prelude.Maybe SharedDirectory,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptSharedDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedDirectory', 'acceptSharedDirectoryResponse_sharedDirectory' - The shared directory in the directory consumer account.
--
-- 'httpStatus', 'acceptSharedDirectoryResponse_httpStatus' - The response's http status code.
newAcceptSharedDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptSharedDirectoryResponse
newAcceptSharedDirectoryResponse pHttpStatus_ =
  AcceptSharedDirectoryResponse'
    { sharedDirectory =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The shared directory in the directory consumer account.
acceptSharedDirectoryResponse_sharedDirectory :: Lens.Lens' AcceptSharedDirectoryResponse (Prelude.Maybe SharedDirectory)
acceptSharedDirectoryResponse_sharedDirectory = Lens.lens (\AcceptSharedDirectoryResponse' {sharedDirectory} -> sharedDirectory) (\s@AcceptSharedDirectoryResponse' {} a -> s {sharedDirectory = a} :: AcceptSharedDirectoryResponse)

-- | The response's http status code.
acceptSharedDirectoryResponse_httpStatus :: Lens.Lens' AcceptSharedDirectoryResponse Prelude.Int
acceptSharedDirectoryResponse_httpStatus = Lens.lens (\AcceptSharedDirectoryResponse' {httpStatus} -> httpStatus) (\s@AcceptSharedDirectoryResponse' {} a -> s {httpStatus = a} :: AcceptSharedDirectoryResponse)

instance Prelude.NFData AcceptSharedDirectoryResponse where
  rnf AcceptSharedDirectoryResponse' {..} =
    Prelude.rnf sharedDirectory `Prelude.seq`
      Prelude.rnf httpStatus
