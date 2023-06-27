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
-- Module      : Amazonka.DirectoryService.UnshareDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the directory sharing between the directory owner and consumer
-- accounts.
module Amazonka.DirectoryService.UnshareDirectory
  ( -- * Creating a Request
    UnshareDirectory (..),
    newUnshareDirectory,

    -- * Request Lenses
    unshareDirectory_directoryId,
    unshareDirectory_unshareTarget,

    -- * Destructuring the Response
    UnshareDirectoryResponse (..),
    newUnshareDirectoryResponse,

    -- * Response Lenses
    unshareDirectoryResponse_sharedDirectoryId,
    unshareDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnshareDirectory' smart constructor.
data UnshareDirectory = UnshareDirectory'
  { -- | The identifier of the Managed Microsoft AD directory that you want to
    -- stop sharing.
    directoryId :: Prelude.Text,
    -- | Identifier for the directory consumer account with whom the directory
    -- has to be unshared.
    unshareTarget :: UnshareTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnshareDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'unshareDirectory_directoryId' - The identifier of the Managed Microsoft AD directory that you want to
-- stop sharing.
--
-- 'unshareTarget', 'unshareDirectory_unshareTarget' - Identifier for the directory consumer account with whom the directory
-- has to be unshared.
newUnshareDirectory ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'unshareTarget'
  UnshareTarget ->
  UnshareDirectory
newUnshareDirectory pDirectoryId_ pUnshareTarget_ =
  UnshareDirectory'
    { directoryId = pDirectoryId_,
      unshareTarget = pUnshareTarget_
    }

-- | The identifier of the Managed Microsoft AD directory that you want to
-- stop sharing.
unshareDirectory_directoryId :: Lens.Lens' UnshareDirectory Prelude.Text
unshareDirectory_directoryId = Lens.lens (\UnshareDirectory' {directoryId} -> directoryId) (\s@UnshareDirectory' {} a -> s {directoryId = a} :: UnshareDirectory)

-- | Identifier for the directory consumer account with whom the directory
-- has to be unshared.
unshareDirectory_unshareTarget :: Lens.Lens' UnshareDirectory UnshareTarget
unshareDirectory_unshareTarget = Lens.lens (\UnshareDirectory' {unshareTarget} -> unshareTarget) (\s@UnshareDirectory' {} a -> s {unshareTarget = a} :: UnshareDirectory)

instance Core.AWSRequest UnshareDirectory where
  type
    AWSResponse UnshareDirectory =
      UnshareDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UnshareDirectoryResponse'
            Prelude.<$> (x Data..?> "SharedDirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnshareDirectory where
  hashWithSalt _salt UnshareDirectory' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` unshareTarget

instance Prelude.NFData UnshareDirectory where
  rnf UnshareDirectory' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf unshareTarget

instance Data.ToHeaders UnshareDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.UnshareDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnshareDirectory where
  toJSON UnshareDirectory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just
              ("UnshareTarget" Data..= unshareTarget)
          ]
      )

instance Data.ToPath UnshareDirectory where
  toPath = Prelude.const "/"

instance Data.ToQuery UnshareDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnshareDirectoryResponse' smart constructor.
data UnshareDirectoryResponse = UnshareDirectoryResponse'
  { -- | Identifier of the directory stored in the directory consumer account
    -- that is to be unshared from the specified directory (@DirectoryId@).
    sharedDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnshareDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedDirectoryId', 'unshareDirectoryResponse_sharedDirectoryId' - Identifier of the directory stored in the directory consumer account
-- that is to be unshared from the specified directory (@DirectoryId@).
--
-- 'httpStatus', 'unshareDirectoryResponse_httpStatus' - The response's http status code.
newUnshareDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnshareDirectoryResponse
newUnshareDirectoryResponse pHttpStatus_ =
  UnshareDirectoryResponse'
    { sharedDirectoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier of the directory stored in the directory consumer account
-- that is to be unshared from the specified directory (@DirectoryId@).
unshareDirectoryResponse_sharedDirectoryId :: Lens.Lens' UnshareDirectoryResponse (Prelude.Maybe Prelude.Text)
unshareDirectoryResponse_sharedDirectoryId = Lens.lens (\UnshareDirectoryResponse' {sharedDirectoryId} -> sharedDirectoryId) (\s@UnshareDirectoryResponse' {} a -> s {sharedDirectoryId = a} :: UnshareDirectoryResponse)

-- | The response's http status code.
unshareDirectoryResponse_httpStatus :: Lens.Lens' UnshareDirectoryResponse Prelude.Int
unshareDirectoryResponse_httpStatus = Lens.lens (\UnshareDirectoryResponse' {httpStatus} -> httpStatus) (\s@UnshareDirectoryResponse' {} a -> s {httpStatus = a} :: UnshareDirectoryResponse)

instance Prelude.NFData UnshareDirectoryResponse where
  rnf UnshareDirectoryResponse' {..} =
    Prelude.rnf sharedDirectoryId
      `Prelude.seq` Prelude.rnf httpStatus
