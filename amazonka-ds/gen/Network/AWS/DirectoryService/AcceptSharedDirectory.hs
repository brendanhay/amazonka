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
-- Module      : Network.AWS.DirectoryService.AcceptSharedDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a directory sharing request that was sent from the directory
-- owner account.
module Network.AWS.DirectoryService.AcceptSharedDirectory
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptSharedDirectory' smart constructor.
data AcceptSharedDirectory = AcceptSharedDirectory'
  { -- | Identifier of the shared directory in the directory consumer account.
    -- This identifier is different for each directory owner account.
    sharedDirectoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  AcceptSharedDirectory
newAcceptSharedDirectory pSharedDirectoryId_ =
  AcceptSharedDirectory'
    { sharedDirectoryId =
        pSharedDirectoryId_
    }

-- | Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
acceptSharedDirectory_sharedDirectoryId :: Lens.Lens' AcceptSharedDirectory Core.Text
acceptSharedDirectory_sharedDirectoryId = Lens.lens (\AcceptSharedDirectory' {sharedDirectoryId} -> sharedDirectoryId) (\s@AcceptSharedDirectory' {} a -> s {sharedDirectoryId = a} :: AcceptSharedDirectory)

instance Core.AWSRequest AcceptSharedDirectory where
  type
    AWSResponse AcceptSharedDirectory =
      AcceptSharedDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptSharedDirectoryResponse'
            Core.<$> (x Core..?> "SharedDirectory")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcceptSharedDirectory

instance Core.NFData AcceptSharedDirectory

instance Core.ToHeaders AcceptSharedDirectory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.AcceptSharedDirectory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AcceptSharedDirectory where
  toJSON AcceptSharedDirectory' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SharedDirectoryId" Core..= sharedDirectoryId)
          ]
      )

instance Core.ToPath AcceptSharedDirectory where
  toPath = Core.const "/"

instance Core.ToQuery AcceptSharedDirectory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAcceptSharedDirectoryResponse' smart constructor.
data AcceptSharedDirectoryResponse = AcceptSharedDirectoryResponse'
  { -- | The shared directory in the directory consumer account.
    sharedDirectory :: Core.Maybe SharedDirectory,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  AcceptSharedDirectoryResponse
newAcceptSharedDirectoryResponse pHttpStatus_ =
  AcceptSharedDirectoryResponse'
    { sharedDirectory =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The shared directory in the directory consumer account.
acceptSharedDirectoryResponse_sharedDirectory :: Lens.Lens' AcceptSharedDirectoryResponse (Core.Maybe SharedDirectory)
acceptSharedDirectoryResponse_sharedDirectory = Lens.lens (\AcceptSharedDirectoryResponse' {sharedDirectory} -> sharedDirectory) (\s@AcceptSharedDirectoryResponse' {} a -> s {sharedDirectory = a} :: AcceptSharedDirectoryResponse)

-- | The response's http status code.
acceptSharedDirectoryResponse_httpStatus :: Lens.Lens' AcceptSharedDirectoryResponse Core.Int
acceptSharedDirectoryResponse_httpStatus = Lens.lens (\AcceptSharedDirectoryResponse' {httpStatus} -> httpStatus) (\s@AcceptSharedDirectoryResponse' {} a -> s {httpStatus = a} :: AcceptSharedDirectoryResponse)

instance Core.NFData AcceptSharedDirectoryResponse
