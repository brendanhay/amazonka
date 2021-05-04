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
-- Module      : Network.AWS.DirectoryService.RejectSharedDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a directory sharing request that was sent from the directory
-- owner account.
module Network.AWS.DirectoryService.RejectSharedDirectory
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectSharedDirectory' smart constructor.
data RejectSharedDirectory = RejectSharedDirectory'
  { -- | Identifier of the shared directory in the directory consumer account.
    -- This identifier is different for each directory owner account.
    sharedDirectoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest RejectSharedDirectory where
  type
    Rs RejectSharedDirectory =
      RejectSharedDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectSharedDirectoryResponse'
            Prelude.<$> (x Prelude..?> "SharedDirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectSharedDirectory

instance Prelude.NFData RejectSharedDirectory

instance Prelude.ToHeaders RejectSharedDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.RejectSharedDirectory" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RejectSharedDirectory where
  toJSON RejectSharedDirectory' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SharedDirectoryId" Prelude..= sharedDirectoryId)
          ]
      )

instance Prelude.ToPath RejectSharedDirectory where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RejectSharedDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectSharedDirectoryResponse' smart constructor.
data RejectSharedDirectoryResponse = RejectSharedDirectoryResponse'
  { -- | Identifier of the shared directory in the directory consumer account.
    sharedDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData RejectSharedDirectoryResponse
