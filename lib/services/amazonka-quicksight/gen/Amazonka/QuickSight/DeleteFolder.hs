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
-- Module      : Amazonka.QuickSight.DeleteFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an empty folder.
module Amazonka.QuickSight.DeleteFolder
  ( -- * Creating a Request
    DeleteFolder (..),
    newDeleteFolder,

    -- * Request Lenses
    deleteFolder_awsAccountId,
    deleteFolder_folderId,

    -- * Destructuring the Response
    DeleteFolderResponse (..),
    newDeleteFolderResponse,

    -- * Response Lenses
    deleteFolderResponse_arn,
    deleteFolderResponse_folderId,
    deleteFolderResponse_requestId,
    deleteFolderResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFolder' smart constructor.
data DeleteFolder = DeleteFolder'
  { -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteFolder_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
--
-- 'folderId', 'deleteFolder_folderId' - The ID of the folder.
newDeleteFolder ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  DeleteFolder
newDeleteFolder pAwsAccountId_ pFolderId_ =
  DeleteFolder'
    { awsAccountId = pAwsAccountId_,
      folderId = pFolderId_
    }

-- | The ID for the Amazon Web Services account that contains the folder.
deleteFolder_awsAccountId :: Lens.Lens' DeleteFolder Prelude.Text
deleteFolder_awsAccountId = Lens.lens (\DeleteFolder' {awsAccountId} -> awsAccountId) (\s@DeleteFolder' {} a -> s {awsAccountId = a} :: DeleteFolder)

-- | The ID of the folder.
deleteFolder_folderId :: Lens.Lens' DeleteFolder Prelude.Text
deleteFolder_folderId = Lens.lens (\DeleteFolder' {folderId} -> folderId) (\s@DeleteFolder' {} a -> s {folderId = a} :: DeleteFolder)

instance Core.AWSRequest DeleteFolder where
  type AWSResponse DeleteFolder = DeleteFolderResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFolderResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "FolderId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFolder where
  hashWithSalt _salt DeleteFolder' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData DeleteFolder where
  rnf DeleteFolder' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance Data.ToHeaders DeleteFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteFolder where
  toPath DeleteFolder' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/folders/",
        Data.toBS folderId
      ]

instance Data.ToQuery DeleteFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFolderResponse' smart constructor.
data DeleteFolderResponse = DeleteFolderResponse'
  { -- | The Amazon Resource Name of the deleted folder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteFolderResponse_arn' - The Amazon Resource Name of the deleted folder.
--
-- 'folderId', 'deleteFolderResponse_folderId' - The ID of the folder.
--
-- 'requestId', 'deleteFolderResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteFolderResponse_status' - The HTTP status of the request.
newDeleteFolderResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteFolderResponse
newDeleteFolderResponse pStatus_ =
  DeleteFolderResponse'
    { arn = Prelude.Nothing,
      folderId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name of the deleted folder.
deleteFolderResponse_arn :: Lens.Lens' DeleteFolderResponse (Prelude.Maybe Prelude.Text)
deleteFolderResponse_arn = Lens.lens (\DeleteFolderResponse' {arn} -> arn) (\s@DeleteFolderResponse' {} a -> s {arn = a} :: DeleteFolderResponse)

-- | The ID of the folder.
deleteFolderResponse_folderId :: Lens.Lens' DeleteFolderResponse (Prelude.Maybe Prelude.Text)
deleteFolderResponse_folderId = Lens.lens (\DeleteFolderResponse' {folderId} -> folderId) (\s@DeleteFolderResponse' {} a -> s {folderId = a} :: DeleteFolderResponse)

-- | The Amazon Web Services request ID for this operation.
deleteFolderResponse_requestId :: Lens.Lens' DeleteFolderResponse (Prelude.Maybe Prelude.Text)
deleteFolderResponse_requestId = Lens.lens (\DeleteFolderResponse' {requestId} -> requestId) (\s@DeleteFolderResponse' {} a -> s {requestId = a} :: DeleteFolderResponse)

-- | The HTTP status of the request.
deleteFolderResponse_status :: Lens.Lens' DeleteFolderResponse Prelude.Int
deleteFolderResponse_status = Lens.lens (\DeleteFolderResponse' {status} -> status) (\s@DeleteFolderResponse' {} a -> s {status = a} :: DeleteFolderResponse)

instance Prelude.NFData DeleteFolderResponse where
  rnf DeleteFolderResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
