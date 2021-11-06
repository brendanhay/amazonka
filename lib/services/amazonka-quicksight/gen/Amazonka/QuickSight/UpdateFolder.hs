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
-- Module      : Amazonka.QuickSight.UpdateFolder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a folder.
module Amazonka.QuickSight.UpdateFolder
  ( -- * Creating a Request
    UpdateFolder (..),
    newUpdateFolder,

    -- * Request Lenses
    updateFolder_awsAccountId,
    updateFolder_folderId,
    updateFolder_name,

    -- * Destructuring the Response
    UpdateFolderResponse (..),
    newUpdateFolderResponse,

    -- * Response Lenses
    updateFolderResponse_requestId,
    updateFolderResponse_arn,
    updateFolderResponse_folderId,
    updateFolderResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFolder' smart constructor.
data UpdateFolder = UpdateFolder'
  { -- | The AWS account ID.
    awsAccountId :: Prelude.Text,
    -- | The folder ID.
    folderId :: Prelude.Text,
    -- | The name of the folder.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'updateFolder_awsAccountId' - The AWS account ID.
--
-- 'folderId', 'updateFolder_folderId' - The folder ID.
--
-- 'name', 'updateFolder_name' - The name of the folder.
newUpdateFolder ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateFolder
newUpdateFolder pAwsAccountId_ pFolderId_ pName_ =
  UpdateFolder'
    { awsAccountId = pAwsAccountId_,
      folderId = pFolderId_,
      name = pName_
    }

-- | The AWS account ID.
updateFolder_awsAccountId :: Lens.Lens' UpdateFolder Prelude.Text
updateFolder_awsAccountId = Lens.lens (\UpdateFolder' {awsAccountId} -> awsAccountId) (\s@UpdateFolder' {} a -> s {awsAccountId = a} :: UpdateFolder)

-- | The folder ID.
updateFolder_folderId :: Lens.Lens' UpdateFolder Prelude.Text
updateFolder_folderId = Lens.lens (\UpdateFolder' {folderId} -> folderId) (\s@UpdateFolder' {} a -> s {folderId = a} :: UpdateFolder)

-- | The name of the folder.
updateFolder_name :: Lens.Lens' UpdateFolder Prelude.Text
updateFolder_name = Lens.lens (\UpdateFolder' {name} -> name) (\s@UpdateFolder' {} a -> s {name = a} :: UpdateFolder)

instance Core.AWSRequest UpdateFolder where
  type AWSResponse UpdateFolder = UpdateFolderResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFolderResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "FolderId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFolder

instance Prelude.NFData UpdateFolder

instance Core.ToHeaders UpdateFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFolder where
  toJSON UpdateFolder' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath UpdateFolder where
  toPath UpdateFolder' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/folders/",
        Core.toBS folderId
      ]

instance Core.ToQuery UpdateFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFolderResponse' smart constructor.
data UpdateFolderResponse = UpdateFolderResponse'
  { -- | The request ID.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The folder ID.
    folderId :: Prelude.Maybe Prelude.Text,
    -- | The status. If succeeded, the status is @SC_OK@.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateFolderResponse_requestId' - The request ID.
--
-- 'arn', 'updateFolderResponse_arn' - The Amazon Resource Name (ARN).
--
-- 'folderId', 'updateFolderResponse_folderId' - The folder ID.
--
-- 'status', 'updateFolderResponse_status' - The status. If succeeded, the status is @SC_OK@.
newUpdateFolderResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateFolderResponse
newUpdateFolderResponse pStatus_ =
  UpdateFolderResponse'
    { requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      folderId = Prelude.Nothing,
      status = pStatus_
    }

-- | The request ID.
updateFolderResponse_requestId :: Lens.Lens' UpdateFolderResponse (Prelude.Maybe Prelude.Text)
updateFolderResponse_requestId = Lens.lens (\UpdateFolderResponse' {requestId} -> requestId) (\s@UpdateFolderResponse' {} a -> s {requestId = a} :: UpdateFolderResponse)

-- | The Amazon Resource Name (ARN).
updateFolderResponse_arn :: Lens.Lens' UpdateFolderResponse (Prelude.Maybe Prelude.Text)
updateFolderResponse_arn = Lens.lens (\UpdateFolderResponse' {arn} -> arn) (\s@UpdateFolderResponse' {} a -> s {arn = a} :: UpdateFolderResponse)

-- | The folder ID.
updateFolderResponse_folderId :: Lens.Lens' UpdateFolderResponse (Prelude.Maybe Prelude.Text)
updateFolderResponse_folderId = Lens.lens (\UpdateFolderResponse' {folderId} -> folderId) (\s@UpdateFolderResponse' {} a -> s {folderId = a} :: UpdateFolderResponse)

-- | The status. If succeeded, the status is @SC_OK@.
updateFolderResponse_status :: Lens.Lens' UpdateFolderResponse Prelude.Int
updateFolderResponse_status = Lens.lens (\UpdateFolderResponse' {status} -> status) (\s@UpdateFolderResponse' {} a -> s {status = a} :: UpdateFolderResponse)

instance Prelude.NFData UpdateFolderResponse
