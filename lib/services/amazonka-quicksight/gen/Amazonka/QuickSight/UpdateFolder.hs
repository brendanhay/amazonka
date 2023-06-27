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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    updateFolderResponse_arn,
    updateFolderResponse_folderId,
    updateFolderResponse_requestId,
    updateFolderResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFolder' smart constructor.
data UpdateFolder = UpdateFolder'
  { -- | The ID for the Amazon Web Services account that contains the folder to
    -- update.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
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
-- 'awsAccountId', 'updateFolder_awsAccountId' - The ID for the Amazon Web Services account that contains the folder to
-- update.
--
-- 'folderId', 'updateFolder_folderId' - The ID of the folder.
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

-- | The ID for the Amazon Web Services account that contains the folder to
-- update.
updateFolder_awsAccountId :: Lens.Lens' UpdateFolder Prelude.Text
updateFolder_awsAccountId = Lens.lens (\UpdateFolder' {awsAccountId} -> awsAccountId) (\s@UpdateFolder' {} a -> s {awsAccountId = a} :: UpdateFolder)

-- | The ID of the folder.
updateFolder_folderId :: Lens.Lens' UpdateFolder Prelude.Text
updateFolder_folderId = Lens.lens (\UpdateFolder' {folderId} -> folderId) (\s@UpdateFolder' {} a -> s {folderId = a} :: UpdateFolder)

-- | The name of the folder.
updateFolder_name :: Lens.Lens' UpdateFolder Prelude.Text
updateFolder_name = Lens.lens (\UpdateFolder' {name} -> name) (\s@UpdateFolder' {} a -> s {name = a} :: UpdateFolder)

instance Core.AWSRequest UpdateFolder where
  type AWSResponse UpdateFolder = UpdateFolderResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFolderResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "FolderId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFolder where
  hashWithSalt _salt UpdateFolder' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateFolder where
  rnf UpdateFolder' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFolder where
  toJSON UpdateFolder' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath UpdateFolder where
  toPath UpdateFolder' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/folders/",
        Data.toBS folderId
      ]

instance Data.ToQuery UpdateFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFolderResponse' smart constructor.
data UpdateFolderResponse = UpdateFolderResponse'
  { -- | The Amazon Resource Name (ARN) of the folder.
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
-- Create a value of 'UpdateFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateFolderResponse_arn' - The Amazon Resource Name (ARN) of the folder.
--
-- 'folderId', 'updateFolderResponse_folderId' - The ID of the folder.
--
-- 'requestId', 'updateFolderResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'updateFolderResponse_status' - The HTTP status of the request.
newUpdateFolderResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateFolderResponse
newUpdateFolderResponse pStatus_ =
  UpdateFolderResponse'
    { arn = Prelude.Nothing,
      folderId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the folder.
updateFolderResponse_arn :: Lens.Lens' UpdateFolderResponse (Prelude.Maybe Prelude.Text)
updateFolderResponse_arn = Lens.lens (\UpdateFolderResponse' {arn} -> arn) (\s@UpdateFolderResponse' {} a -> s {arn = a} :: UpdateFolderResponse)

-- | The ID of the folder.
updateFolderResponse_folderId :: Lens.Lens' UpdateFolderResponse (Prelude.Maybe Prelude.Text)
updateFolderResponse_folderId = Lens.lens (\UpdateFolderResponse' {folderId} -> folderId) (\s@UpdateFolderResponse' {} a -> s {folderId = a} :: UpdateFolderResponse)

-- | The Amazon Web Services request ID for this operation.
updateFolderResponse_requestId :: Lens.Lens' UpdateFolderResponse (Prelude.Maybe Prelude.Text)
updateFolderResponse_requestId = Lens.lens (\UpdateFolderResponse' {requestId} -> requestId) (\s@UpdateFolderResponse' {} a -> s {requestId = a} :: UpdateFolderResponse)

-- | The HTTP status of the request.
updateFolderResponse_status :: Lens.Lens' UpdateFolderResponse Prelude.Int
updateFolderResponse_status = Lens.lens (\UpdateFolderResponse' {status} -> status) (\s@UpdateFolderResponse' {} a -> s {status = a} :: UpdateFolderResponse)

instance Prelude.NFData UpdateFolderResponse where
  rnf UpdateFolderResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
