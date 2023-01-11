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
-- Module      : Amazonka.BackupStorage.StartObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start upload containing one or many chunks.
module Amazonka.BackupStorage.StartObject
  ( -- * Creating a Request
    StartObject (..),
    newStartObject,

    -- * Request Lenses
    startObject_throwOnDuplicate,
    startObject_backupJobId,
    startObject_objectName,

    -- * Destructuring the Response
    StartObjectResponse (..),
    newStartObjectResponse,

    -- * Response Lenses
    startObjectResponse_httpStatus,
    startObjectResponse_uploadId,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartObject' smart constructor.
data StartObject = StartObject'
  { -- | Throw an exception if Object name is already exist.
    throwOnDuplicate :: Prelude.Maybe Prelude.Bool,
    -- | Backup job Id for the in-progress backup
    backupJobId :: Prelude.Text,
    -- | Name for the object.
    objectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'throwOnDuplicate', 'startObject_throwOnDuplicate' - Throw an exception if Object name is already exist.
--
-- 'backupJobId', 'startObject_backupJobId' - Backup job Id for the in-progress backup
--
-- 'objectName', 'startObject_objectName' - Name for the object.
newStartObject ::
  -- | 'backupJobId'
  Prelude.Text ->
  -- | 'objectName'
  Prelude.Text ->
  StartObject
newStartObject pBackupJobId_ pObjectName_ =
  StartObject'
    { throwOnDuplicate = Prelude.Nothing,
      backupJobId = pBackupJobId_,
      objectName = pObjectName_
    }

-- | Throw an exception if Object name is already exist.
startObject_throwOnDuplicate :: Lens.Lens' StartObject (Prelude.Maybe Prelude.Bool)
startObject_throwOnDuplicate = Lens.lens (\StartObject' {throwOnDuplicate} -> throwOnDuplicate) (\s@StartObject' {} a -> s {throwOnDuplicate = a} :: StartObject)

-- | Backup job Id for the in-progress backup
startObject_backupJobId :: Lens.Lens' StartObject Prelude.Text
startObject_backupJobId = Lens.lens (\StartObject' {backupJobId} -> backupJobId) (\s@StartObject' {} a -> s {backupJobId = a} :: StartObject)

-- | Name for the object.
startObject_objectName :: Lens.Lens' StartObject Prelude.Text
startObject_objectName = Lens.lens (\StartObject' {objectName} -> objectName) (\s@StartObject' {} a -> s {objectName = a} :: StartObject)

instance Core.AWSRequest StartObject where
  type AWSResponse StartObject = StartObjectResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartObjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UploadId")
      )

instance Prelude.Hashable StartObject where
  hashWithSalt _salt StartObject' {..} =
    _salt `Prelude.hashWithSalt` throwOnDuplicate
      `Prelude.hashWithSalt` backupJobId
      `Prelude.hashWithSalt` objectName

instance Prelude.NFData StartObject where
  rnf StartObject' {..} =
    Prelude.rnf throwOnDuplicate
      `Prelude.seq` Prelude.rnf backupJobId
      `Prelude.seq` Prelude.rnf objectName

instance Data.ToHeaders StartObject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartObject where
  toJSON StartObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ThrowOnDuplicate" Data..=)
              Prelude.<$> throwOnDuplicate
          ]
      )

instance Data.ToPath StartObject where
  toPath StartObject' {..} =
    Prelude.mconcat
      [ "/backup-jobs/",
        Data.toBS backupJobId,
        "/object/",
        Data.toBS objectName
      ]

instance Data.ToQuery StartObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartObjectResponse' smart constructor.
data StartObjectResponse = StartObjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Upload Id for a given upload.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startObjectResponse_httpStatus' - The response's http status code.
--
-- 'uploadId', 'startObjectResponse_uploadId' - Upload Id for a given upload.
newStartObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'uploadId'
  Prelude.Text ->
  StartObjectResponse
newStartObjectResponse pHttpStatus_ pUploadId_ =
  StartObjectResponse'
    { httpStatus = pHttpStatus_,
      uploadId = pUploadId_
    }

-- | The response's http status code.
startObjectResponse_httpStatus :: Lens.Lens' StartObjectResponse Prelude.Int
startObjectResponse_httpStatus = Lens.lens (\StartObjectResponse' {httpStatus} -> httpStatus) (\s@StartObjectResponse' {} a -> s {httpStatus = a} :: StartObjectResponse)

-- | Upload Id for a given upload.
startObjectResponse_uploadId :: Lens.Lens' StartObjectResponse Prelude.Text
startObjectResponse_uploadId = Lens.lens (\StartObjectResponse' {uploadId} -> uploadId) (\s@StartObjectResponse' {} a -> s {uploadId = a} :: StartObjectResponse)

instance Prelude.NFData StartObjectResponse where
  rnf StartObjectResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf uploadId
