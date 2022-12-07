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
-- Module      : Amazonka.BackupStorage.DeleteObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete Object from the incremental base Backup.
module Amazonka.BackupStorage.DeleteObject
  ( -- * Creating a Request
    DeleteObject (..),
    newDeleteObject,

    -- * Request Lenses
    deleteObject_backupJobId,
    deleteObject_objectName,

    -- * Destructuring the Response
    DeleteObjectResponse (..),
    newDeleteObjectResponse,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { -- | Backup job Id for the in-progress backup.
    backupJobId :: Prelude.Text,
    -- | The name of the Object.
    objectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupJobId', 'deleteObject_backupJobId' - Backup job Id for the in-progress backup.
--
-- 'objectName', 'deleteObject_objectName' - The name of the Object.
newDeleteObject ::
  -- | 'backupJobId'
  Prelude.Text ->
  -- | 'objectName'
  Prelude.Text ->
  DeleteObject
newDeleteObject pBackupJobId_ pObjectName_ =
  DeleteObject'
    { backupJobId = pBackupJobId_,
      objectName = pObjectName_
    }

-- | Backup job Id for the in-progress backup.
deleteObject_backupJobId :: Lens.Lens' DeleteObject Prelude.Text
deleteObject_backupJobId = Lens.lens (\DeleteObject' {backupJobId} -> backupJobId) (\s@DeleteObject' {} a -> s {backupJobId = a} :: DeleteObject)

-- | The name of the Object.
deleteObject_objectName :: Lens.Lens' DeleteObject Prelude.Text
deleteObject_objectName = Lens.lens (\DeleteObject' {objectName} -> objectName) (\s@DeleteObject' {} a -> s {objectName = a} :: DeleteObject)

instance Core.AWSRequest DeleteObject where
  type AWSResponse DeleteObject = DeleteObjectResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteObjectResponse'

instance Prelude.Hashable DeleteObject where
  hashWithSalt _salt DeleteObject' {..} =
    _salt `Prelude.hashWithSalt` backupJobId
      `Prelude.hashWithSalt` objectName

instance Prelude.NFData DeleteObject where
  rnf DeleteObject' {..} =
    Prelude.rnf backupJobId
      `Prelude.seq` Prelude.rnf objectName

instance Data.ToHeaders DeleteObject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteObject where
  toPath DeleteObject' {..} =
    Prelude.mconcat
      [ "/backup-jobs/",
        Data.toBS backupJobId,
        "/object/",
        Data.toBS objectName
      ]

instance Data.ToQuery DeleteObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteObjectResponse ::
  DeleteObjectResponse
newDeleteObjectResponse = DeleteObjectResponse'

instance Prelude.NFData DeleteObjectResponse where
  rnf _ = ()
