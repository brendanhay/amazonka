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
-- Module      : Amazonka.SSM.DeleteResourceDataSync
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource data sync configuration. After the configuration is
-- deleted, changes to data on managed nodes are no longer synced to or
-- from the target. Deleting a sync configuration doesn\'t delete data.
module Amazonka.SSM.DeleteResourceDataSync
  ( -- * Creating a Request
    DeleteResourceDataSync (..),
    newDeleteResourceDataSync,

    -- * Request Lenses
    deleteResourceDataSync_syncType,
    deleteResourceDataSync_syncName,

    -- * Destructuring the Response
    DeleteResourceDataSyncResponse (..),
    newDeleteResourceDataSyncResponse,

    -- * Response Lenses
    deleteResourceDataSyncResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteResourceDataSync' smart constructor.
data DeleteResourceDataSync = DeleteResourceDataSync'
  { -- | Specify the type of resource data sync to delete.
    syncType :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration to delete.
    syncName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceDataSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncType', 'deleteResourceDataSync_syncType' - Specify the type of resource data sync to delete.
--
-- 'syncName', 'deleteResourceDataSync_syncName' - The name of the configuration to delete.
newDeleteResourceDataSync ::
  -- | 'syncName'
  Prelude.Text ->
  DeleteResourceDataSync
newDeleteResourceDataSync pSyncName_ =
  DeleteResourceDataSync'
    { syncType = Prelude.Nothing,
      syncName = pSyncName_
    }

-- | Specify the type of resource data sync to delete.
deleteResourceDataSync_syncType :: Lens.Lens' DeleteResourceDataSync (Prelude.Maybe Prelude.Text)
deleteResourceDataSync_syncType = Lens.lens (\DeleteResourceDataSync' {syncType} -> syncType) (\s@DeleteResourceDataSync' {} a -> s {syncType = a} :: DeleteResourceDataSync)

-- | The name of the configuration to delete.
deleteResourceDataSync_syncName :: Lens.Lens' DeleteResourceDataSync Prelude.Text
deleteResourceDataSync_syncName = Lens.lens (\DeleteResourceDataSync' {syncName} -> syncName) (\s@DeleteResourceDataSync' {} a -> s {syncName = a} :: DeleteResourceDataSync)

instance Core.AWSRequest DeleteResourceDataSync where
  type
    AWSResponse DeleteResourceDataSync =
      DeleteResourceDataSyncResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourceDataSyncResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourceDataSync where
  hashWithSalt _salt DeleteResourceDataSync' {..} =
    _salt
      `Prelude.hashWithSalt` syncType
      `Prelude.hashWithSalt` syncName

instance Prelude.NFData DeleteResourceDataSync where
  rnf DeleteResourceDataSync' {..} =
    Prelude.rnf syncType
      `Prelude.seq` Prelude.rnf syncName

instance Data.ToHeaders DeleteResourceDataSync where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DeleteResourceDataSync" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourceDataSync where
  toJSON DeleteResourceDataSync' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SyncType" Data..=) Prelude.<$> syncType,
            Prelude.Just ("SyncName" Data..= syncName)
          ]
      )

instance Data.ToPath DeleteResourceDataSync where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResourceDataSync where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceDataSyncResponse' smart constructor.
data DeleteResourceDataSyncResponse = DeleteResourceDataSyncResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceDataSyncResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourceDataSyncResponse_httpStatus' - The response's http status code.
newDeleteResourceDataSyncResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourceDataSyncResponse
newDeleteResourceDataSyncResponse pHttpStatus_ =
  DeleteResourceDataSyncResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourceDataSyncResponse_httpStatus :: Lens.Lens' DeleteResourceDataSyncResponse Prelude.Int
deleteResourceDataSyncResponse_httpStatus = Lens.lens (\DeleteResourceDataSyncResponse' {httpStatus} -> httpStatus) (\s@DeleteResourceDataSyncResponse' {} a -> s {httpStatus = a} :: DeleteResourceDataSyncResponse)

instance
  Prelude.NFData
    DeleteResourceDataSyncResponse
  where
  rnf DeleteResourceDataSyncResponse' {..} =
    Prelude.rnf httpStatus
