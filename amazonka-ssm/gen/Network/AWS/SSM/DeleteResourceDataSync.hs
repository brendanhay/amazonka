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
-- Module      : Network.AWS.SSM.DeleteResourceDataSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Resource Data Sync configuration. After the configuration is
-- deleted, changes to data on managed instances are no longer synced to or
-- from the target. Deleting a sync configuration does not delete data.
module Network.AWS.SSM.DeleteResourceDataSync
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeleteResourceDataSync' smart constructor.
data DeleteResourceDataSync = DeleteResourceDataSync'
  { -- | Specify the type of resource data sync to delete.
    syncType :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration to delete.
    syncName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteResourceDataSync where
  type
    Rs DeleteResourceDataSync =
      DeleteResourceDataSyncResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourceDataSyncResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourceDataSync

instance Prelude.NFData DeleteResourceDataSync

instance Prelude.ToHeaders DeleteResourceDataSync where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DeleteResourceDataSync" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteResourceDataSync where
  toJSON DeleteResourceDataSync' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SyncType" Prelude..=) Prelude.<$> syncType,
            Prelude.Just ("SyncName" Prelude..= syncName)
          ]
      )

instance Prelude.ToPath DeleteResourceDataSync where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteResourceDataSync where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceDataSyncResponse' smart constructor.
data DeleteResourceDataSyncResponse = DeleteResourceDataSyncResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
