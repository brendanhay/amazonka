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
-- Module      : Amazonka.SSM.UpdateResourceDataSync
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a resource data sync. After you create a resource data sync for a
-- Region, you can\'t change the account options for that sync. For
-- example, if you create a sync in the us-east-2 (Ohio) Region and you
-- choose the @Include only the current account@ option, you can\'t edit
-- that sync later and choose the
-- @Include all accounts from my Organizations configuration@ option.
-- Instead, you must delete the first resource data sync, and create a new
-- one.
--
-- This API operation only supports a resource data sync that was created
-- with a SyncFromSource @SyncType@.
module Amazonka.SSM.UpdateResourceDataSync
  ( -- * Creating a Request
    UpdateResourceDataSync (..),
    newUpdateResourceDataSync,

    -- * Request Lenses
    updateResourceDataSync_syncName,
    updateResourceDataSync_syncType,
    updateResourceDataSync_syncSource,

    -- * Destructuring the Response
    UpdateResourceDataSyncResponse (..),
    newUpdateResourceDataSyncResponse,

    -- * Response Lenses
    updateResourceDataSyncResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateResourceDataSync' smart constructor.
data UpdateResourceDataSync = UpdateResourceDataSync'
  { -- | The name of the resource data sync you want to update.
    syncName :: Prelude.Text,
    -- | The type of resource data sync. The supported @SyncType@ is
    -- SyncFromSource.
    syncType :: Prelude.Text,
    -- | Specify information about the data sources to synchronize.
    syncSource :: ResourceDataSyncSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceDataSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncName', 'updateResourceDataSync_syncName' - The name of the resource data sync you want to update.
--
-- 'syncType', 'updateResourceDataSync_syncType' - The type of resource data sync. The supported @SyncType@ is
-- SyncFromSource.
--
-- 'syncSource', 'updateResourceDataSync_syncSource' - Specify information about the data sources to synchronize.
newUpdateResourceDataSync ::
  -- | 'syncName'
  Prelude.Text ->
  -- | 'syncType'
  Prelude.Text ->
  -- | 'syncSource'
  ResourceDataSyncSource ->
  UpdateResourceDataSync
newUpdateResourceDataSync
  pSyncName_
  pSyncType_
  pSyncSource_ =
    UpdateResourceDataSync'
      { syncName = pSyncName_,
        syncType = pSyncType_,
        syncSource = pSyncSource_
      }

-- | The name of the resource data sync you want to update.
updateResourceDataSync_syncName :: Lens.Lens' UpdateResourceDataSync Prelude.Text
updateResourceDataSync_syncName = Lens.lens (\UpdateResourceDataSync' {syncName} -> syncName) (\s@UpdateResourceDataSync' {} a -> s {syncName = a} :: UpdateResourceDataSync)

-- | The type of resource data sync. The supported @SyncType@ is
-- SyncFromSource.
updateResourceDataSync_syncType :: Lens.Lens' UpdateResourceDataSync Prelude.Text
updateResourceDataSync_syncType = Lens.lens (\UpdateResourceDataSync' {syncType} -> syncType) (\s@UpdateResourceDataSync' {} a -> s {syncType = a} :: UpdateResourceDataSync)

-- | Specify information about the data sources to synchronize.
updateResourceDataSync_syncSource :: Lens.Lens' UpdateResourceDataSync ResourceDataSyncSource
updateResourceDataSync_syncSource = Lens.lens (\UpdateResourceDataSync' {syncSource} -> syncSource) (\s@UpdateResourceDataSync' {} a -> s {syncSource = a} :: UpdateResourceDataSync)

instance Core.AWSRequest UpdateResourceDataSync where
  type
    AWSResponse UpdateResourceDataSync =
      UpdateResourceDataSyncResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceDataSyncResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourceDataSync where
  hashWithSalt _salt UpdateResourceDataSync' {..} =
    _salt
      `Prelude.hashWithSalt` syncName
      `Prelude.hashWithSalt` syncType
      `Prelude.hashWithSalt` syncSource

instance Prelude.NFData UpdateResourceDataSync where
  rnf UpdateResourceDataSync' {..} =
    Prelude.rnf syncName `Prelude.seq`
      Prelude.rnf syncType `Prelude.seq`
        Prelude.rnf syncSource

instance Data.ToHeaders UpdateResourceDataSync where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateResourceDataSync" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResourceDataSync where
  toJSON UpdateResourceDataSync' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SyncName" Data..= syncName),
            Prelude.Just ("SyncType" Data..= syncType),
            Prelude.Just ("SyncSource" Data..= syncSource)
          ]
      )

instance Data.ToPath UpdateResourceDataSync where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateResourceDataSync where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceDataSyncResponse' smart constructor.
data UpdateResourceDataSyncResponse = UpdateResourceDataSyncResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceDataSyncResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourceDataSyncResponse_httpStatus' - The response's http status code.
newUpdateResourceDataSyncResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceDataSyncResponse
newUpdateResourceDataSyncResponse pHttpStatus_ =
  UpdateResourceDataSyncResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateResourceDataSyncResponse_httpStatus :: Lens.Lens' UpdateResourceDataSyncResponse Prelude.Int
updateResourceDataSyncResponse_httpStatus = Lens.lens (\UpdateResourceDataSyncResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceDataSyncResponse' {} a -> s {httpStatus = a} :: UpdateResourceDataSyncResponse)

instance
  Prelude.NFData
    UpdateResourceDataSyncResponse
  where
  rnf UpdateResourceDataSyncResponse' {..} =
    Prelude.rnf httpStatus
