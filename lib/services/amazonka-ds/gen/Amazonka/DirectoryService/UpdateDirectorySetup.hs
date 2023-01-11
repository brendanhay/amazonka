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
-- Module      : Amazonka.DirectoryService.UpdateDirectorySetup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the directory for a particular update type.
module Amazonka.DirectoryService.UpdateDirectorySetup
  ( -- * Creating a Request
    UpdateDirectorySetup (..),
    newUpdateDirectorySetup,

    -- * Request Lenses
    updateDirectorySetup_createSnapshotBeforeUpdate,
    updateDirectorySetup_oSUpdateSettings,
    updateDirectorySetup_directoryId,
    updateDirectorySetup_updateType,

    -- * Destructuring the Response
    UpdateDirectorySetupResponse (..),
    newUpdateDirectorySetupResponse,

    -- * Response Lenses
    updateDirectorySetupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDirectorySetup' smart constructor.
data UpdateDirectorySetup = UpdateDirectorySetup'
  { -- | The boolean that specifies if a snapshot for the directory needs to be
    -- taken before updating the directory.
    createSnapshotBeforeUpdate :: Prelude.Maybe Prelude.Bool,
    -- | The settings for the OS update that needs to be performed on the
    -- directory.
    oSUpdateSettings :: Prelude.Maybe OSUpdateSettings,
    -- | The identifier of the directory on which you want to perform the update.
    directoryId :: Prelude.Text,
    -- | The type of update that needs to be performed on the directory. For
    -- example, OS.
    updateType :: UpdateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDirectorySetup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createSnapshotBeforeUpdate', 'updateDirectorySetup_createSnapshotBeforeUpdate' - The boolean that specifies if a snapshot for the directory needs to be
-- taken before updating the directory.
--
-- 'oSUpdateSettings', 'updateDirectorySetup_oSUpdateSettings' - The settings for the OS update that needs to be performed on the
-- directory.
--
-- 'directoryId', 'updateDirectorySetup_directoryId' - The identifier of the directory on which you want to perform the update.
--
-- 'updateType', 'updateDirectorySetup_updateType' - The type of update that needs to be performed on the directory. For
-- example, OS.
newUpdateDirectorySetup ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'updateType'
  UpdateType ->
  UpdateDirectorySetup
newUpdateDirectorySetup pDirectoryId_ pUpdateType_ =
  UpdateDirectorySetup'
    { createSnapshotBeforeUpdate =
        Prelude.Nothing,
      oSUpdateSettings = Prelude.Nothing,
      directoryId = pDirectoryId_,
      updateType = pUpdateType_
    }

-- | The boolean that specifies if a snapshot for the directory needs to be
-- taken before updating the directory.
updateDirectorySetup_createSnapshotBeforeUpdate :: Lens.Lens' UpdateDirectorySetup (Prelude.Maybe Prelude.Bool)
updateDirectorySetup_createSnapshotBeforeUpdate = Lens.lens (\UpdateDirectorySetup' {createSnapshotBeforeUpdate} -> createSnapshotBeforeUpdate) (\s@UpdateDirectorySetup' {} a -> s {createSnapshotBeforeUpdate = a} :: UpdateDirectorySetup)

-- | The settings for the OS update that needs to be performed on the
-- directory.
updateDirectorySetup_oSUpdateSettings :: Lens.Lens' UpdateDirectorySetup (Prelude.Maybe OSUpdateSettings)
updateDirectorySetup_oSUpdateSettings = Lens.lens (\UpdateDirectorySetup' {oSUpdateSettings} -> oSUpdateSettings) (\s@UpdateDirectorySetup' {} a -> s {oSUpdateSettings = a} :: UpdateDirectorySetup)

-- | The identifier of the directory on which you want to perform the update.
updateDirectorySetup_directoryId :: Lens.Lens' UpdateDirectorySetup Prelude.Text
updateDirectorySetup_directoryId = Lens.lens (\UpdateDirectorySetup' {directoryId} -> directoryId) (\s@UpdateDirectorySetup' {} a -> s {directoryId = a} :: UpdateDirectorySetup)

-- | The type of update that needs to be performed on the directory. For
-- example, OS.
updateDirectorySetup_updateType :: Lens.Lens' UpdateDirectorySetup UpdateType
updateDirectorySetup_updateType = Lens.lens (\UpdateDirectorySetup' {updateType} -> updateType) (\s@UpdateDirectorySetup' {} a -> s {updateType = a} :: UpdateDirectorySetup)

instance Core.AWSRequest UpdateDirectorySetup where
  type
    AWSResponse UpdateDirectorySetup =
      UpdateDirectorySetupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDirectorySetupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDirectorySetup where
  hashWithSalt _salt UpdateDirectorySetup' {..} =
    _salt
      `Prelude.hashWithSalt` createSnapshotBeforeUpdate
      `Prelude.hashWithSalt` oSUpdateSettings
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` updateType

instance Prelude.NFData UpdateDirectorySetup where
  rnf UpdateDirectorySetup' {..} =
    Prelude.rnf createSnapshotBeforeUpdate
      `Prelude.seq` Prelude.rnf oSUpdateSettings
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf updateType

instance Data.ToHeaders UpdateDirectorySetup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.UpdateDirectorySetup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDirectorySetup where
  toJSON UpdateDirectorySetup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreateSnapshotBeforeUpdate" Data..=)
              Prelude.<$> createSnapshotBeforeUpdate,
            ("OSUpdateSettings" Data..=)
              Prelude.<$> oSUpdateSettings,
            Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("UpdateType" Data..= updateType)
          ]
      )

instance Data.ToPath UpdateDirectorySetup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDirectorySetup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDirectorySetupResponse' smart constructor.
data UpdateDirectorySetupResponse = UpdateDirectorySetupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDirectorySetupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDirectorySetupResponse_httpStatus' - The response's http status code.
newUpdateDirectorySetupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDirectorySetupResponse
newUpdateDirectorySetupResponse pHttpStatus_ =
  UpdateDirectorySetupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDirectorySetupResponse_httpStatus :: Lens.Lens' UpdateDirectorySetupResponse Prelude.Int
updateDirectorySetupResponse_httpStatus = Lens.lens (\UpdateDirectorySetupResponse' {httpStatus} -> httpStatus) (\s@UpdateDirectorySetupResponse' {} a -> s {httpStatus = a} :: UpdateDirectorySetupResponse)

instance Prelude.NFData UpdateDirectorySetupResponse where
  rnf UpdateDirectorySetupResponse' {..} =
    Prelude.rnf httpStatus
