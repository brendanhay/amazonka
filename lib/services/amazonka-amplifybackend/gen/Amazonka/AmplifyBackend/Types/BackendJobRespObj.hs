{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyBackend.Types.BackendJobRespObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.BackendJobRespObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The response object for this operation.
--
-- /See:/ 'newBackendJobRespObj' smart constructor.
data BackendJobRespObj = BackendJobRespObj'
  { -- | The time when the job was created.
    createTime :: Prelude.Maybe Prelude.Text,
    -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | The ID for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name of the operation.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The current status of the request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The time when the job was last updated.
    updateTime :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendJobRespObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'backendJobRespObj_createTime' - The time when the job was created.
--
-- 'error', 'backendJobRespObj_error' - If the request fails, this error is returned.
--
-- 'jobId', 'backendJobRespObj_jobId' - The ID for the job.
--
-- 'operation', 'backendJobRespObj_operation' - The name of the operation.
--
-- 'status', 'backendJobRespObj_status' - The current status of the request.
--
-- 'updateTime', 'backendJobRespObj_updateTime' - The time when the job was last updated.
--
-- 'appId', 'backendJobRespObj_appId' - The app ID.
--
-- 'backendEnvironmentName', 'backendJobRespObj_backendEnvironmentName' - The name of the backend environment.
newBackendJobRespObj ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  BackendJobRespObj
newBackendJobRespObj pAppId_ pBackendEnvironmentName_ =
  BackendJobRespObj'
    { createTime = Prelude.Nothing,
      error = Prelude.Nothing,
      jobId = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      appId = pAppId_,
      backendEnvironmentName = pBackendEnvironmentName_
    }

-- | The time when the job was created.
backendJobRespObj_createTime :: Lens.Lens' BackendJobRespObj (Prelude.Maybe Prelude.Text)
backendJobRespObj_createTime = Lens.lens (\BackendJobRespObj' {createTime} -> createTime) (\s@BackendJobRespObj' {} a -> s {createTime = a} :: BackendJobRespObj)

-- | If the request fails, this error is returned.
backendJobRespObj_error :: Lens.Lens' BackendJobRespObj (Prelude.Maybe Prelude.Text)
backendJobRespObj_error = Lens.lens (\BackendJobRespObj' {error} -> error) (\s@BackendJobRespObj' {} a -> s {error = a} :: BackendJobRespObj)

-- | The ID for the job.
backendJobRespObj_jobId :: Lens.Lens' BackendJobRespObj (Prelude.Maybe Prelude.Text)
backendJobRespObj_jobId = Lens.lens (\BackendJobRespObj' {jobId} -> jobId) (\s@BackendJobRespObj' {} a -> s {jobId = a} :: BackendJobRespObj)

-- | The name of the operation.
backendJobRespObj_operation :: Lens.Lens' BackendJobRespObj (Prelude.Maybe Prelude.Text)
backendJobRespObj_operation = Lens.lens (\BackendJobRespObj' {operation} -> operation) (\s@BackendJobRespObj' {} a -> s {operation = a} :: BackendJobRespObj)

-- | The current status of the request.
backendJobRespObj_status :: Lens.Lens' BackendJobRespObj (Prelude.Maybe Prelude.Text)
backendJobRespObj_status = Lens.lens (\BackendJobRespObj' {status} -> status) (\s@BackendJobRespObj' {} a -> s {status = a} :: BackendJobRespObj)

-- | The time when the job was last updated.
backendJobRespObj_updateTime :: Lens.Lens' BackendJobRespObj (Prelude.Maybe Prelude.Text)
backendJobRespObj_updateTime = Lens.lens (\BackendJobRespObj' {updateTime} -> updateTime) (\s@BackendJobRespObj' {} a -> s {updateTime = a} :: BackendJobRespObj)

-- | The app ID.
backendJobRespObj_appId :: Lens.Lens' BackendJobRespObj Prelude.Text
backendJobRespObj_appId = Lens.lens (\BackendJobRespObj' {appId} -> appId) (\s@BackendJobRespObj' {} a -> s {appId = a} :: BackendJobRespObj)

-- | The name of the backend environment.
backendJobRespObj_backendEnvironmentName :: Lens.Lens' BackendJobRespObj Prelude.Text
backendJobRespObj_backendEnvironmentName = Lens.lens (\BackendJobRespObj' {backendEnvironmentName} -> backendEnvironmentName) (\s@BackendJobRespObj' {} a -> s {backendEnvironmentName = a} :: BackendJobRespObj)

instance Data.FromJSON BackendJobRespObj where
  parseJSON =
    Data.withObject
      "BackendJobRespObj"
      ( \x ->
          BackendJobRespObj'
            Prelude.<$> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "error")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "operation")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "updateTime")
            Prelude.<*> (x Data..: "appId")
            Prelude.<*> (x Data..: "backendEnvironmentName")
      )

instance Prelude.Hashable BackendJobRespObj where
  hashWithSalt _salt BackendJobRespObj' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName

instance Prelude.NFData BackendJobRespObj where
  rnf BackendJobRespObj' {..} =
    Prelude.rnf createTime `Prelude.seq`
      Prelude.rnf error `Prelude.seq`
        Prelude.rnf jobId `Prelude.seq`
          Prelude.rnf operation `Prelude.seq`
            Prelude.rnf status `Prelude.seq`
              Prelude.rnf updateTime `Prelude.seq`
                Prelude.rnf appId `Prelude.seq`
                  Prelude.rnf backendEnvironmentName
