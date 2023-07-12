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
-- Module      : Amazonka.M2.Types.ApplicationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.ApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.ApplicationDeploymentLifecycle
import Amazonka.M2.Types.ApplicationLifecycle
import Amazonka.M2.Types.ApplicationVersionLifecycle
import Amazonka.M2.Types.EngineType
import qualified Amazonka.Prelude as Prelude

-- | A subset of the possible application attributes. Used in the application
-- list.
--
-- /See:/ 'newApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { -- | Indicates either an ongoing deployment or if the application has ever
    -- deployed successfully.
    deploymentStatus :: Prelude.Maybe ApplicationDeploymentLifecycle,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the runtime environment that hosts this
    -- application.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when you last started the application. Null until the
    -- application runs for the first time.
    lastStartTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates the status of the latest version of the application.
    versionStatus :: Prelude.Maybe ApplicationVersionLifecycle,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationArn :: Prelude.Text,
    -- | The unique identifier of the application.
    applicationId :: Prelude.Text,
    -- | The version of the application.
    applicationVersion :: Prelude.Natural,
    -- | The timestamp when the application was created.
    creationTime :: Data.POSIX,
    -- | The type of the target platform for this application.
    engineType :: EngineType,
    -- | The name of the application.
    name :: Prelude.Text,
    -- | The status of the application.
    status :: ApplicationLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatus', 'applicationSummary_deploymentStatus' - Indicates either an ongoing deployment or if the application has ever
-- deployed successfully.
--
-- 'description', 'applicationSummary_description' - The description of the application.
--
-- 'environmentId', 'applicationSummary_environmentId' - The unique identifier of the runtime environment that hosts this
-- application.
--
-- 'lastStartTime', 'applicationSummary_lastStartTime' - The timestamp when you last started the application. Null until the
-- application runs for the first time.
--
-- 'versionStatus', 'applicationSummary_versionStatus' - Indicates the status of the latest version of the application.
--
-- 'applicationArn', 'applicationSummary_applicationArn' - The Amazon Resource Name (ARN) of the application.
--
-- 'applicationId', 'applicationSummary_applicationId' - The unique identifier of the application.
--
-- 'applicationVersion', 'applicationSummary_applicationVersion' - The version of the application.
--
-- 'creationTime', 'applicationSummary_creationTime' - The timestamp when the application was created.
--
-- 'engineType', 'applicationSummary_engineType' - The type of the target platform for this application.
--
-- 'name', 'applicationSummary_name' - The name of the application.
--
-- 'status', 'applicationSummary_status' - The status of the application.
newApplicationSummary ::
  -- | 'applicationArn'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationVersion'
  Prelude.Natural ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'engineType'
  EngineType ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ApplicationLifecycle ->
  ApplicationSummary
newApplicationSummary
  pApplicationArn_
  pApplicationId_
  pApplicationVersion_
  pCreationTime_
  pEngineType_
  pName_
  pStatus_ =
    ApplicationSummary'
      { deploymentStatus =
          Prelude.Nothing,
        description = Prelude.Nothing,
        environmentId = Prelude.Nothing,
        lastStartTime = Prelude.Nothing,
        versionStatus = Prelude.Nothing,
        applicationArn = pApplicationArn_,
        applicationId = pApplicationId_,
        applicationVersion = pApplicationVersion_,
        creationTime = Data._Time Lens.# pCreationTime_,
        engineType = pEngineType_,
        name = pName_,
        status = pStatus_
      }

-- | Indicates either an ongoing deployment or if the application has ever
-- deployed successfully.
applicationSummary_deploymentStatus :: Lens.Lens' ApplicationSummary (Prelude.Maybe ApplicationDeploymentLifecycle)
applicationSummary_deploymentStatus = Lens.lens (\ApplicationSummary' {deploymentStatus} -> deploymentStatus) (\s@ApplicationSummary' {} a -> s {deploymentStatus = a} :: ApplicationSummary)

-- | The description of the application.
applicationSummary_description :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_description = Lens.lens (\ApplicationSummary' {description} -> description) (\s@ApplicationSummary' {} a -> s {description = a} :: ApplicationSummary)

-- | The unique identifier of the runtime environment that hosts this
-- application.
applicationSummary_environmentId :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_environmentId = Lens.lens (\ApplicationSummary' {environmentId} -> environmentId) (\s@ApplicationSummary' {} a -> s {environmentId = a} :: ApplicationSummary)

-- | The timestamp when you last started the application. Null until the
-- application runs for the first time.
applicationSummary_lastStartTime :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.UTCTime)
applicationSummary_lastStartTime = Lens.lens (\ApplicationSummary' {lastStartTime} -> lastStartTime) (\s@ApplicationSummary' {} a -> s {lastStartTime = a} :: ApplicationSummary) Prelude.. Lens.mapping Data._Time

-- | Indicates the status of the latest version of the application.
applicationSummary_versionStatus :: Lens.Lens' ApplicationSummary (Prelude.Maybe ApplicationVersionLifecycle)
applicationSummary_versionStatus = Lens.lens (\ApplicationSummary' {versionStatus} -> versionStatus) (\s@ApplicationSummary' {} a -> s {versionStatus = a} :: ApplicationSummary)

-- | The Amazon Resource Name (ARN) of the application.
applicationSummary_applicationArn :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_applicationArn = Lens.lens (\ApplicationSummary' {applicationArn} -> applicationArn) (\s@ApplicationSummary' {} a -> s {applicationArn = a} :: ApplicationSummary)

-- | The unique identifier of the application.
applicationSummary_applicationId :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_applicationId = Lens.lens (\ApplicationSummary' {applicationId} -> applicationId) (\s@ApplicationSummary' {} a -> s {applicationId = a} :: ApplicationSummary)

-- | The version of the application.
applicationSummary_applicationVersion :: Lens.Lens' ApplicationSummary Prelude.Natural
applicationSummary_applicationVersion = Lens.lens (\ApplicationSummary' {applicationVersion} -> applicationVersion) (\s@ApplicationSummary' {} a -> s {applicationVersion = a} :: ApplicationSummary)

-- | The timestamp when the application was created.
applicationSummary_creationTime :: Lens.Lens' ApplicationSummary Prelude.UTCTime
applicationSummary_creationTime = Lens.lens (\ApplicationSummary' {creationTime} -> creationTime) (\s@ApplicationSummary' {} a -> s {creationTime = a} :: ApplicationSummary) Prelude.. Data._Time

-- | The type of the target platform for this application.
applicationSummary_engineType :: Lens.Lens' ApplicationSummary EngineType
applicationSummary_engineType = Lens.lens (\ApplicationSummary' {engineType} -> engineType) (\s@ApplicationSummary' {} a -> s {engineType = a} :: ApplicationSummary)

-- | The name of the application.
applicationSummary_name :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_name = Lens.lens (\ApplicationSummary' {name} -> name) (\s@ApplicationSummary' {} a -> s {name = a} :: ApplicationSummary)

-- | The status of the application.
applicationSummary_status :: Lens.Lens' ApplicationSummary ApplicationLifecycle
applicationSummary_status = Lens.lens (\ApplicationSummary' {status} -> status) (\s@ApplicationSummary' {} a -> s {status = a} :: ApplicationSummary)

instance Data.FromJSON ApplicationSummary where
  parseJSON =
    Data.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Prelude.<$> (x Data..:? "deploymentStatus")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "environmentId")
            Prelude.<*> (x Data..:? "lastStartTime")
            Prelude.<*> (x Data..:? "versionStatus")
            Prelude.<*> (x Data..: "applicationArn")
            Prelude.<*> (x Data..: "applicationId")
            Prelude.<*> (x Data..: "applicationVersion")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "engineType")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ApplicationSummary where
  hashWithSalt _salt ApplicationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` lastStartTime
      `Prelude.hashWithSalt` versionStatus
      `Prelude.hashWithSalt` applicationArn
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData ApplicationSummary where
  rnf ApplicationSummary' {..} =
    Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf lastStartTime
      `Prelude.seq` Prelude.rnf versionStatus
      `Prelude.seq` Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
