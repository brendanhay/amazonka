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
-- Module      : Amazonka.Comprehend.Types.FlywheelProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.FlywheelProperties where

import Amazonka.Comprehend.Types.DataSecurityConfig
import Amazonka.Comprehend.Types.FlywheelStatus
import Amazonka.Comprehend.Types.ModelType
import Amazonka.Comprehend.Types.TaskConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The flywheel properties.
--
-- /See:/ 'newFlywheelProperties' smart constructor.
data FlywheelProperties = FlywheelProperties'
  { -- | The Amazon Resource Number (ARN) of the active model version.
    activeModelArn :: Prelude.Maybe Prelude.Text,
    -- | Creation time of the flywheel.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM role that grants Amazon
    -- Comprehend permission to access the flywheel data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon S3 URI of the data lake location.
    dataLakeS3Uri :: Prelude.Maybe Prelude.Text,
    -- | Data security configuration.
    dataSecurityConfig :: Prelude.Maybe DataSecurityConfig,
    -- | The Amazon Resource Number (ARN) of the flywheel.
    flywheelArn :: Prelude.Maybe Prelude.Text,
    -- | Last modified time for the flywheel.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The most recent flywheel iteration.
    latestFlywheelIteration :: Prelude.Maybe Prelude.Text,
    -- | A description of the status of the flywheel.
    message :: Prelude.Maybe Prelude.Text,
    -- | Model type of the flywheel\'s model.
    modelType :: Prelude.Maybe ModelType,
    -- | The status of the flywheel.
    status :: Prelude.Maybe FlywheelStatus,
    -- | Configuration about the custom classifier associated with the flywheel.
    taskConfig :: Prelude.Maybe TaskConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlywheelProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeModelArn', 'flywheelProperties_activeModelArn' - The Amazon Resource Number (ARN) of the active model version.
--
-- 'creationTime', 'flywheelProperties_creationTime' - Creation time of the flywheel.
--
-- 'dataAccessRoleArn', 'flywheelProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the IAM role that grants Amazon
-- Comprehend permission to access the flywheel data.
--
-- 'dataLakeS3Uri', 'flywheelProperties_dataLakeS3Uri' - Amazon S3 URI of the data lake location.
--
-- 'dataSecurityConfig', 'flywheelProperties_dataSecurityConfig' - Data security configuration.
--
-- 'flywheelArn', 'flywheelProperties_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel.
--
-- 'lastModifiedTime', 'flywheelProperties_lastModifiedTime' - Last modified time for the flywheel.
--
-- 'latestFlywheelIteration', 'flywheelProperties_latestFlywheelIteration' - The most recent flywheel iteration.
--
-- 'message', 'flywheelProperties_message' - A description of the status of the flywheel.
--
-- 'modelType', 'flywheelProperties_modelType' - Model type of the flywheel\'s model.
--
-- 'status', 'flywheelProperties_status' - The status of the flywheel.
--
-- 'taskConfig', 'flywheelProperties_taskConfig' - Configuration about the custom classifier associated with the flywheel.
newFlywheelProperties ::
  FlywheelProperties
newFlywheelProperties =
  FlywheelProperties'
    { activeModelArn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      dataLakeS3Uri = Prelude.Nothing,
      dataSecurityConfig = Prelude.Nothing,
      flywheelArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      latestFlywheelIteration = Prelude.Nothing,
      message = Prelude.Nothing,
      modelType = Prelude.Nothing,
      status = Prelude.Nothing,
      taskConfig = Prelude.Nothing
    }

-- | The Amazon Resource Number (ARN) of the active model version.
flywheelProperties_activeModelArn :: Lens.Lens' FlywheelProperties (Prelude.Maybe Prelude.Text)
flywheelProperties_activeModelArn = Lens.lens (\FlywheelProperties' {activeModelArn} -> activeModelArn) (\s@FlywheelProperties' {} a -> s {activeModelArn = a} :: FlywheelProperties)

-- | Creation time of the flywheel.
flywheelProperties_creationTime :: Lens.Lens' FlywheelProperties (Prelude.Maybe Prelude.UTCTime)
flywheelProperties_creationTime = Lens.lens (\FlywheelProperties' {creationTime} -> creationTime) (\s@FlywheelProperties' {} a -> s {creationTime = a} :: FlywheelProperties) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the IAM role that grants Amazon
-- Comprehend permission to access the flywheel data.
flywheelProperties_dataAccessRoleArn :: Lens.Lens' FlywheelProperties (Prelude.Maybe Prelude.Text)
flywheelProperties_dataAccessRoleArn = Lens.lens (\FlywheelProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@FlywheelProperties' {} a -> s {dataAccessRoleArn = a} :: FlywheelProperties)

-- | Amazon S3 URI of the data lake location.
flywheelProperties_dataLakeS3Uri :: Lens.Lens' FlywheelProperties (Prelude.Maybe Prelude.Text)
flywheelProperties_dataLakeS3Uri = Lens.lens (\FlywheelProperties' {dataLakeS3Uri} -> dataLakeS3Uri) (\s@FlywheelProperties' {} a -> s {dataLakeS3Uri = a} :: FlywheelProperties)

-- | Data security configuration.
flywheelProperties_dataSecurityConfig :: Lens.Lens' FlywheelProperties (Prelude.Maybe DataSecurityConfig)
flywheelProperties_dataSecurityConfig = Lens.lens (\FlywheelProperties' {dataSecurityConfig} -> dataSecurityConfig) (\s@FlywheelProperties' {} a -> s {dataSecurityConfig = a} :: FlywheelProperties)

-- | The Amazon Resource Number (ARN) of the flywheel.
flywheelProperties_flywheelArn :: Lens.Lens' FlywheelProperties (Prelude.Maybe Prelude.Text)
flywheelProperties_flywheelArn = Lens.lens (\FlywheelProperties' {flywheelArn} -> flywheelArn) (\s@FlywheelProperties' {} a -> s {flywheelArn = a} :: FlywheelProperties)

-- | Last modified time for the flywheel.
flywheelProperties_lastModifiedTime :: Lens.Lens' FlywheelProperties (Prelude.Maybe Prelude.UTCTime)
flywheelProperties_lastModifiedTime = Lens.lens (\FlywheelProperties' {lastModifiedTime} -> lastModifiedTime) (\s@FlywheelProperties' {} a -> s {lastModifiedTime = a} :: FlywheelProperties) Prelude.. Lens.mapping Data._Time

-- | The most recent flywheel iteration.
flywheelProperties_latestFlywheelIteration :: Lens.Lens' FlywheelProperties (Prelude.Maybe Prelude.Text)
flywheelProperties_latestFlywheelIteration = Lens.lens (\FlywheelProperties' {latestFlywheelIteration} -> latestFlywheelIteration) (\s@FlywheelProperties' {} a -> s {latestFlywheelIteration = a} :: FlywheelProperties)

-- | A description of the status of the flywheel.
flywheelProperties_message :: Lens.Lens' FlywheelProperties (Prelude.Maybe Prelude.Text)
flywheelProperties_message = Lens.lens (\FlywheelProperties' {message} -> message) (\s@FlywheelProperties' {} a -> s {message = a} :: FlywheelProperties)

-- | Model type of the flywheel\'s model.
flywheelProperties_modelType :: Lens.Lens' FlywheelProperties (Prelude.Maybe ModelType)
flywheelProperties_modelType = Lens.lens (\FlywheelProperties' {modelType} -> modelType) (\s@FlywheelProperties' {} a -> s {modelType = a} :: FlywheelProperties)

-- | The status of the flywheel.
flywheelProperties_status :: Lens.Lens' FlywheelProperties (Prelude.Maybe FlywheelStatus)
flywheelProperties_status = Lens.lens (\FlywheelProperties' {status} -> status) (\s@FlywheelProperties' {} a -> s {status = a} :: FlywheelProperties)

-- | Configuration about the custom classifier associated with the flywheel.
flywheelProperties_taskConfig :: Lens.Lens' FlywheelProperties (Prelude.Maybe TaskConfig)
flywheelProperties_taskConfig = Lens.lens (\FlywheelProperties' {taskConfig} -> taskConfig) (\s@FlywheelProperties' {} a -> s {taskConfig = a} :: FlywheelProperties)

instance Data.FromJSON FlywheelProperties where
  parseJSON =
    Data.withObject
      "FlywheelProperties"
      ( \x ->
          FlywheelProperties'
            Prelude.<$> (x Data..:? "ActiveModelArn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "DataLakeS3Uri")
            Prelude.<*> (x Data..:? "DataSecurityConfig")
            Prelude.<*> (x Data..:? "FlywheelArn")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LatestFlywheelIteration")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "ModelType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TaskConfig")
      )

instance Prelude.Hashable FlywheelProperties where
  hashWithSalt _salt FlywheelProperties' {..} =
    _salt
      `Prelude.hashWithSalt` activeModelArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` dataLakeS3Uri
      `Prelude.hashWithSalt` dataSecurityConfig
      `Prelude.hashWithSalt` flywheelArn
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` latestFlywheelIteration
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` taskConfig

instance Prelude.NFData FlywheelProperties where
  rnf FlywheelProperties' {..} =
    Prelude.rnf activeModelArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf dataLakeS3Uri
      `Prelude.seq` Prelude.rnf dataSecurityConfig
      `Prelude.seq` Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf latestFlywheelIteration
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf taskConfig
