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
-- Module      : Amazonka.LookoutVision.Types.ModelPackagingJobMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelPackagingJobMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.ModelPackagingJobStatus
import qualified Amazonka.Prelude as Prelude

-- | Metadata for a model packaging job. For more information, see
-- ListModelPackagingJobs.
--
-- /See:/ 'newModelPackagingJobMetadata' smart constructor.
data ModelPackagingJobMetadata = ModelPackagingJobMetadata'
  { -- | The Unix timestamp for the time and date that the model packaging job
    -- was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the model packaging job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for the time and date that the model packaging job
    -- was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The description for the model packaging job.
    modelPackagingJobDescription :: Prelude.Maybe Prelude.Text,
    -- | The AWS service used to package the job. Currently Lookout for Vision
    -- can package jobs with AWS IoT Greengrass.
    modelPackagingMethod :: Prelude.Maybe Prelude.Text,
    -- | The version of the model that is in the model package.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The project that contains the model that is in the model package.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The status of the model packaging job.
    status :: Prelude.Maybe ModelPackagingJobStatus,
    -- | The status message for the model packaging job.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelPackagingJobMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'modelPackagingJobMetadata_creationTimestamp' - The Unix timestamp for the time and date that the model packaging job
-- was created.
--
-- 'jobName', 'modelPackagingJobMetadata_jobName' - The name of the model packaging job.
--
-- 'lastUpdatedTimestamp', 'modelPackagingJobMetadata_lastUpdatedTimestamp' - The Unix timestamp for the time and date that the model packaging job
-- was last updated.
--
-- 'modelPackagingJobDescription', 'modelPackagingJobMetadata_modelPackagingJobDescription' - The description for the model packaging job.
--
-- 'modelPackagingMethod', 'modelPackagingJobMetadata_modelPackagingMethod' - The AWS service used to package the job. Currently Lookout for Vision
-- can package jobs with AWS IoT Greengrass.
--
-- 'modelVersion', 'modelPackagingJobMetadata_modelVersion' - The version of the model that is in the model package.
--
-- 'projectName', 'modelPackagingJobMetadata_projectName' - The project that contains the model that is in the model package.
--
-- 'status', 'modelPackagingJobMetadata_status' - The status of the model packaging job.
--
-- 'statusMessage', 'modelPackagingJobMetadata_statusMessage' - The status message for the model packaging job.
newModelPackagingJobMetadata ::
  ModelPackagingJobMetadata
newModelPackagingJobMetadata =
  ModelPackagingJobMetadata'
    { creationTimestamp =
        Prelude.Nothing,
      jobName = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      modelPackagingJobDescription = Prelude.Nothing,
      modelPackagingMethod = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      projectName = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The Unix timestamp for the time and date that the model packaging job
-- was created.
modelPackagingJobMetadata_creationTimestamp :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe Prelude.UTCTime)
modelPackagingJobMetadata_creationTimestamp = Lens.lens (\ModelPackagingJobMetadata' {creationTimestamp} -> creationTimestamp) (\s@ModelPackagingJobMetadata' {} a -> s {creationTimestamp = a} :: ModelPackagingJobMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the model packaging job.
modelPackagingJobMetadata_jobName :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe Prelude.Text)
modelPackagingJobMetadata_jobName = Lens.lens (\ModelPackagingJobMetadata' {jobName} -> jobName) (\s@ModelPackagingJobMetadata' {} a -> s {jobName = a} :: ModelPackagingJobMetadata)

-- | The Unix timestamp for the time and date that the model packaging job
-- was last updated.
modelPackagingJobMetadata_lastUpdatedTimestamp :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe Prelude.UTCTime)
modelPackagingJobMetadata_lastUpdatedTimestamp = Lens.lens (\ModelPackagingJobMetadata' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ModelPackagingJobMetadata' {} a -> s {lastUpdatedTimestamp = a} :: ModelPackagingJobMetadata) Prelude.. Lens.mapping Data._Time

-- | The description for the model packaging job.
modelPackagingJobMetadata_modelPackagingJobDescription :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe Prelude.Text)
modelPackagingJobMetadata_modelPackagingJobDescription = Lens.lens (\ModelPackagingJobMetadata' {modelPackagingJobDescription} -> modelPackagingJobDescription) (\s@ModelPackagingJobMetadata' {} a -> s {modelPackagingJobDescription = a} :: ModelPackagingJobMetadata)

-- | The AWS service used to package the job. Currently Lookout for Vision
-- can package jobs with AWS IoT Greengrass.
modelPackagingJobMetadata_modelPackagingMethod :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe Prelude.Text)
modelPackagingJobMetadata_modelPackagingMethod = Lens.lens (\ModelPackagingJobMetadata' {modelPackagingMethod} -> modelPackagingMethod) (\s@ModelPackagingJobMetadata' {} a -> s {modelPackagingMethod = a} :: ModelPackagingJobMetadata)

-- | The version of the model that is in the model package.
modelPackagingJobMetadata_modelVersion :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe Prelude.Text)
modelPackagingJobMetadata_modelVersion = Lens.lens (\ModelPackagingJobMetadata' {modelVersion} -> modelVersion) (\s@ModelPackagingJobMetadata' {} a -> s {modelVersion = a} :: ModelPackagingJobMetadata)

-- | The project that contains the model that is in the model package.
modelPackagingJobMetadata_projectName :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe Prelude.Text)
modelPackagingJobMetadata_projectName = Lens.lens (\ModelPackagingJobMetadata' {projectName} -> projectName) (\s@ModelPackagingJobMetadata' {} a -> s {projectName = a} :: ModelPackagingJobMetadata)

-- | The status of the model packaging job.
modelPackagingJobMetadata_status :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe ModelPackagingJobStatus)
modelPackagingJobMetadata_status = Lens.lens (\ModelPackagingJobMetadata' {status} -> status) (\s@ModelPackagingJobMetadata' {} a -> s {status = a} :: ModelPackagingJobMetadata)

-- | The status message for the model packaging job.
modelPackagingJobMetadata_statusMessage :: Lens.Lens' ModelPackagingJobMetadata (Prelude.Maybe Prelude.Text)
modelPackagingJobMetadata_statusMessage = Lens.lens (\ModelPackagingJobMetadata' {statusMessage} -> statusMessage) (\s@ModelPackagingJobMetadata' {} a -> s {statusMessage = a} :: ModelPackagingJobMetadata)

instance Data.FromJSON ModelPackagingJobMetadata where
  parseJSON =
    Data.withObject
      "ModelPackagingJobMetadata"
      ( \x ->
          ModelPackagingJobMetadata'
            Prelude.<$> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "ModelPackagingJobDescription")
            Prelude.<*> (x Data..:? "ModelPackagingMethod")
            Prelude.<*> (x Data..:? "ModelVersion")
            Prelude.<*> (x Data..:? "ProjectName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable ModelPackagingJobMetadata where
  hashWithSalt _salt ModelPackagingJobMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` modelPackagingJobDescription
      `Prelude.hashWithSalt` modelPackagingMethod
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ModelPackagingJobMetadata where
  rnf ModelPackagingJobMetadata' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf modelPackagingJobDescription
      `Prelude.seq` Prelude.rnf modelPackagingMethod
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
