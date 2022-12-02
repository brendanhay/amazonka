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
-- Module      : Amazonka.CodePipeline.Types.JobData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.JobData where

import Amazonka.CodePipeline.Types.AWSSessionCredentials
import Amazonka.CodePipeline.Types.ActionConfiguration
import Amazonka.CodePipeline.Types.ActionTypeId
import Amazonka.CodePipeline.Types.Artifact
import Amazonka.CodePipeline.Types.EncryptionKey
import Amazonka.CodePipeline.Types.PipelineContext
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents other information about a job required for a job worker to
-- complete the job.
--
-- /See:/ 'newJobData' smart constructor.
data JobData = JobData'
  { -- | Represents information about an action type.
    actionTypeId :: Prelude.Maybe ActionTypeId,
    -- | The output of the job.
    outputArtifacts :: Prelude.Maybe [Artifact],
    -- | Represents an AWS session credentials object. These credentials are
    -- temporary credentials that are issued by AWS Secure Token Service (STS).
    -- They can be used to access input and output artifacts in the S3 bucket
    -- used to store artifacts for the pipeline in AWS CodePipeline.
    artifactCredentials :: Prelude.Maybe (Data.Sensitive AWSSessionCredentials),
    -- | A system-generated token, such as a AWS CodeDeploy deployment ID,
    -- required by a job to continue the job asynchronously.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | Represents information about an action configuration.
    actionConfiguration :: Prelude.Maybe ActionConfiguration,
    -- | The artifact supplied to the job.
    inputArtifacts :: Prelude.Maybe [Artifact],
    -- | Represents information about the key used to encrypt data in the
    -- artifact store, such as an AWS Key Management Service (AWS KMS) key.
    encryptionKey :: Prelude.Maybe EncryptionKey,
    -- | Represents information about a pipeline to a job worker.
    --
    -- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
    pipelineContext :: Prelude.Maybe PipelineContext
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionTypeId', 'jobData_actionTypeId' - Represents information about an action type.
--
-- 'outputArtifacts', 'jobData_outputArtifacts' - The output of the job.
--
-- 'artifactCredentials', 'jobData_artifactCredentials' - Represents an AWS session credentials object. These credentials are
-- temporary credentials that are issued by AWS Secure Token Service (STS).
-- They can be used to access input and output artifacts in the S3 bucket
-- used to store artifacts for the pipeline in AWS CodePipeline.
--
-- 'continuationToken', 'jobData_continuationToken' - A system-generated token, such as a AWS CodeDeploy deployment ID,
-- required by a job to continue the job asynchronously.
--
-- 'actionConfiguration', 'jobData_actionConfiguration' - Represents information about an action configuration.
--
-- 'inputArtifacts', 'jobData_inputArtifacts' - The artifact supplied to the job.
--
-- 'encryptionKey', 'jobData_encryptionKey' - Represents information about the key used to encrypt data in the
-- artifact store, such as an AWS Key Management Service (AWS KMS) key.
--
-- 'pipelineContext', 'jobData_pipelineContext' - Represents information about a pipeline to a job worker.
--
-- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
newJobData ::
  JobData
newJobData =
  JobData'
    { actionTypeId = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      artifactCredentials = Prelude.Nothing,
      continuationToken = Prelude.Nothing,
      actionConfiguration = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      pipelineContext = Prelude.Nothing
    }

-- | Represents information about an action type.
jobData_actionTypeId :: Lens.Lens' JobData (Prelude.Maybe ActionTypeId)
jobData_actionTypeId = Lens.lens (\JobData' {actionTypeId} -> actionTypeId) (\s@JobData' {} a -> s {actionTypeId = a} :: JobData)

-- | The output of the job.
jobData_outputArtifacts :: Lens.Lens' JobData (Prelude.Maybe [Artifact])
jobData_outputArtifacts = Lens.lens (\JobData' {outputArtifacts} -> outputArtifacts) (\s@JobData' {} a -> s {outputArtifacts = a} :: JobData) Prelude.. Lens.mapping Lens.coerced

-- | Represents an AWS session credentials object. These credentials are
-- temporary credentials that are issued by AWS Secure Token Service (STS).
-- They can be used to access input and output artifacts in the S3 bucket
-- used to store artifacts for the pipeline in AWS CodePipeline.
jobData_artifactCredentials :: Lens.Lens' JobData (Prelude.Maybe AWSSessionCredentials)
jobData_artifactCredentials = Lens.lens (\JobData' {artifactCredentials} -> artifactCredentials) (\s@JobData' {} a -> s {artifactCredentials = a} :: JobData) Prelude.. Lens.mapping Data._Sensitive

-- | A system-generated token, such as a AWS CodeDeploy deployment ID,
-- required by a job to continue the job asynchronously.
jobData_continuationToken :: Lens.Lens' JobData (Prelude.Maybe Prelude.Text)
jobData_continuationToken = Lens.lens (\JobData' {continuationToken} -> continuationToken) (\s@JobData' {} a -> s {continuationToken = a} :: JobData)

-- | Represents information about an action configuration.
jobData_actionConfiguration :: Lens.Lens' JobData (Prelude.Maybe ActionConfiguration)
jobData_actionConfiguration = Lens.lens (\JobData' {actionConfiguration} -> actionConfiguration) (\s@JobData' {} a -> s {actionConfiguration = a} :: JobData)

-- | The artifact supplied to the job.
jobData_inputArtifacts :: Lens.Lens' JobData (Prelude.Maybe [Artifact])
jobData_inputArtifacts = Lens.lens (\JobData' {inputArtifacts} -> inputArtifacts) (\s@JobData' {} a -> s {inputArtifacts = a} :: JobData) Prelude.. Lens.mapping Lens.coerced

-- | Represents information about the key used to encrypt data in the
-- artifact store, such as an AWS Key Management Service (AWS KMS) key.
jobData_encryptionKey :: Lens.Lens' JobData (Prelude.Maybe EncryptionKey)
jobData_encryptionKey = Lens.lens (\JobData' {encryptionKey} -> encryptionKey) (\s@JobData' {} a -> s {encryptionKey = a} :: JobData)

-- | Represents information about a pipeline to a job worker.
--
-- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
jobData_pipelineContext :: Lens.Lens' JobData (Prelude.Maybe PipelineContext)
jobData_pipelineContext = Lens.lens (\JobData' {pipelineContext} -> pipelineContext) (\s@JobData' {} a -> s {pipelineContext = a} :: JobData)

instance Data.FromJSON JobData where
  parseJSON =
    Data.withObject
      "JobData"
      ( \x ->
          JobData'
            Prelude.<$> (x Data..:? "actionTypeId")
            Prelude.<*> ( x Data..:? "outputArtifacts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "artifactCredentials")
            Prelude.<*> (x Data..:? "continuationToken")
            Prelude.<*> (x Data..:? "actionConfiguration")
            Prelude.<*> (x Data..:? "inputArtifacts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "encryptionKey")
            Prelude.<*> (x Data..:? "pipelineContext")
      )

instance Prelude.Hashable JobData where
  hashWithSalt _salt JobData' {..} =
    _salt `Prelude.hashWithSalt` actionTypeId
      `Prelude.hashWithSalt` outputArtifacts
      `Prelude.hashWithSalt` artifactCredentials
      `Prelude.hashWithSalt` continuationToken
      `Prelude.hashWithSalt` actionConfiguration
      `Prelude.hashWithSalt` inputArtifacts
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` pipelineContext

instance Prelude.NFData JobData where
  rnf JobData' {..} =
    Prelude.rnf actionTypeId
      `Prelude.seq` Prelude.rnf outputArtifacts
      `Prelude.seq` Prelude.rnf artifactCredentials
      `Prelude.seq` Prelude.rnf continuationToken
      `Prelude.seq` Prelude.rnf actionConfiguration
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf pipelineContext
