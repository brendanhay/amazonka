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
-- Module      : Network.AWS.CodePipeline.Types.JobData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.JobData where

import Network.AWS.CodePipeline.Types.AWSSessionCredentials
import Network.AWS.CodePipeline.Types.ActionConfiguration
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.Artifact
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.CodePipeline.Types.PipelineContext
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents other information about a job required for a job worker to
-- complete the job.
--
-- /See:/ 'newJobData' smart constructor.
data JobData = JobData'
  { -- | Represents an AWS session credentials object. These credentials are
    -- temporary credentials that are issued by AWS Secure Token Service (STS).
    -- They can be used to access input and output artifacts in the S3 bucket
    -- used to store artifacts for the pipeline in AWS CodePipeline.
    artifactCredentials :: Prelude.Maybe (Core.Sensitive AWSSessionCredentials),
    -- | Represents information about the key used to encrypt data in the
    -- artifact store, such as an AWS Key Management Service (AWS KMS) key.
    encryptionKey :: Prelude.Maybe EncryptionKey,
    -- | Represents information about an action configuration.
    actionConfiguration :: Prelude.Maybe ActionConfiguration,
    -- | The artifact supplied to the job.
    inputArtifacts :: Prelude.Maybe [Artifact],
    -- | Represents information about an action type.
    actionTypeId :: Prelude.Maybe ActionTypeId,
    -- | Represents information about a pipeline to a job worker.
    --
    -- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
    pipelineContext :: Prelude.Maybe PipelineContext,
    -- | The output of the job.
    outputArtifacts :: Prelude.Maybe [Artifact],
    -- | A system-generated token, such as a AWS CodeDeploy deployment ID,
    -- required by a job to continue the job asynchronously.
    continuationToken :: Prelude.Maybe Prelude.Text
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
-- 'artifactCredentials', 'jobData_artifactCredentials' - Represents an AWS session credentials object. These credentials are
-- temporary credentials that are issued by AWS Secure Token Service (STS).
-- They can be used to access input and output artifacts in the S3 bucket
-- used to store artifacts for the pipeline in AWS CodePipeline.
--
-- 'encryptionKey', 'jobData_encryptionKey' - Represents information about the key used to encrypt data in the
-- artifact store, such as an AWS Key Management Service (AWS KMS) key.
--
-- 'actionConfiguration', 'jobData_actionConfiguration' - Represents information about an action configuration.
--
-- 'inputArtifacts', 'jobData_inputArtifacts' - The artifact supplied to the job.
--
-- 'actionTypeId', 'jobData_actionTypeId' - Represents information about an action type.
--
-- 'pipelineContext', 'jobData_pipelineContext' - Represents information about a pipeline to a job worker.
--
-- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
--
-- 'outputArtifacts', 'jobData_outputArtifacts' - The output of the job.
--
-- 'continuationToken', 'jobData_continuationToken' - A system-generated token, such as a AWS CodeDeploy deployment ID,
-- required by a job to continue the job asynchronously.
newJobData ::
  JobData
newJobData =
  JobData'
    { artifactCredentials = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      actionConfiguration = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      actionTypeId = Prelude.Nothing,
      pipelineContext = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      continuationToken = Prelude.Nothing
    }

-- | Represents an AWS session credentials object. These credentials are
-- temporary credentials that are issued by AWS Secure Token Service (STS).
-- They can be used to access input and output artifacts in the S3 bucket
-- used to store artifacts for the pipeline in AWS CodePipeline.
jobData_artifactCredentials :: Lens.Lens' JobData (Prelude.Maybe AWSSessionCredentials)
jobData_artifactCredentials = Lens.lens (\JobData' {artifactCredentials} -> artifactCredentials) (\s@JobData' {} a -> s {artifactCredentials = a} :: JobData) Prelude.. Lens.mapping Core._Sensitive

-- | Represents information about the key used to encrypt data in the
-- artifact store, such as an AWS Key Management Service (AWS KMS) key.
jobData_encryptionKey :: Lens.Lens' JobData (Prelude.Maybe EncryptionKey)
jobData_encryptionKey = Lens.lens (\JobData' {encryptionKey} -> encryptionKey) (\s@JobData' {} a -> s {encryptionKey = a} :: JobData)

-- | Represents information about an action configuration.
jobData_actionConfiguration :: Lens.Lens' JobData (Prelude.Maybe ActionConfiguration)
jobData_actionConfiguration = Lens.lens (\JobData' {actionConfiguration} -> actionConfiguration) (\s@JobData' {} a -> s {actionConfiguration = a} :: JobData)

-- | The artifact supplied to the job.
jobData_inputArtifacts :: Lens.Lens' JobData (Prelude.Maybe [Artifact])
jobData_inputArtifacts = Lens.lens (\JobData' {inputArtifacts} -> inputArtifacts) (\s@JobData' {} a -> s {inputArtifacts = a} :: JobData) Prelude.. Lens.mapping Lens._Coerce

-- | Represents information about an action type.
jobData_actionTypeId :: Lens.Lens' JobData (Prelude.Maybe ActionTypeId)
jobData_actionTypeId = Lens.lens (\JobData' {actionTypeId} -> actionTypeId) (\s@JobData' {} a -> s {actionTypeId = a} :: JobData)

-- | Represents information about a pipeline to a job worker.
--
-- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
jobData_pipelineContext :: Lens.Lens' JobData (Prelude.Maybe PipelineContext)
jobData_pipelineContext = Lens.lens (\JobData' {pipelineContext} -> pipelineContext) (\s@JobData' {} a -> s {pipelineContext = a} :: JobData)

-- | The output of the job.
jobData_outputArtifacts :: Lens.Lens' JobData (Prelude.Maybe [Artifact])
jobData_outputArtifacts = Lens.lens (\JobData' {outputArtifacts} -> outputArtifacts) (\s@JobData' {} a -> s {outputArtifacts = a} :: JobData) Prelude.. Lens.mapping Lens._Coerce

-- | A system-generated token, such as a AWS CodeDeploy deployment ID,
-- required by a job to continue the job asynchronously.
jobData_continuationToken :: Lens.Lens' JobData (Prelude.Maybe Prelude.Text)
jobData_continuationToken = Lens.lens (\JobData' {continuationToken} -> continuationToken) (\s@JobData' {} a -> s {continuationToken = a} :: JobData)

instance Core.FromJSON JobData where
  parseJSON =
    Core.withObject
      "JobData"
      ( \x ->
          JobData'
            Prelude.<$> (x Core..:? "artifactCredentials")
            Prelude.<*> (x Core..:? "encryptionKey")
            Prelude.<*> (x Core..:? "actionConfiguration")
            Prelude.<*> (x Core..:? "inputArtifacts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "actionTypeId")
            Prelude.<*> (x Core..:? "pipelineContext")
            Prelude.<*> ( x Core..:? "outputArtifacts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "continuationToken")
      )

instance Prelude.Hashable JobData

instance Prelude.NFData JobData
