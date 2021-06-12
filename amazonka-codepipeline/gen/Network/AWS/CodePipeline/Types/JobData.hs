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

-- | Represents other information about a job required for a job worker to
-- complete the job.
--
-- /See:/ 'newJobData' smart constructor.
data JobData = JobData'
  { -- | Represents an AWS session credentials object. These credentials are
    -- temporary credentials that are issued by AWS Secure Token Service (STS).
    -- They can be used to access input and output artifacts in the S3 bucket
    -- used to store artifacts for the pipeline in AWS CodePipeline.
    artifactCredentials :: Core.Maybe (Core.Sensitive AWSSessionCredentials),
    -- | Represents information about the key used to encrypt data in the
    -- artifact store, such as an AWS Key Management Service (AWS KMS) key.
    encryptionKey :: Core.Maybe EncryptionKey,
    -- | Represents information about an action configuration.
    actionConfiguration :: Core.Maybe ActionConfiguration,
    -- | Represents information about an action type.
    actionTypeId :: Core.Maybe ActionTypeId,
    -- | The artifact supplied to the job.
    inputArtifacts :: Core.Maybe [Artifact],
    -- | Represents information about a pipeline to a job worker.
    --
    -- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
    pipelineContext :: Core.Maybe PipelineContext,
    -- | A system-generated token, such as a AWS CodeDeploy deployment ID,
    -- required by a job to continue the job asynchronously.
    continuationToken :: Core.Maybe Core.Text,
    -- | The output of the job.
    outputArtifacts :: Core.Maybe [Artifact]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'actionTypeId', 'jobData_actionTypeId' - Represents information about an action type.
--
-- 'inputArtifacts', 'jobData_inputArtifacts' - The artifact supplied to the job.
--
-- 'pipelineContext', 'jobData_pipelineContext' - Represents information about a pipeline to a job worker.
--
-- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
--
-- 'continuationToken', 'jobData_continuationToken' - A system-generated token, such as a AWS CodeDeploy deployment ID,
-- required by a job to continue the job asynchronously.
--
-- 'outputArtifacts', 'jobData_outputArtifacts' - The output of the job.
newJobData ::
  JobData
newJobData =
  JobData'
    { artifactCredentials = Core.Nothing,
      encryptionKey = Core.Nothing,
      actionConfiguration = Core.Nothing,
      actionTypeId = Core.Nothing,
      inputArtifacts = Core.Nothing,
      pipelineContext = Core.Nothing,
      continuationToken = Core.Nothing,
      outputArtifacts = Core.Nothing
    }

-- | Represents an AWS session credentials object. These credentials are
-- temporary credentials that are issued by AWS Secure Token Service (STS).
-- They can be used to access input and output artifacts in the S3 bucket
-- used to store artifacts for the pipeline in AWS CodePipeline.
jobData_artifactCredentials :: Lens.Lens' JobData (Core.Maybe AWSSessionCredentials)
jobData_artifactCredentials = Lens.lens (\JobData' {artifactCredentials} -> artifactCredentials) (\s@JobData' {} a -> s {artifactCredentials = a} :: JobData) Core.. Lens.mapping Core._Sensitive

-- | Represents information about the key used to encrypt data in the
-- artifact store, such as an AWS Key Management Service (AWS KMS) key.
jobData_encryptionKey :: Lens.Lens' JobData (Core.Maybe EncryptionKey)
jobData_encryptionKey = Lens.lens (\JobData' {encryptionKey} -> encryptionKey) (\s@JobData' {} a -> s {encryptionKey = a} :: JobData)

-- | Represents information about an action configuration.
jobData_actionConfiguration :: Lens.Lens' JobData (Core.Maybe ActionConfiguration)
jobData_actionConfiguration = Lens.lens (\JobData' {actionConfiguration} -> actionConfiguration) (\s@JobData' {} a -> s {actionConfiguration = a} :: JobData)

-- | Represents information about an action type.
jobData_actionTypeId :: Lens.Lens' JobData (Core.Maybe ActionTypeId)
jobData_actionTypeId = Lens.lens (\JobData' {actionTypeId} -> actionTypeId) (\s@JobData' {} a -> s {actionTypeId = a} :: JobData)

-- | The artifact supplied to the job.
jobData_inputArtifacts :: Lens.Lens' JobData (Core.Maybe [Artifact])
jobData_inputArtifacts = Lens.lens (\JobData' {inputArtifacts} -> inputArtifacts) (\s@JobData' {} a -> s {inputArtifacts = a} :: JobData) Core.. Lens.mapping Lens._Coerce

-- | Represents information about a pipeline to a job worker.
--
-- Includes @pipelineArn@ and @pipelineExecutionId@ for custom jobs.
jobData_pipelineContext :: Lens.Lens' JobData (Core.Maybe PipelineContext)
jobData_pipelineContext = Lens.lens (\JobData' {pipelineContext} -> pipelineContext) (\s@JobData' {} a -> s {pipelineContext = a} :: JobData)

-- | A system-generated token, such as a AWS CodeDeploy deployment ID,
-- required by a job to continue the job asynchronously.
jobData_continuationToken :: Lens.Lens' JobData (Core.Maybe Core.Text)
jobData_continuationToken = Lens.lens (\JobData' {continuationToken} -> continuationToken) (\s@JobData' {} a -> s {continuationToken = a} :: JobData)

-- | The output of the job.
jobData_outputArtifacts :: Lens.Lens' JobData (Core.Maybe [Artifact])
jobData_outputArtifacts = Lens.lens (\JobData' {outputArtifacts} -> outputArtifacts) (\s@JobData' {} a -> s {outputArtifacts = a} :: JobData) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON JobData where
  parseJSON =
    Core.withObject
      "JobData"
      ( \x ->
          JobData'
            Core.<$> (x Core..:? "artifactCredentials")
            Core.<*> (x Core..:? "encryptionKey")
            Core.<*> (x Core..:? "actionConfiguration")
            Core.<*> (x Core..:? "actionTypeId")
            Core.<*> (x Core..:? "inputArtifacts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "pipelineContext")
            Core.<*> (x Core..:? "continuationToken")
            Core.<*> (x Core..:? "outputArtifacts" Core..!= Core.mempty)
      )

instance Core.Hashable JobData

instance Core.NFData JobData
