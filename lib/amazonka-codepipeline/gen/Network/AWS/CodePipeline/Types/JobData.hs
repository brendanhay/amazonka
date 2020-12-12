{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.JobData
  ( JobData (..),

    -- * Smart constructor
    mkJobData,

    -- * Lenses
    jdContinuationToken,
    jdOutputArtifacts,
    jdArtifactCredentials,
    jdPipelineContext,
    jdEncryptionKey,
    jdActionTypeId,
    jdInputArtifacts,
    jdActionConfiguration,
  )
where

import Network.AWS.CodePipeline.Types.AWSSessionCredentials
import Network.AWS.CodePipeline.Types.ActionConfiguration
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.Artifact
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.CodePipeline.Types.PipelineContext
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents other information about a job required for a job worker to complete the job.
--
-- /See:/ 'mkJobData' smart constructor.
data JobData = JobData'
  { continuationToken :: Lude.Maybe Lude.Text,
    outputArtifacts :: Lude.Maybe [Artifact],
    artifactCredentials :: Lude.Maybe AWSSessionCredentials,
    pipelineContext :: Lude.Maybe PipelineContext,
    encryptionKey :: Lude.Maybe EncryptionKey,
    actionTypeId :: Lude.Maybe ActionTypeId,
    inputArtifacts :: Lude.Maybe [Artifact],
    actionConfiguration :: Lude.Maybe ActionConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobData' with the minimum fields required to make a request.
--
-- * 'actionConfiguration' - Represents information about an action configuration.
-- * 'actionTypeId' - Represents information about an action type.
-- * 'artifactCredentials' - Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifacts for the pipeline in AWS CodePipeline.
-- * 'continuationToken' - A system-generated token, such as a AWS CodeDeploy deployment ID, required by a job to continue the job asynchronously.
-- * 'encryptionKey' - Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.
-- * 'inputArtifacts' - The artifact supplied to the job.
-- * 'outputArtifacts' - The output of the job.
-- * 'pipelineContext' - Represents information about a pipeline to a job worker.
mkJobData ::
  JobData
mkJobData =
  JobData'
    { continuationToken = Lude.Nothing,
      outputArtifacts = Lude.Nothing,
      artifactCredentials = Lude.Nothing,
      pipelineContext = Lude.Nothing,
      encryptionKey = Lude.Nothing,
      actionTypeId = Lude.Nothing,
      inputArtifacts = Lude.Nothing,
      actionConfiguration = Lude.Nothing
    }

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, required by a job to continue the job asynchronously.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdContinuationToken :: Lens.Lens' JobData (Lude.Maybe Lude.Text)
jdContinuationToken = Lens.lens (continuationToken :: JobData -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: JobData)
{-# DEPRECATED jdContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The output of the job.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdOutputArtifacts :: Lens.Lens' JobData (Lude.Maybe [Artifact])
jdOutputArtifacts = Lens.lens (outputArtifacts :: JobData -> Lude.Maybe [Artifact]) (\s a -> s {outputArtifacts = a} :: JobData)
{-# DEPRECATED jdOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifacts for the pipeline in AWS CodePipeline.
--
-- /Note:/ Consider using 'artifactCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdArtifactCredentials :: Lens.Lens' JobData (Lude.Maybe AWSSessionCredentials)
jdArtifactCredentials = Lens.lens (artifactCredentials :: JobData -> Lude.Maybe AWSSessionCredentials) (\s a -> s {artifactCredentials = a} :: JobData)
{-# DEPRECATED jdArtifactCredentials "Use generic-lens or generic-optics with 'artifactCredentials' instead." #-}

-- | Represents information about a pipeline to a job worker.
--
-- /Note:/ Consider using 'pipelineContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdPipelineContext :: Lens.Lens' JobData (Lude.Maybe PipelineContext)
jdPipelineContext = Lens.lens (pipelineContext :: JobData -> Lude.Maybe PipelineContext) (\s a -> s {pipelineContext = a} :: JobData)
{-# DEPRECATED jdPipelineContext "Use generic-lens or generic-optics with 'pipelineContext' instead." #-}

-- | Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdEncryptionKey :: Lens.Lens' JobData (Lude.Maybe EncryptionKey)
jdEncryptionKey = Lens.lens (encryptionKey :: JobData -> Lude.Maybe EncryptionKey) (\s a -> s {encryptionKey = a} :: JobData)
{-# DEPRECATED jdEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | Represents information about an action type.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdActionTypeId :: Lens.Lens' JobData (Lude.Maybe ActionTypeId)
jdActionTypeId = Lens.lens (actionTypeId :: JobData -> Lude.Maybe ActionTypeId) (\s a -> s {actionTypeId = a} :: JobData)
{-# DEPRECATED jdActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

-- | The artifact supplied to the job.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdInputArtifacts :: Lens.Lens' JobData (Lude.Maybe [Artifact])
jdInputArtifacts = Lens.lens (inputArtifacts :: JobData -> Lude.Maybe [Artifact]) (\s a -> s {inputArtifacts = a} :: JobData)
{-# DEPRECATED jdInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | Represents information about an action configuration.
--
-- /Note:/ Consider using 'actionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdActionConfiguration :: Lens.Lens' JobData (Lude.Maybe ActionConfiguration)
jdActionConfiguration = Lens.lens (actionConfiguration :: JobData -> Lude.Maybe ActionConfiguration) (\s a -> s {actionConfiguration = a} :: JobData)
{-# DEPRECATED jdActionConfiguration "Use generic-lens or generic-optics with 'actionConfiguration' instead." #-}

instance Lude.FromJSON JobData where
  parseJSON =
    Lude.withObject
      "JobData"
      ( \x ->
          JobData'
            Lude.<$> (x Lude..:? "continuationToken")
            Lude.<*> (x Lude..:? "outputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "artifactCredentials")
            Lude.<*> (x Lude..:? "pipelineContext")
            Lude.<*> (x Lude..:? "encryptionKey")
            Lude.<*> (x Lude..:? "actionTypeId")
            Lude.<*> (x Lude..:? "inputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "actionConfiguration")
      )
