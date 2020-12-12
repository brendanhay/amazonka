{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDefinition
  ( JobDefinition (..),

    -- * Smart constructor
    mkJobDefinition,

    -- * Lenses
    jobStatus,
    jobRetryStrategy,
    jobParameters,
    jobTimeout,
    jobContainerProperties,
    jobNodeProperties,
    jobTags,
    jobJobDefinitionName,
    jobJobDefinitionARN,
    jobRevision,
    jobType,
  )
where

import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.RetryStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an AWS Batch job definition.
--
-- /See:/ 'mkJobDefinition' smart constructor.
data JobDefinition = JobDefinition'
  { status :: Lude.Maybe Lude.Text,
    retryStrategy :: Lude.Maybe RetryStrategy,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    timeout :: Lude.Maybe JobTimeout,
    containerProperties :: Lude.Maybe ContainerProperties,
    nodeProperties :: Lude.Maybe NodeProperties,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    jobDefinitionName :: Lude.Text,
    jobDefinitionARN :: Lude.Text,
    revision :: Lude.Int,
    type' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobDefinition' with the minimum fields required to make a request.
--
-- * 'containerProperties' - An object with various properties specific to container-based jobs.
-- * 'jobDefinitionARN' - The Amazon Resource Name (ARN) for the job definition.
-- * 'jobDefinitionName' - The name of the job definition.
-- * 'nodeProperties' - An object with various properties specific to multi-node parallel jobs.
-- * 'parameters' - Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
-- * 'retryStrategy' - The retry strategy to use for failed jobs that are submitted with this job definition.
-- * 'revision' - The revision of the job definition.
-- * 'status' - The status of the job definition.
-- * 'tags' - The tags applied to the job definition.
-- * 'timeout' - The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
-- * 'type'' - The type of job definition.
mkJobDefinition ::
  -- | 'jobDefinitionName'
  Lude.Text ->
  -- | 'jobDefinitionARN'
  Lude.Text ->
  -- | 'revision'
  Lude.Int ->
  -- | 'type''
  Lude.Text ->
  JobDefinition
mkJobDefinition
  pJobDefinitionName_
  pJobDefinitionARN_
  pRevision_
  pType_ =
    JobDefinition'
      { status = Lude.Nothing,
        retryStrategy = Lude.Nothing,
        parameters = Lude.Nothing,
        timeout = Lude.Nothing,
        containerProperties = Lude.Nothing,
        nodeProperties = Lude.Nothing,
        tags = Lude.Nothing,
        jobDefinitionName = pJobDefinitionName_,
        jobDefinitionARN = pJobDefinitionARN_,
        revision = pRevision_,
        type' = pType_
      }

-- | The status of the job definition.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobStatus :: Lens.Lens' JobDefinition (Lude.Maybe Lude.Text)
jobStatus = Lens.lens (status :: JobDefinition -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: JobDefinition)
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The retry strategy to use for failed jobs that are submitted with this job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobRetryStrategy :: Lens.Lens' JobDefinition (Lude.Maybe RetryStrategy)
jobRetryStrategy = Lens.lens (retryStrategy :: JobDefinition -> Lude.Maybe RetryStrategy) (\s a -> s {retryStrategy = a} :: JobDefinition)
{-# DEPRECATED jobRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobParameters :: Lens.Lens' JobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jobParameters = Lens.lens (parameters :: JobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: JobDefinition)
{-# DEPRECATED jobParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobTimeout :: Lens.Lens' JobDefinition (Lude.Maybe JobTimeout)
jobTimeout = Lens.lens (timeout :: JobDefinition -> Lude.Maybe JobTimeout) (\s a -> s {timeout = a} :: JobDefinition)
{-# DEPRECATED jobTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | An object with various properties specific to container-based jobs.
--
-- /Note:/ Consider using 'containerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobContainerProperties :: Lens.Lens' JobDefinition (Lude.Maybe ContainerProperties)
jobContainerProperties = Lens.lens (containerProperties :: JobDefinition -> Lude.Maybe ContainerProperties) (\s a -> s {containerProperties = a} :: JobDefinition)
{-# DEPRECATED jobContainerProperties "Use generic-lens or generic-optics with 'containerProperties' instead." #-}

-- | An object with various properties specific to multi-node parallel jobs.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobNodeProperties :: Lens.Lens' JobDefinition (Lude.Maybe NodeProperties)
jobNodeProperties = Lens.lens (nodeProperties :: JobDefinition -> Lude.Maybe NodeProperties) (\s a -> s {nodeProperties = a} :: JobDefinition)
{-# DEPRECATED jobNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | The tags applied to the job definition.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobTags :: Lens.Lens' JobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jobTags = Lens.lens (tags :: JobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: JobDefinition)
{-# DEPRECATED jobTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobJobDefinitionName :: Lens.Lens' JobDefinition Lude.Text
jobJobDefinitionName = Lens.lens (jobDefinitionName :: JobDefinition -> Lude.Text) (\s a -> s {jobDefinitionName = a} :: JobDefinition)
{-# DEPRECATED jobJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | The Amazon Resource Name (ARN) for the job definition.
--
-- /Note:/ Consider using 'jobDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobJobDefinitionARN :: Lens.Lens' JobDefinition Lude.Text
jobJobDefinitionARN = Lens.lens (jobDefinitionARN :: JobDefinition -> Lude.Text) (\s a -> s {jobDefinitionARN = a} :: JobDefinition)
{-# DEPRECATED jobJobDefinitionARN "Use generic-lens or generic-optics with 'jobDefinitionARN' instead." #-}

-- | The revision of the job definition.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobRevision :: Lens.Lens' JobDefinition Lude.Int
jobRevision = Lens.lens (revision :: JobDefinition -> Lude.Int) (\s a -> s {revision = a} :: JobDefinition)
{-# DEPRECATED jobRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The type of job definition.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jobType :: Lens.Lens' JobDefinition Lude.Text
jobType = Lens.lens (type' :: JobDefinition -> Lude.Text) (\s a -> s {type' = a} :: JobDefinition)
{-# DEPRECATED jobType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON JobDefinition where
  parseJSON =
    Lude.withObject
      "JobDefinition"
      ( \x ->
          JobDefinition'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "retryStrategy")
            Lude.<*> (x Lude..:? "parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "timeout")
            Lude.<*> (x Lude..:? "containerProperties")
            Lude.<*> (x Lude..:? "nodeProperties")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "jobDefinitionName")
            Lude.<*> (x Lude..: "jobDefinitionArn")
            Lude.<*> (x Lude..: "revision")
            Lude.<*> (x Lude..: "type")
      )
