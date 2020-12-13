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
    jdfStatus,
    jdfJobDefinitionName,
    jdfRetryStrategy,
    jdfJobDefinitionARN,
    jdfParameters,
    jdfType,
    jdfTimeout,
    jdfRevision,
    jdfContainerProperties,
    jdfNodeProperties,
    jdfTags,
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
  { -- | The status of the job definition.
    status :: Lude.Maybe Lude.Text,
    -- | The name of the job definition.
    jobDefinitionName :: Lude.Text,
    -- | The retry strategy to use for failed jobs that are submitted with this job definition.
    retryStrategy :: Lude.Maybe RetryStrategy,
    -- | The Amazon Resource Name (ARN) for the job definition.
    jobDefinitionARN :: Lude.Text,
    -- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The type of job definition.
    type' :: Lude.Text,
    -- | The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
    timeout :: Lude.Maybe JobTimeout,
    -- | The revision of the job definition.
    revision :: Lude.Int,
    -- | An object with various properties specific to container-based jobs.
    containerProperties :: Lude.Maybe ContainerProperties,
    -- | An object with various properties specific to multi-node parallel jobs.
    nodeProperties :: Lude.Maybe NodeProperties,
    -- | The tags applied to the job definition.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobDefinition' with the minimum fields required to make a request.
--
-- * 'status' - The status of the job definition.
-- * 'jobDefinitionName' - The name of the job definition.
-- * 'retryStrategy' - The retry strategy to use for failed jobs that are submitted with this job definition.
-- * 'jobDefinitionARN' - The Amazon Resource Name (ARN) for the job definition.
-- * 'parameters' - Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
-- * 'type'' - The type of job definition.
-- * 'timeout' - The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
-- * 'revision' - The revision of the job definition.
-- * 'containerProperties' - An object with various properties specific to container-based jobs.
-- * 'nodeProperties' - An object with various properties specific to multi-node parallel jobs.
-- * 'tags' - The tags applied to the job definition.
mkJobDefinition ::
  -- | 'jobDefinitionName'
  Lude.Text ->
  -- | 'jobDefinitionARN'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  -- | 'revision'
  Lude.Int ->
  JobDefinition
mkJobDefinition
  pJobDefinitionName_
  pJobDefinitionARN_
  pType_
  pRevision_ =
    JobDefinition'
      { status = Lude.Nothing,
        jobDefinitionName = pJobDefinitionName_,
        retryStrategy = Lude.Nothing,
        jobDefinitionARN = pJobDefinitionARN_,
        parameters = Lude.Nothing,
        type' = pType_,
        timeout = Lude.Nothing,
        revision = pRevision_,
        containerProperties = Lude.Nothing,
        nodeProperties = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | The status of the job definition.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfStatus :: Lens.Lens' JobDefinition (Lude.Maybe Lude.Text)
jdfStatus = Lens.lens (status :: JobDefinition -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: JobDefinition)
{-# DEPRECATED jdfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfJobDefinitionName :: Lens.Lens' JobDefinition Lude.Text
jdfJobDefinitionName = Lens.lens (jobDefinitionName :: JobDefinition -> Lude.Text) (\s a -> s {jobDefinitionName = a} :: JobDefinition)
{-# DEPRECATED jdfJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | The retry strategy to use for failed jobs that are submitted with this job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfRetryStrategy :: Lens.Lens' JobDefinition (Lude.Maybe RetryStrategy)
jdfRetryStrategy = Lens.lens (retryStrategy :: JobDefinition -> Lude.Maybe RetryStrategy) (\s a -> s {retryStrategy = a} :: JobDefinition)
{-# DEPRECATED jdfRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | The Amazon Resource Name (ARN) for the job definition.
--
-- /Note:/ Consider using 'jobDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfJobDefinitionARN :: Lens.Lens' JobDefinition Lude.Text
jdfJobDefinitionARN = Lens.lens (jobDefinitionARN :: JobDefinition -> Lude.Text) (\s a -> s {jobDefinitionARN = a} :: JobDefinition)
{-# DEPRECATED jdfJobDefinitionARN "Use generic-lens or generic-optics with 'jobDefinitionARN' instead." #-}

-- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfParameters :: Lens.Lens' JobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jdfParameters = Lens.lens (parameters :: JobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: JobDefinition)
{-# DEPRECATED jdfParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The type of job definition.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfType :: Lens.Lens' JobDefinition Lude.Text
jdfType = Lens.lens (type' :: JobDefinition -> Lude.Text) (\s a -> s {type' = a} :: JobDefinition)
{-# DEPRECATED jdfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfTimeout :: Lens.Lens' JobDefinition (Lude.Maybe JobTimeout)
jdfTimeout = Lens.lens (timeout :: JobDefinition -> Lude.Maybe JobTimeout) (\s a -> s {timeout = a} :: JobDefinition)
{-# DEPRECATED jdfTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The revision of the job definition.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfRevision :: Lens.Lens' JobDefinition Lude.Int
jdfRevision = Lens.lens (revision :: JobDefinition -> Lude.Int) (\s a -> s {revision = a} :: JobDefinition)
{-# DEPRECATED jdfRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | An object with various properties specific to container-based jobs.
--
-- /Note:/ Consider using 'containerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfContainerProperties :: Lens.Lens' JobDefinition (Lude.Maybe ContainerProperties)
jdfContainerProperties = Lens.lens (containerProperties :: JobDefinition -> Lude.Maybe ContainerProperties) (\s a -> s {containerProperties = a} :: JobDefinition)
{-# DEPRECATED jdfContainerProperties "Use generic-lens or generic-optics with 'containerProperties' instead." #-}

-- | An object with various properties specific to multi-node parallel jobs.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfNodeProperties :: Lens.Lens' JobDefinition (Lude.Maybe NodeProperties)
jdfNodeProperties = Lens.lens (nodeProperties :: JobDefinition -> Lude.Maybe NodeProperties) (\s a -> s {nodeProperties = a} :: JobDefinition)
{-# DEPRECATED jdfNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | The tags applied to the job definition.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfTags :: Lens.Lens' JobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jdfTags = Lens.lens (tags :: JobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: JobDefinition)
{-# DEPRECATED jdfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON JobDefinition where
  parseJSON =
    Lude.withObject
      "JobDefinition"
      ( \x ->
          JobDefinition'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..: "jobDefinitionName")
            Lude.<*> (x Lude..:? "retryStrategy")
            Lude.<*> (x Lude..: "jobDefinitionArn")
            Lude.<*> (x Lude..:? "parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..:? "timeout")
            Lude.<*> (x Lude..: "revision")
            Lude.<*> (x Lude..:? "containerProperties")
            Lude.<*> (x Lude..:? "nodeProperties")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
