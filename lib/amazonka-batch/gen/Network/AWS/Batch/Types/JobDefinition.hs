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
    jdfJobDefinitionName,
    jdfJobDefinitionArn,
    jdfRevision,
    jdfType,
    jdfContainerProperties,
    jdfNodeProperties,
    jdfParameters,
    jdfRetryStrategy,
    jdfStatus,
    jdfTags,
    jdfTimeout,
  )
where

import qualified Network.AWS.Batch.Types.ContainerProperties as Types
import qualified Network.AWS.Batch.Types.JobTimeout as Types
import qualified Network.AWS.Batch.Types.NodeProperties as Types
import qualified Network.AWS.Batch.Types.RetryStrategy as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Batch.Types.TagKey as Types
import qualified Network.AWS.Batch.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an AWS Batch job definition.
--
-- /See:/ 'mkJobDefinition' smart constructor.
data JobDefinition = JobDefinition'
  { -- | The name of the job definition.
    jobDefinitionName :: Types.String,
    -- | The Amazon Resource Name (ARN) for the job definition.
    jobDefinitionArn :: Types.String,
    -- | The revision of the job definition.
    revision :: Core.Int,
    -- | The type of job definition.
    type' :: Types.String,
    -- | An object with various properties specific to container-based jobs.
    containerProperties :: Core.Maybe Types.ContainerProperties,
    -- | An object with various properties specific to multi-node parallel jobs.
    nodeProperties :: Core.Maybe Types.NodeProperties,
    -- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
    parameters :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The retry strategy to use for failed jobs that are submitted with this job definition.
    retryStrategy :: Core.Maybe Types.RetryStrategy,
    -- | The status of the job definition.
    status :: Core.Maybe Types.String,
    -- | The tags applied to the job definition.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
    timeout :: Core.Maybe Types.JobTimeout
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobDefinition' value with any optional fields omitted.
mkJobDefinition ::
  -- | 'jobDefinitionName'
  Types.String ->
  -- | 'jobDefinitionArn'
  Types.String ->
  -- | 'revision'
  Core.Int ->
  -- | 'type\''
  Types.String ->
  JobDefinition
mkJobDefinition jobDefinitionName jobDefinitionArn revision type' =
  JobDefinition'
    { jobDefinitionName,
      jobDefinitionArn,
      revision,
      type',
      containerProperties = Core.Nothing,
      nodeProperties = Core.Nothing,
      parameters = Core.Nothing,
      retryStrategy = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing,
      timeout = Core.Nothing
    }

-- | The name of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfJobDefinitionName :: Lens.Lens' JobDefinition Types.String
jdfJobDefinitionName = Lens.field @"jobDefinitionName"
{-# DEPRECATED jdfJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | The Amazon Resource Name (ARN) for the job definition.
--
-- /Note:/ Consider using 'jobDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfJobDefinitionArn :: Lens.Lens' JobDefinition Types.String
jdfJobDefinitionArn = Lens.field @"jobDefinitionArn"
{-# DEPRECATED jdfJobDefinitionArn "Use generic-lens or generic-optics with 'jobDefinitionArn' instead." #-}

-- | The revision of the job definition.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfRevision :: Lens.Lens' JobDefinition Core.Int
jdfRevision = Lens.field @"revision"
{-# DEPRECATED jdfRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The type of job definition.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfType :: Lens.Lens' JobDefinition Types.String
jdfType = Lens.field @"type'"
{-# DEPRECATED jdfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An object with various properties specific to container-based jobs.
--
-- /Note:/ Consider using 'containerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfContainerProperties :: Lens.Lens' JobDefinition (Core.Maybe Types.ContainerProperties)
jdfContainerProperties = Lens.field @"containerProperties"
{-# DEPRECATED jdfContainerProperties "Use generic-lens or generic-optics with 'containerProperties' instead." #-}

-- | An object with various properties specific to multi-node parallel jobs.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfNodeProperties :: Lens.Lens' JobDefinition (Core.Maybe Types.NodeProperties)
jdfNodeProperties = Lens.field @"nodeProperties"
{-# DEPRECATED jdfNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfParameters :: Lens.Lens' JobDefinition (Core.Maybe (Core.HashMap Types.String Types.String))
jdfParameters = Lens.field @"parameters"
{-# DEPRECATED jdfParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The retry strategy to use for failed jobs that are submitted with this job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfRetryStrategy :: Lens.Lens' JobDefinition (Core.Maybe Types.RetryStrategy)
jdfRetryStrategy = Lens.field @"retryStrategy"
{-# DEPRECATED jdfRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | The status of the job definition.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfStatus :: Lens.Lens' JobDefinition (Core.Maybe Types.String)
jdfStatus = Lens.field @"status"
{-# DEPRECATED jdfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The tags applied to the job definition.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfTags :: Lens.Lens' JobDefinition (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
jdfTags = Lens.field @"tags"
{-# DEPRECATED jdfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfTimeout :: Lens.Lens' JobDefinition (Core.Maybe Types.JobTimeout)
jdfTimeout = Lens.field @"timeout"
{-# DEPRECATED jdfTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

instance Core.FromJSON JobDefinition where
  parseJSON =
    Core.withObject "JobDefinition" Core.$
      \x ->
        JobDefinition'
          Core.<$> (x Core..: "jobDefinitionName")
          Core.<*> (x Core..: "jobDefinitionArn")
          Core.<*> (x Core..: "revision")
          Core.<*> (x Core..: "type")
          Core.<*> (x Core..:? "containerProperties")
          Core.<*> (x Core..:? "nodeProperties")
          Core.<*> (x Core..:? "parameters")
          Core.<*> (x Core..:? "retryStrategy")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "timeout")
