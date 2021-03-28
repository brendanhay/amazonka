{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.JobDefinition
  ( JobDefinition (..)
  -- * Smart constructor
  , mkJobDefinition
  -- * Lenses
  , jdfJobDefinitionName
  , jdfJobDefinitionArn
  , jdfRevision
  , jdfType
  , jdfContainerProperties
  , jdfNodeProperties
  , jdfParameters
  , jdfRetryStrategy
  , jdfStatus
  , jdfTags
  , jdfTimeout
  ) where

import qualified Network.AWS.Batch.Types.ContainerProperties as Types
import qualified Network.AWS.Batch.Types.JobTimeout as Types
import qualified Network.AWS.Batch.Types.NodeProperties as Types
import qualified Network.AWS.Batch.Types.RetryStrategy as Types
import qualified Network.AWS.Batch.Types.TagKey as Types
import qualified Network.AWS.Batch.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an AWS Batch job definition.
--
-- /See:/ 'mkJobDefinition' smart constructor.
data JobDefinition = JobDefinition'
  { jobDefinitionName :: Core.Text
    -- ^ The name of the job definition.
  , jobDefinitionArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) for the job definition.
  , revision :: Core.Int
    -- ^ The revision of the job definition.
  , type' :: Core.Text
    -- ^ The type of job definition.
  , containerProperties :: Core.Maybe Types.ContainerProperties
    -- ^ An object with various properties specific to container-based jobs.
  , nodeProperties :: Core.Maybe Types.NodeProperties
    -- ^ An object with various properties specific to multi-node parallel jobs.
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
  , retryStrategy :: Core.Maybe Types.RetryStrategy
    -- ^ The retry strategy to use for failed jobs that are submitted with this job definition.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the job definition.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags applied to the job definition.
  , timeout :: Core.Maybe Types.JobTimeout
    -- ^ The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobDefinition' value with any optional fields omitted.
mkJobDefinition
    :: Core.Text -- ^ 'jobDefinitionName'
    -> Core.Text -- ^ 'jobDefinitionArn'
    -> Core.Int -- ^ 'revision'
    -> Core.Text -- ^ 'type\''
    -> JobDefinition
mkJobDefinition jobDefinitionName jobDefinitionArn revision type'
  = JobDefinition'{jobDefinitionName, jobDefinitionArn, revision,
                   type', containerProperties = Core.Nothing,
                   nodeProperties = Core.Nothing, parameters = Core.Nothing,
                   retryStrategy = Core.Nothing, status = Core.Nothing,
                   tags = Core.Nothing, timeout = Core.Nothing}

-- | The name of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfJobDefinitionName :: Lens.Lens' JobDefinition Core.Text
jdfJobDefinitionName = Lens.field @"jobDefinitionName"
{-# INLINEABLE jdfJobDefinitionName #-}
{-# DEPRECATED jobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead"  #-}

-- | The Amazon Resource Name (ARN) for the job definition.
--
-- /Note:/ Consider using 'jobDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfJobDefinitionArn :: Lens.Lens' JobDefinition Core.Text
jdfJobDefinitionArn = Lens.field @"jobDefinitionArn"
{-# INLINEABLE jdfJobDefinitionArn #-}
{-# DEPRECATED jobDefinitionArn "Use generic-lens or generic-optics with 'jobDefinitionArn' instead"  #-}

-- | The revision of the job definition.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfRevision :: Lens.Lens' JobDefinition Core.Int
jdfRevision = Lens.field @"revision"
{-# INLINEABLE jdfRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | The type of job definition.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfType :: Lens.Lens' JobDefinition Core.Text
jdfType = Lens.field @"type'"
{-# INLINEABLE jdfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | An object with various properties specific to container-based jobs.
--
-- /Note:/ Consider using 'containerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfContainerProperties :: Lens.Lens' JobDefinition (Core.Maybe Types.ContainerProperties)
jdfContainerProperties = Lens.field @"containerProperties"
{-# INLINEABLE jdfContainerProperties #-}
{-# DEPRECATED containerProperties "Use generic-lens or generic-optics with 'containerProperties' instead"  #-}

-- | An object with various properties specific to multi-node parallel jobs.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfNodeProperties :: Lens.Lens' JobDefinition (Core.Maybe Types.NodeProperties)
jdfNodeProperties = Lens.field @"nodeProperties"
{-# INLINEABLE jdfNodeProperties #-}
{-# DEPRECATED nodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead"  #-}

-- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfParameters :: Lens.Lens' JobDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
jdfParameters = Lens.field @"parameters"
{-# INLINEABLE jdfParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The retry strategy to use for failed jobs that are submitted with this job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfRetryStrategy :: Lens.Lens' JobDefinition (Core.Maybe Types.RetryStrategy)
jdfRetryStrategy = Lens.field @"retryStrategy"
{-# INLINEABLE jdfRetryStrategy #-}
{-# DEPRECATED retryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead"  #-}

-- | The status of the job definition.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfStatus :: Lens.Lens' JobDefinition (Core.Maybe Core.Text)
jdfStatus = Lens.field @"status"
{-# INLINEABLE jdfStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The tags applied to the job definition.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfTags :: Lens.Lens' JobDefinition (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
jdfTags = Lens.field @"tags"
{-# INLINEABLE jdfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdfTimeout :: Lens.Lens' JobDefinition (Core.Maybe Types.JobTimeout)
jdfTimeout = Lens.field @"timeout"
{-# INLINEABLE jdfTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

instance Core.FromJSON JobDefinition where
        parseJSON
          = Core.withObject "JobDefinition" Core.$
              \ x ->
                JobDefinition' Core.<$>
                  (x Core..: "jobDefinitionName") Core.<*>
                    x Core..: "jobDefinitionArn"
                    Core.<*> x Core..: "revision"
                    Core.<*> x Core..: "type"
                    Core.<*> x Core..:? "containerProperties"
                    Core.<*> x Core..:? "nodeProperties"
                    Core.<*> x Core..:? "parameters"
                    Core.<*> x Core..:? "retryStrategy"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "timeout"
