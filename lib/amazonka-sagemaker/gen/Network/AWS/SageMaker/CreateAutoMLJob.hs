{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Autopilot job.
--
-- Find the best performing model after you run an Autopilot job by calling . Deploy that model by following the steps described in <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html Step 6.1: Deploy the Model to Amazon SageMaker Hosting Services> .
-- For information about how to use Autopilot, see <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development.html Automate Model Development with Amazon SageMaker Autopilot> .
module Network.AWS.SageMaker.CreateAutoMLJob
    (
    -- * Creating a request
      CreateAutoMLJob (..)
    , mkCreateAutoMLJob
    -- ** Request lenses
    , camljAutoMLJobName
    , camljInputDataConfig
    , camljOutputDataConfig
    , camljRoleArn
    , camljAutoMLJobConfig
    , camljAutoMLJobObjective
    , camljGenerateCandidateDefinitionsOnly
    , camljProblemType
    , camljTags

    -- * Destructuring the response
    , CreateAutoMLJobResponse (..)
    , mkCreateAutoMLJobResponse
    -- ** Response lenses
    , camljrrsAutoMLJobArn
    , camljrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateAutoMLJob' smart constructor.
data CreateAutoMLJob = CreateAutoMLJob'
  { autoMLJobName :: Types.AutoMLJobName
    -- ^ Identifies an Autopilot job. Must be unique to your account and is case-insensitive.
  , inputDataConfig :: Core.NonEmpty Types.AutoMLChannel
    -- ^ Similar to InputDataConfig supported by Tuning. Format(s) supported: CSV. Minimum of 500 rows.
  , outputDataConfig :: Types.AutoMLOutputDataConfig
    -- ^ Similar to OutputDataConfig supported by Tuning. Format(s) supported: CSV.
  , roleArn :: Types.RoleArn
    -- ^ The ARN of the role that is used to access the data.
  , autoMLJobConfig :: Core.Maybe Types.AutoMLJobConfig
    -- ^ Contains CompletionCriteria and SecurityConfig.
  , autoMLJobObjective :: Core.Maybe Types.AutoMLJobObjective
    -- ^ Defines the objective of a an AutoML job. You provide a 'AutoMLJobObjective$MetricName' and Autopilot infers whether to minimize or maximize it. If a metric is not specified, the most commonly used ObjectiveMetric for problem type is automaically selected.
  , generateCandidateDefinitionsOnly :: Core.Maybe Core.Bool
    -- ^ Generates possible candidates without training a model. A candidate is a combination of data preprocessors, algorithms, and algorithm parameter settings.
  , problemType :: Core.Maybe Types.ProblemType
    -- ^ Defines the kind of preprocessing and algorithms intended for the candidates. Options include: BinaryClassification, MulticlassClassification, and Regression.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Each tag consists of a key and an optional value. Tag keys must be unique per resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAutoMLJob' value with any optional fields omitted.
mkCreateAutoMLJob
    :: Types.AutoMLJobName -- ^ 'autoMLJobName'
    -> Core.NonEmpty Types.AutoMLChannel -- ^ 'inputDataConfig'
    -> Types.AutoMLOutputDataConfig -- ^ 'outputDataConfig'
    -> Types.RoleArn -- ^ 'roleArn'
    -> CreateAutoMLJob
mkCreateAutoMLJob autoMLJobName inputDataConfig outputDataConfig
  roleArn
  = CreateAutoMLJob'{autoMLJobName, inputDataConfig,
                     outputDataConfig, roleArn, autoMLJobConfig = Core.Nothing,
                     autoMLJobObjective = Core.Nothing,
                     generateCandidateDefinitionsOnly = Core.Nothing,
                     problemType = Core.Nothing, tags = Core.Nothing}

-- | Identifies an Autopilot job. Must be unique to your account and is case-insensitive.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljAutoMLJobName :: Lens.Lens' CreateAutoMLJob Types.AutoMLJobName
camljAutoMLJobName = Lens.field @"autoMLJobName"
{-# INLINEABLE camljAutoMLJobName #-}
{-# DEPRECATED autoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead"  #-}

-- | Similar to InputDataConfig supported by Tuning. Format(s) supported: CSV. Minimum of 500 rows.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljInputDataConfig :: Lens.Lens' CreateAutoMLJob (Core.NonEmpty Types.AutoMLChannel)
camljInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE camljInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Similar to OutputDataConfig supported by Tuning. Format(s) supported: CSV.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljOutputDataConfig :: Lens.Lens' CreateAutoMLJob Types.AutoMLOutputDataConfig
camljOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE camljOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The ARN of the role that is used to access the data.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljRoleArn :: Lens.Lens' CreateAutoMLJob Types.RoleArn
camljRoleArn = Lens.field @"roleArn"
{-# INLINEABLE camljRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Contains CompletionCriteria and SecurityConfig.
--
-- /Note:/ Consider using 'autoMLJobConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljAutoMLJobConfig :: Lens.Lens' CreateAutoMLJob (Core.Maybe Types.AutoMLJobConfig)
camljAutoMLJobConfig = Lens.field @"autoMLJobConfig"
{-# INLINEABLE camljAutoMLJobConfig #-}
{-# DEPRECATED autoMLJobConfig "Use generic-lens or generic-optics with 'autoMLJobConfig' instead"  #-}

-- | Defines the objective of a an AutoML job. You provide a 'AutoMLJobObjective$MetricName' and Autopilot infers whether to minimize or maximize it. If a metric is not specified, the most commonly used ObjectiveMetric for problem type is automaically selected.
--
-- /Note:/ Consider using 'autoMLJobObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljAutoMLJobObjective :: Lens.Lens' CreateAutoMLJob (Core.Maybe Types.AutoMLJobObjective)
camljAutoMLJobObjective = Lens.field @"autoMLJobObjective"
{-# INLINEABLE camljAutoMLJobObjective #-}
{-# DEPRECATED autoMLJobObjective "Use generic-lens or generic-optics with 'autoMLJobObjective' instead"  #-}

-- | Generates possible candidates without training a model. A candidate is a combination of data preprocessors, algorithms, and algorithm parameter settings.
--
-- /Note:/ Consider using 'generateCandidateDefinitionsOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljGenerateCandidateDefinitionsOnly :: Lens.Lens' CreateAutoMLJob (Core.Maybe Core.Bool)
camljGenerateCandidateDefinitionsOnly = Lens.field @"generateCandidateDefinitionsOnly"
{-# INLINEABLE camljGenerateCandidateDefinitionsOnly #-}
{-# DEPRECATED generateCandidateDefinitionsOnly "Use generic-lens or generic-optics with 'generateCandidateDefinitionsOnly' instead"  #-}

-- | Defines the kind of preprocessing and algorithms intended for the candidates. Options include: BinaryClassification, MulticlassClassification, and Regression.
--
-- /Note:/ Consider using 'problemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljProblemType :: Lens.Lens' CreateAutoMLJob (Core.Maybe Types.ProblemType)
camljProblemType = Lens.field @"problemType"
{-# INLINEABLE camljProblemType #-}
{-# DEPRECATED problemType "Use generic-lens or generic-optics with 'problemType' instead"  #-}

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljTags :: Lens.Lens' CreateAutoMLJob (Core.Maybe [Types.Tag])
camljTags = Lens.field @"tags"
{-# INLINEABLE camljTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateAutoMLJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAutoMLJob where
        toHeaders CreateAutoMLJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateAutoMLJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateAutoMLJob where
        toJSON CreateAutoMLJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AutoMLJobName" Core..= autoMLJobName),
                  Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("OutputDataConfig" Core..= outputDataConfig),
                  Core.Just ("RoleArn" Core..= roleArn),
                  ("AutoMLJobConfig" Core..=) Core.<$> autoMLJobConfig,
                  ("AutoMLJobObjective" Core..=) Core.<$> autoMLJobObjective,
                  ("GenerateCandidateDefinitionsOnly" Core..=) Core.<$>
                    generateCandidateDefinitionsOnly,
                  ("ProblemType" Core..=) Core.<$> problemType,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateAutoMLJob where
        type Rs CreateAutoMLJob = CreateAutoMLJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAutoMLJobResponse' Core.<$>
                   (x Core..: "AutoMLJobArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAutoMLJobResponse' smart constructor.
data CreateAutoMLJobResponse = CreateAutoMLJobResponse'
  { autoMLJobArn :: Types.AutoMLJobArn
    -- ^ When a job is created, it is assigned a unique ARN.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAutoMLJobResponse' value with any optional fields omitted.
mkCreateAutoMLJobResponse
    :: Types.AutoMLJobArn -- ^ 'autoMLJobArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateAutoMLJobResponse
mkCreateAutoMLJobResponse autoMLJobArn responseStatus
  = CreateAutoMLJobResponse'{autoMLJobArn, responseStatus}

-- | When a job is created, it is assigned a unique ARN.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljrrsAutoMLJobArn :: Lens.Lens' CreateAutoMLJobResponse Types.AutoMLJobArn
camljrrsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# INLINEABLE camljrrsAutoMLJobArn #-}
{-# DEPRECATED autoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljrrsResponseStatus :: Lens.Lens' CreateAutoMLJobResponse Core.Int
camljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE camljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
