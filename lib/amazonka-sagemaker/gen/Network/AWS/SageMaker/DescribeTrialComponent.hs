{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trials component's properties.
module Network.AWS.SageMaker.DescribeTrialComponent
    (
    -- * Creating a request
      DescribeTrialComponent (..)
    , mkDescribeTrialComponent
    -- ** Request lenses
    , dtcfTrialComponentName

    -- * Destructuring the response
    , DescribeTrialComponentResponse (..)
    , mkDescribeTrialComponentResponse
    -- ** Response lenses
    , dtcrgrsCreatedBy
    , dtcrgrsCreationTime
    , dtcrgrsDisplayName
    , dtcrgrsEndTime
    , dtcrgrsInputArtifacts
    , dtcrgrsLastModifiedBy
    , dtcrgrsLastModifiedTime
    , dtcrgrsMetrics
    , dtcrgrsOutputArtifacts
    , dtcrgrsParameters
    , dtcrgrsSource
    , dtcrgrsStartTime
    , dtcrgrsStatus
    , dtcrgrsTrialComponentArn
    , dtcrgrsTrialComponentName
    , dtcrgrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeTrialComponent' smart constructor.
newtype DescribeTrialComponent = DescribeTrialComponent'
  { trialComponentName :: Types.ExperimentEntityName
    -- ^ The name of the trial component to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrialComponent' value with any optional fields omitted.
mkDescribeTrialComponent
    :: Types.ExperimentEntityName -- ^ 'trialComponentName'
    -> DescribeTrialComponent
mkDescribeTrialComponent trialComponentName
  = DescribeTrialComponent'{trialComponentName}

-- | The name of the trial component to describe.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcfTrialComponentName :: Lens.Lens' DescribeTrialComponent Types.ExperimentEntityName
dtcfTrialComponentName = Lens.field @"trialComponentName"
{-# INLINEABLE dtcfTrialComponentName #-}
{-# DEPRECATED trialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead"  #-}

instance Core.ToQuery DescribeTrialComponent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTrialComponent where
        toHeaders DescribeTrialComponent{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeTrialComponent")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTrialComponent where
        toJSON DescribeTrialComponent{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TrialComponentName" Core..= trialComponentName)])

instance Core.AWSRequest DescribeTrialComponent where
        type Rs DescribeTrialComponent = DescribeTrialComponentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTrialComponentResponse' Core.<$>
                   (x Core..:? "CreatedBy") Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "DisplayName"
                     Core.<*> x Core..:? "EndTime"
                     Core.<*> x Core..:? "InputArtifacts"
                     Core.<*> x Core..:? "LastModifiedBy"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "Metrics"
                     Core.<*> x Core..:? "OutputArtifacts"
                     Core.<*> x Core..:? "Parameters"
                     Core.<*> x Core..:? "Source"
                     Core.<*> x Core..:? "StartTime"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "TrialComponentArn"
                     Core.<*> x Core..:? "TrialComponentName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTrialComponentResponse' smart constructor.
data DescribeTrialComponentResponse = DescribeTrialComponentResponse'
  { createdBy :: Core.Maybe Types.UserContext
    -- ^ Who created the component.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component was created.
  , displayName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component ended.
  , inputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact)
    -- ^ The input artifacts of the component.
  , lastModifiedBy :: Core.Maybe Types.UserContext
    -- ^ Who last modified the component.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component was last modified.
  , metrics :: Core.Maybe [Types.TrialComponentMetricSummary]
    -- ^ The metrics for the component.
  , outputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact)
    -- ^ The output artifacts of the component.
  , parameters :: Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue)
    -- ^ The hyperparameters of the component.
  , source :: Core.Maybe Types.TrialComponentSource
    -- ^ The Amazon Resource Name (ARN) of the source and, optionally, the job type.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component started.
  , status :: Core.Maybe Types.TrialComponentStatus
    -- ^ The status of the component. States include:
--
--
--     * InProgress
--
--
--     * Completed
--
--
--     * Failed
--
--
  , trialComponentArn :: Core.Maybe Types.TrialComponentArn
    -- ^ The Amazon Resource Name (ARN) of the trial component.
  , trialComponentName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the trial component.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTrialComponentResponse' value with any optional fields omitted.
mkDescribeTrialComponentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTrialComponentResponse
mkDescribeTrialComponentResponse responseStatus
  = DescribeTrialComponentResponse'{createdBy = Core.Nothing,
                                    creationTime = Core.Nothing, displayName = Core.Nothing,
                                    endTime = Core.Nothing, inputArtifacts = Core.Nothing,
                                    lastModifiedBy = Core.Nothing, lastModifiedTime = Core.Nothing,
                                    metrics = Core.Nothing, outputArtifacts = Core.Nothing,
                                    parameters = Core.Nothing, source = Core.Nothing,
                                    startTime = Core.Nothing, status = Core.Nothing,
                                    trialComponentArn = Core.Nothing,
                                    trialComponentName = Core.Nothing, responseStatus}

-- | Who created the component.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsCreatedBy :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Types.UserContext)
dtcrgrsCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE dtcrgrsCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsCreationTime :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.NominalDiffTime)
dtcrgrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dtcrgrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsDisplayName :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Types.ExperimentEntityName)
dtcrgrsDisplayName = Lens.field @"displayName"
{-# INLINEABLE dtcrgrsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsEndTime :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.NominalDiffTime)
dtcrgrsEndTime = Lens.field @"endTime"
{-# INLINEABLE dtcrgrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The input artifacts of the component.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsInputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
dtcrgrsInputArtifacts = Lens.field @"inputArtifacts"
{-# INLINEABLE dtcrgrsInputArtifacts #-}
{-# DEPRECATED inputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead"  #-}

-- | Who last modified the component.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsLastModifiedBy :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Types.UserContext)
dtcrgrsLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE dtcrgrsLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | When the component was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsLastModifiedTime :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.NominalDiffTime)
dtcrgrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dtcrgrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The metrics for the component.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsMetrics :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe [Types.TrialComponentMetricSummary])
dtcrgrsMetrics = Lens.field @"metrics"
{-# INLINEABLE dtcrgrsMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | The output artifacts of the component.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsOutputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
dtcrgrsOutputArtifacts = Lens.field @"outputArtifacts"
{-# INLINEABLE dtcrgrsOutputArtifacts #-}
{-# DEPRECATED outputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead"  #-}

-- | The hyperparameters of the component.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsParameters :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue))
dtcrgrsParameters = Lens.field @"parameters"
{-# INLINEABLE dtcrgrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsSource :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Types.TrialComponentSource)
dtcrgrsSource = Lens.field @"source"
{-# INLINEABLE dtcrgrsSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsStartTime :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.NominalDiffTime)
dtcrgrsStartTime = Lens.field @"startTime"
{-# INLINEABLE dtcrgrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the component. States include:
--
--
--     * InProgress
--
--
--     * Completed
--
--
--     * Failed
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsStatus :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Types.TrialComponentStatus)
dtcrgrsStatus = Lens.field @"status"
{-# INLINEABLE dtcrgrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsTrialComponentArn :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Types.TrialComponentArn)
dtcrgrsTrialComponentArn = Lens.field @"trialComponentArn"
{-# INLINEABLE dtcrgrsTrialComponentArn #-}
{-# DEPRECATED trialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead"  #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsTrialComponentName :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Types.ExperimentEntityName)
dtcrgrsTrialComponentName = Lens.field @"trialComponentName"
{-# INLINEABLE dtcrgrsTrialComponentName #-}
{-# DEPRECATED trialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrgrsResponseStatus :: Lens.Lens' DescribeTrialComponentResponse Core.Int
dtcrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtcrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
