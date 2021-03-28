{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutActionRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS CodePipeline about new revisions to a source.
module Network.AWS.CodePipeline.PutActionRevision
    (
    -- * Creating a request
      PutActionRevision (..)
    , mkPutActionRevision
    -- ** Request lenses
    , pPipelineName
    , pStageName
    , pActionName
    , pActionRevision

    -- * Destructuring the response
    , PutActionRevisionResponse (..)
    , mkPutActionRevisionResponse
    -- ** Response lenses
    , prsNewRevision
    , prsPipelineExecutionId
    , prsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutActionRevision@ action.
--
-- /See:/ 'mkPutActionRevision' smart constructor.
data PutActionRevision = PutActionRevision'
  { pipelineName :: Types.PipelineName
    -- ^ The name of the pipeline that starts processing the revision to the source.
  , stageName :: Types.StageName
    -- ^ The name of the stage that contains the action that acts on the revision.
  , actionName :: Types.ActionName
    -- ^ The name of the action that processes the revision.
  , actionRevision :: Types.ActionRevision
    -- ^ Represents information about the version (or revision) of an action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutActionRevision' value with any optional fields omitted.
mkPutActionRevision
    :: Types.PipelineName -- ^ 'pipelineName'
    -> Types.StageName -- ^ 'stageName'
    -> Types.ActionName -- ^ 'actionName'
    -> Types.ActionRevision -- ^ 'actionRevision'
    -> PutActionRevision
mkPutActionRevision pipelineName stageName actionName
  actionRevision
  = PutActionRevision'{pipelineName, stageName, actionName,
                       actionRevision}

-- | The name of the pipeline that starts processing the revision to the source.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPipelineName :: Lens.Lens' PutActionRevision Types.PipelineName
pPipelineName = Lens.field @"pipelineName"
{-# INLINEABLE pPipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

-- | The name of the stage that contains the action that acts on the revision.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStageName :: Lens.Lens' PutActionRevision Types.StageName
pStageName = Lens.field @"stageName"
{-# INLINEABLE pStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | The name of the action that processes the revision.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActionName :: Lens.Lens' PutActionRevision Types.ActionName
pActionName = Lens.field @"actionName"
{-# INLINEABLE pActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | Represents information about the version (or revision) of an action.
--
-- /Note:/ Consider using 'actionRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActionRevision :: Lens.Lens' PutActionRevision Types.ActionRevision
pActionRevision = Lens.field @"actionRevision"
{-# INLINEABLE pActionRevision #-}
{-# DEPRECATED actionRevision "Use generic-lens or generic-optics with 'actionRevision' instead"  #-}

instance Core.ToQuery PutActionRevision where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutActionRevision where
        toHeaders PutActionRevision{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.PutActionRevision")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutActionRevision where
        toJSON PutActionRevision{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineName" Core..= pipelineName),
                  Core.Just ("stageName" Core..= stageName),
                  Core.Just ("actionName" Core..= actionName),
                  Core.Just ("actionRevision" Core..= actionRevision)])

instance Core.AWSRequest PutActionRevision where
        type Rs PutActionRevision = PutActionRevisionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutActionRevisionResponse' Core.<$>
                   (x Core..:? "newRevision") Core.<*>
                     x Core..:? "pipelineExecutionId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @PutActionRevision@ action.
--
-- /See:/ 'mkPutActionRevisionResponse' smart constructor.
data PutActionRevisionResponse = PutActionRevisionResponse'
  { newRevision :: Core.Maybe Core.Bool
    -- ^ Indicates whether the artifact revision was previously used in an execution of the specified pipeline.
  , pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId
    -- ^ The ID of the current workflow state of the pipeline.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutActionRevisionResponse' value with any optional fields omitted.
mkPutActionRevisionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutActionRevisionResponse
mkPutActionRevisionResponse responseStatus
  = PutActionRevisionResponse'{newRevision = Core.Nothing,
                               pipelineExecutionId = Core.Nothing, responseStatus}

-- | Indicates whether the artifact revision was previously used in an execution of the specified pipeline.
--
-- /Note:/ Consider using 'newRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsNewRevision :: Lens.Lens' PutActionRevisionResponse (Core.Maybe Core.Bool)
prsNewRevision = Lens.field @"newRevision"
{-# INLINEABLE prsNewRevision #-}
{-# DEPRECATED newRevision "Use generic-lens or generic-optics with 'newRevision' instead"  #-}

-- | The ID of the current workflow state of the pipeline.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsPipelineExecutionId :: Lens.Lens' PutActionRevisionResponse (Core.Maybe Types.PipelineExecutionId)
prsPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# INLINEABLE prsPipelineExecutionId #-}
{-# DEPRECATED pipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PutActionRevisionResponse Core.Int
prsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
