{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of an experiment's properties.
module Network.AWS.SageMaker.DescribeExperiment
    (
    -- * Creating a request
      DescribeExperiment (..)
    , mkDescribeExperiment
    -- ** Request lenses
    , deExperimentName

    -- * Destructuring the response
    , DescribeExperimentResponse (..)
    , mkDescribeExperimentResponse
    -- ** Response lenses
    , derrsCreatedBy
    , derrsCreationTime
    , derrsDescription
    , derrsDisplayName
    , derrsExperimentArn
    , derrsExperimentName
    , derrsLastModifiedBy
    , derrsLastModifiedTime
    , derrsSource
    , derrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeExperiment' smart constructor.
newtype DescribeExperiment = DescribeExperiment'
  { experimentName :: Types.ExperimentName
    -- ^ The name of the experiment to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExperiment' value with any optional fields omitted.
mkDescribeExperiment
    :: Types.ExperimentName -- ^ 'experimentName'
    -> DescribeExperiment
mkDescribeExperiment experimentName
  = DescribeExperiment'{experimentName}

-- | The name of the experiment to describe.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExperimentName :: Lens.Lens' DescribeExperiment Types.ExperimentName
deExperimentName = Lens.field @"experimentName"
{-# INLINEABLE deExperimentName #-}
{-# DEPRECATED experimentName "Use generic-lens or generic-optics with 'experimentName' instead"  #-}

instance Core.ToQuery DescribeExperiment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeExperiment where
        toHeaders DescribeExperiment{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeExperiment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeExperiment where
        toJSON DescribeExperiment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ExperimentName" Core..= experimentName)])

instance Core.AWSRequest DescribeExperiment where
        type Rs DescribeExperiment = DescribeExperimentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeExperimentResponse' Core.<$>
                   (x Core..:? "CreatedBy") Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "Description"
                     Core.<*> x Core..:? "DisplayName"
                     Core.<*> x Core..:? "ExperimentArn"
                     Core.<*> x Core..:? "ExperimentName"
                     Core.<*> x Core..:? "LastModifiedBy"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "Source"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeExperimentResponse' smart constructor.
data DescribeExperimentResponse = DescribeExperimentResponse'
  { createdBy :: Core.Maybe Types.UserContext
    -- ^ Who created the experiment.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the experiment was created.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the experiment.
  , displayName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
  , experimentArn :: Core.Maybe Types.ExperimentArn
    -- ^ The Amazon Resource Name (ARN) of the experiment.
  , experimentName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the experiment.
  , lastModifiedBy :: Core.Maybe Types.UserContext
    -- ^ Who last modified the experiment.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the experiment was last modified.
  , source :: Core.Maybe Types.ExperimentSource
    -- ^ The ARN of the source and, optionally, the type.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeExperimentResponse' value with any optional fields omitted.
mkDescribeExperimentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeExperimentResponse
mkDescribeExperimentResponse responseStatus
  = DescribeExperimentResponse'{createdBy = Core.Nothing,
                                creationTime = Core.Nothing, description = Core.Nothing,
                                displayName = Core.Nothing, experimentArn = Core.Nothing,
                                experimentName = Core.Nothing, lastModifiedBy = Core.Nothing,
                                lastModifiedTime = Core.Nothing, source = Core.Nothing,
                                responseStatus}

-- | Who created the experiment.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsCreatedBy :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Types.UserContext)
derrsCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE derrsCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | When the experiment was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsCreationTime :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Core.NominalDiffTime)
derrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE derrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The description of the experiment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsDescription :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Types.Description)
derrsDescription = Lens.field @"description"
{-# INLINEABLE derrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsDisplayName :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Types.ExperimentEntityName)
derrsDisplayName = Lens.field @"displayName"
{-# INLINEABLE derrsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsExperimentArn :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Types.ExperimentArn)
derrsExperimentArn = Lens.field @"experimentArn"
{-# INLINEABLE derrsExperimentArn #-}
{-# DEPRECATED experimentArn "Use generic-lens or generic-optics with 'experimentArn' instead"  #-}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsExperimentName :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Types.ExperimentEntityName)
derrsExperimentName = Lens.field @"experimentName"
{-# INLINEABLE derrsExperimentName #-}
{-# DEPRECATED experimentName "Use generic-lens or generic-optics with 'experimentName' instead"  #-}

-- | Who last modified the experiment.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsLastModifiedBy :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Types.UserContext)
derrsLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE derrsLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | When the experiment was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsLastModifiedTime :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Core.NominalDiffTime)
derrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE derrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The ARN of the source and, optionally, the type.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsSource :: Lens.Lens' DescribeExperimentResponse (Core.Maybe Types.ExperimentSource)
derrsSource = Lens.field @"source"
{-# INLINEABLE derrsSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeExperimentResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
