{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trial's properties.
module Network.AWS.SageMaker.DescribeTrial
    (
    -- * Creating a request
      DescribeTrial (..)
    , mkDescribeTrial
    -- ** Request lenses
    , dtTrialName

    -- * Destructuring the response
    , DescribeTrialResponse (..)
    , mkDescribeTrialResponse
    -- ** Response lenses
    , dtrrsCreatedBy
    , dtrrsCreationTime
    , dtrrsDisplayName
    , dtrrsExperimentName
    , dtrrsLastModifiedBy
    , dtrrsLastModifiedTime
    , dtrrsSource
    , dtrrsTrialArn
    , dtrrsTrialName
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeTrial' smart constructor.
newtype DescribeTrial = DescribeTrial'
  { trialName :: Types.ExperimentEntityName
    -- ^ The name of the trial to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrial' value with any optional fields omitted.
mkDescribeTrial
    :: Types.ExperimentEntityName -- ^ 'trialName'
    -> DescribeTrial
mkDescribeTrial trialName = DescribeTrial'{trialName}

-- | The name of the trial to describe.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTrialName :: Lens.Lens' DescribeTrial Types.ExperimentEntityName
dtTrialName = Lens.field @"trialName"
{-# INLINEABLE dtTrialName #-}
{-# DEPRECATED trialName "Use generic-lens or generic-optics with 'trialName' instead"  #-}

instance Core.ToQuery DescribeTrial where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTrial where
        toHeaders DescribeTrial{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeTrial") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTrial where
        toJSON DescribeTrial{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TrialName" Core..= trialName)])

instance Core.AWSRequest DescribeTrial where
        type Rs DescribeTrial = DescribeTrialResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTrialResponse' Core.<$>
                   (x Core..:? "CreatedBy") Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "DisplayName"
                     Core.<*> x Core..:? "ExperimentName"
                     Core.<*> x Core..:? "LastModifiedBy"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "Source"
                     Core.<*> x Core..:? "TrialArn"
                     Core.<*> x Core..:? "TrialName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTrialResponse' smart constructor.
data DescribeTrialResponse = DescribeTrialResponse'
  { createdBy :: Core.Maybe Types.UserContext
    -- ^ Who created the trial.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the trial was created.
  , displayName :: Core.Maybe Types.DisplayName
    -- ^ The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
  , experimentName :: Core.Maybe Types.ExperimentName
    -- ^ The name of the experiment the trial is part of.
  , lastModifiedBy :: Core.Maybe Types.UserContext
    -- ^ Who last modified the trial.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the trial was last modified.
  , source :: Core.Maybe Types.TrialSource
    -- ^ The Amazon Resource Name (ARN) of the source and, optionally, the job type.
  , trialArn :: Core.Maybe Types.TrialArn
    -- ^ The Amazon Resource Name (ARN) of the trial.
  , trialName :: Core.Maybe Types.TrialName
    -- ^ The name of the trial.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTrialResponse' value with any optional fields omitted.
mkDescribeTrialResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTrialResponse
mkDescribeTrialResponse responseStatus
  = DescribeTrialResponse'{createdBy = Core.Nothing,
                           creationTime = Core.Nothing, displayName = Core.Nothing,
                           experimentName = Core.Nothing, lastModifiedBy = Core.Nothing,
                           lastModifiedTime = Core.Nothing, source = Core.Nothing,
                           trialArn = Core.Nothing, trialName = Core.Nothing, responseStatus}

-- | Who created the trial.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsCreatedBy :: Lens.Lens' DescribeTrialResponse (Core.Maybe Types.UserContext)
dtrrsCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE dtrrsCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | When the trial was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsCreationTime :: Lens.Lens' DescribeTrialResponse (Core.Maybe Core.NominalDiffTime)
dtrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dtrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsDisplayName :: Lens.Lens' DescribeTrialResponse (Core.Maybe Types.DisplayName)
dtrrsDisplayName = Lens.field @"displayName"
{-# INLINEABLE dtrrsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The name of the experiment the trial is part of.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsExperimentName :: Lens.Lens' DescribeTrialResponse (Core.Maybe Types.ExperimentName)
dtrrsExperimentName = Lens.field @"experimentName"
{-# INLINEABLE dtrrsExperimentName #-}
{-# DEPRECATED experimentName "Use generic-lens or generic-optics with 'experimentName' instead"  #-}

-- | Who last modified the trial.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsLastModifiedBy :: Lens.Lens' DescribeTrialResponse (Core.Maybe Types.UserContext)
dtrrsLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE dtrrsLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | When the trial was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsLastModifiedTime :: Lens.Lens' DescribeTrialResponse (Core.Maybe Core.NominalDiffTime)
dtrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dtrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsSource :: Lens.Lens' DescribeTrialResponse (Core.Maybe Types.TrialSource)
dtrrsSource = Lens.field @"source"
{-# INLINEABLE dtrrsSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTrialArn :: Lens.Lens' DescribeTrialResponse (Core.Maybe Types.TrialArn)
dtrrsTrialArn = Lens.field @"trialArn"
{-# INLINEABLE dtrrsTrialArn #-}
{-# DEPRECATED trialArn "Use generic-lens or generic-optics with 'trialArn' instead"  #-}

-- | The name of the trial.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTrialName :: Lens.Lens' DescribeTrialResponse (Core.Maybe Types.TrialName)
dtrrsTrialName = Lens.field @"trialName"
{-# INLINEABLE dtrrsTrialName #-}
{-# DEPRECATED trialName "Use generic-lens or generic-optics with 'trialName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTrialResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
