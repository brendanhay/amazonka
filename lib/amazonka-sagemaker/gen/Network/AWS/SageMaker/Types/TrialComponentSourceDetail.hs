{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrialComponentSourceDetail
  ( TrialComponentSourceDetail (..)
  -- * Smart constructor
  , mkTrialComponentSourceDetail
  -- * Lenses
  , tcsdProcessingJob
  , tcsdSourceArn
  , tcsdTrainingJob
  , tcsdTransformJob
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ProcessingJob as Types
import qualified Network.AWS.SageMaker.Types.SourceArn as Types
import qualified Network.AWS.SageMaker.Types.TrainingJob as Types
import qualified Network.AWS.SageMaker.Types.TransformJob as Types

-- | Detailed information about the source of a trial component. Either @ProcessingJob@ or @TrainingJob@ is returned.
--
-- /See:/ 'mkTrialComponentSourceDetail' smart constructor.
data TrialComponentSourceDetail = TrialComponentSourceDetail'
  { processingJob :: Core.Maybe Types.ProcessingJob
    -- ^ Information about a processing job that's the source of a trial component.
  , sourceArn :: Core.Maybe Types.SourceArn
    -- ^ The Amazon Resource Name (ARN) of the source.
  , trainingJob :: Core.Maybe Types.TrainingJob
    -- ^ Information about a training job that's the source of a trial component.
  , transformJob :: Core.Maybe Types.TransformJob
    -- ^ Information about a transform job that's the source of a trial component.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TrialComponentSourceDetail' value with any optional fields omitted.
mkTrialComponentSourceDetail
    :: TrialComponentSourceDetail
mkTrialComponentSourceDetail
  = TrialComponentSourceDetail'{processingJob = Core.Nothing,
                                sourceArn = Core.Nothing, trainingJob = Core.Nothing,
                                transformJob = Core.Nothing}

-- | Information about a processing job that's the source of a trial component.
--
-- /Note:/ Consider using 'processingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsdProcessingJob :: Lens.Lens' TrialComponentSourceDetail (Core.Maybe Types.ProcessingJob)
tcsdProcessingJob = Lens.field @"processingJob"
{-# INLINEABLE tcsdProcessingJob #-}
{-# DEPRECATED processingJob "Use generic-lens or generic-optics with 'processingJob' instead"  #-}

-- | The Amazon Resource Name (ARN) of the source.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsdSourceArn :: Lens.Lens' TrialComponentSourceDetail (Core.Maybe Types.SourceArn)
tcsdSourceArn = Lens.field @"sourceArn"
{-# INLINEABLE tcsdSourceArn #-}
{-# DEPRECATED sourceArn "Use generic-lens or generic-optics with 'sourceArn' instead"  #-}

-- | Information about a training job that's the source of a trial component.
--
-- /Note:/ Consider using 'trainingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsdTrainingJob :: Lens.Lens' TrialComponentSourceDetail (Core.Maybe Types.TrainingJob)
tcsdTrainingJob = Lens.field @"trainingJob"
{-# INLINEABLE tcsdTrainingJob #-}
{-# DEPRECATED trainingJob "Use generic-lens or generic-optics with 'trainingJob' instead"  #-}

-- | Information about a transform job that's the source of a trial component.
--
-- /Note:/ Consider using 'transformJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsdTransformJob :: Lens.Lens' TrialComponentSourceDetail (Core.Maybe Types.TransformJob)
tcsdTransformJob = Lens.field @"transformJob"
{-# INLINEABLE tcsdTransformJob #-}
{-# DEPRECATED transformJob "Use generic-lens or generic-optics with 'transformJob' instead"  #-}

instance Core.FromJSON TrialComponentSourceDetail where
        parseJSON
          = Core.withObject "TrialComponentSourceDetail" Core.$
              \ x ->
                TrialComponentSourceDetail' Core.<$>
                  (x Core..:? "ProcessingJob") Core.<*> x Core..:? "SourceArn"
                    Core.<*> x Core..:? "TrainingJob"
                    Core.<*> x Core..:? "TransformJob"
