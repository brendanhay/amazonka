{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TransformJobSummary
  ( TransformJobSummary (..)
  -- * Smart constructor
  , mkTransformJobSummary
  -- * Lenses
  , tjsTransformJobName
  , tjsTransformJobArn
  , tjsCreationTime
  , tjsTransformJobStatus
  , tjsFailureReason
  , tjsLastModifiedTime
  , tjsTransformEndTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.FailureReason as Types
import qualified Network.AWS.SageMaker.Types.TransformJobArn as Types
import qualified Network.AWS.SageMaker.Types.TransformJobName as Types
import qualified Network.AWS.SageMaker.Types.TransformJobStatus as Types

-- | Provides a summary of a transform job. Multiple @TransformJobSummary@ objects are returned as a list after in response to a 'ListTransformJobs' call.
--
-- /See:/ 'mkTransformJobSummary' smart constructor.
data TransformJobSummary = TransformJobSummary'
  { transformJobName :: Types.TransformJobName
    -- ^ The name of the transform job.
  , transformJobArn :: Types.TransformJobArn
    -- ^ The Amazon Resource Name (ARN) of the transform job.
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the transform Job was created.
  , transformJobStatus :: Types.TransformJobStatus
    -- ^ The status of the transform job.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ If the transform job failed, the reason it failed.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Indicates when the transform job was last modified.
  , transformEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Indicates when the transform job ends on compute instances. For successful jobs and stopped jobs, this is the exact time recorded after the results are uploaded. For failed jobs, this is when Amazon SageMaker detected that the job failed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TransformJobSummary' value with any optional fields omitted.
mkTransformJobSummary
    :: Types.TransformJobName -- ^ 'transformJobName'
    -> Types.TransformJobArn -- ^ 'transformJobArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.TransformJobStatus -- ^ 'transformJobStatus'
    -> TransformJobSummary
mkTransformJobSummary transformJobName transformJobArn creationTime
  transformJobStatus
  = TransformJobSummary'{transformJobName, transformJobArn,
                         creationTime, transformJobStatus, failureReason = Core.Nothing,
                         lastModifiedTime = Core.Nothing, transformEndTime = Core.Nothing}

-- | The name of the transform job.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTransformJobName :: Lens.Lens' TransformJobSummary Types.TransformJobName
tjsTransformJobName = Lens.field @"transformJobName"
{-# INLINEABLE tjsTransformJobName #-}
{-# DEPRECATED transformJobName "Use generic-lens or generic-optics with 'transformJobName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the transform job.
--
-- /Note:/ Consider using 'transformJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTransformJobArn :: Lens.Lens' TransformJobSummary Types.TransformJobArn
tjsTransformJobArn = Lens.field @"transformJobArn"
{-# INLINEABLE tjsTransformJobArn #-}
{-# DEPRECATED transformJobArn "Use generic-lens or generic-optics with 'transformJobArn' instead"  #-}

-- | A timestamp that shows when the transform Job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsCreationTime :: Lens.Lens' TransformJobSummary Core.NominalDiffTime
tjsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tjsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The status of the transform job.
--
-- /Note:/ Consider using 'transformJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTransformJobStatus :: Lens.Lens' TransformJobSummary Types.TransformJobStatus
tjsTransformJobStatus = Lens.field @"transformJobStatus"
{-# INLINEABLE tjsTransformJobStatus #-}
{-# DEPRECATED transformJobStatus "Use generic-lens or generic-optics with 'transformJobStatus' instead"  #-}

-- | If the transform job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsFailureReason :: Lens.Lens' TransformJobSummary (Core.Maybe Types.FailureReason)
tjsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE tjsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | Indicates when the transform job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsLastModifiedTime :: Lens.Lens' TransformJobSummary (Core.Maybe Core.NominalDiffTime)
tjsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE tjsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | Indicates when the transform job ends on compute instances. For successful jobs and stopped jobs, this is the exact time recorded after the results are uploaded. For failed jobs, this is when Amazon SageMaker detected that the job failed.
--
-- /Note:/ Consider using 'transformEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTransformEndTime :: Lens.Lens' TransformJobSummary (Core.Maybe Core.NominalDiffTime)
tjsTransformEndTime = Lens.field @"transformEndTime"
{-# INLINEABLE tjsTransformEndTime #-}
{-# DEPRECATED transformEndTime "Use generic-lens or generic-optics with 'transformEndTime' instead"  #-}

instance Core.FromJSON TransformJobSummary where
        parseJSON
          = Core.withObject "TransformJobSummary" Core.$
              \ x ->
                TransformJobSummary' Core.<$>
                  (x Core..: "TransformJobName") Core.<*> x Core..: "TransformJobArn"
                    Core.<*> x Core..: "CreationTime"
                    Core.<*> x Core..: "TransformJobStatus"
                    Core.<*> x Core..:? "FailureReason"
                    Core.<*> x Core..:? "LastModifiedTime"
                    Core.<*> x Core..:? "TransformEndTime"
