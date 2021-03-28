{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLJobSummary
  ( AutoMLJobSummary (..)
  -- * Smart constructor
  , mkAutoMLJobSummary
  -- * Lenses
  , amljsAutoMLJobName
  , amljsAutoMLJobArn
  , amljsAutoMLJobStatus
  , amljsAutoMLJobSecondaryStatus
  , amljsCreationTime
  , amljsLastModifiedTime
  , amljsEndTime
  , amljsFailureReason
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLFailureReason as Types
import qualified Network.AWS.SageMaker.Types.AutoMLJobArn as Types
import qualified Network.AWS.SageMaker.Types.AutoMLJobName as Types
import qualified Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus as Types
import qualified Network.AWS.SageMaker.Types.AutoMLJobStatus as Types

-- | Provides a summary about a job.
--
-- /See:/ 'mkAutoMLJobSummary' smart constructor.
data AutoMLJobSummary = AutoMLJobSummary'
  { autoMLJobName :: Types.AutoMLJobName
    -- ^ The name of the object you are requesting.
  , autoMLJobArn :: Types.AutoMLJobArn
    -- ^ The ARN of the job.
  , autoMLJobStatus :: Types.AutoMLJobStatus
    -- ^ The job's status.
  , autoMLJobSecondaryStatus :: Types.AutoMLJobSecondaryStatus
    -- ^ The job's secondary status.
  , creationTime :: Core.NominalDiffTime
    -- ^ When the job was created.
  , lastModifiedTime :: Core.NominalDiffTime
    -- ^ When the job was last modified.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The end time of an AutoML job.
  , failureReason :: Core.Maybe Types.AutoMLFailureReason
    -- ^ The failure reason of a job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AutoMLJobSummary' value with any optional fields omitted.
mkAutoMLJobSummary
    :: Types.AutoMLJobName -- ^ 'autoMLJobName'
    -> Types.AutoMLJobArn -- ^ 'autoMLJobArn'
    -> Types.AutoMLJobStatus -- ^ 'autoMLJobStatus'
    -> Types.AutoMLJobSecondaryStatus -- ^ 'autoMLJobSecondaryStatus'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTime'
    -> AutoMLJobSummary
mkAutoMLJobSummary autoMLJobName autoMLJobArn autoMLJobStatus
  autoMLJobSecondaryStatus creationTime lastModifiedTime
  = AutoMLJobSummary'{autoMLJobName, autoMLJobArn, autoMLJobStatus,
                      autoMLJobSecondaryStatus, creationTime, lastModifiedTime,
                      endTime = Core.Nothing, failureReason = Core.Nothing}

-- | The name of the object you are requesting.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobName :: Lens.Lens' AutoMLJobSummary Types.AutoMLJobName
amljsAutoMLJobName = Lens.field @"autoMLJobName"
{-# INLINEABLE amljsAutoMLJobName #-}
{-# DEPRECATED autoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead"  #-}

-- | The ARN of the job.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobArn :: Lens.Lens' AutoMLJobSummary Types.AutoMLJobArn
amljsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# INLINEABLE amljsAutoMLJobArn #-}
{-# DEPRECATED autoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead"  #-}

-- | The job's status.
--
-- /Note:/ Consider using 'autoMLJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobStatus :: Lens.Lens' AutoMLJobSummary Types.AutoMLJobStatus
amljsAutoMLJobStatus = Lens.field @"autoMLJobStatus"
{-# INLINEABLE amljsAutoMLJobStatus #-}
{-# DEPRECATED autoMLJobStatus "Use generic-lens or generic-optics with 'autoMLJobStatus' instead"  #-}

-- | The job's secondary status.
--
-- /Note:/ Consider using 'autoMLJobSecondaryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobSecondaryStatus :: Lens.Lens' AutoMLJobSummary Types.AutoMLJobSecondaryStatus
amljsAutoMLJobSecondaryStatus = Lens.field @"autoMLJobSecondaryStatus"
{-# INLINEABLE amljsAutoMLJobSecondaryStatus #-}
{-# DEPRECATED autoMLJobSecondaryStatus "Use generic-lens or generic-optics with 'autoMLJobSecondaryStatus' instead"  #-}

-- | When the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsCreationTime :: Lens.Lens' AutoMLJobSummary Core.NominalDiffTime
amljsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE amljsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | When the job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsLastModifiedTime :: Lens.Lens' AutoMLJobSummary Core.NominalDiffTime
amljsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE amljsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The end time of an AutoML job.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsEndTime :: Lens.Lens' AutoMLJobSummary (Core.Maybe Core.NominalDiffTime)
amljsEndTime = Lens.field @"endTime"
{-# INLINEABLE amljsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The failure reason of a job.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsFailureReason :: Lens.Lens' AutoMLJobSummary (Core.Maybe Types.AutoMLFailureReason)
amljsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE amljsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

instance Core.FromJSON AutoMLJobSummary where
        parseJSON
          = Core.withObject "AutoMLJobSummary" Core.$
              \ x ->
                AutoMLJobSummary' Core.<$>
                  (x Core..: "AutoMLJobName") Core.<*> x Core..: "AutoMLJobArn"
                    Core.<*> x Core..: "AutoMLJobStatus"
                    Core.<*> x Core..: "AutoMLJobSecondaryStatus"
                    Core.<*> x Core..: "CreationTime"
                    Core.<*> x Core..: "LastModifiedTime"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "FailureReason"
