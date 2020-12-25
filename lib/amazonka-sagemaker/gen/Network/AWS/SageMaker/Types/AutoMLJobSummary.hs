{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobSummary
  ( AutoMLJobSummary (..),

    -- * Smart constructor
    mkAutoMLJobSummary,

    -- * Lenses
    amljsAutoMLJobName,
    amljsAutoMLJobArn,
    amljsAutoMLJobStatus,
    amljsAutoMLJobSecondaryStatus,
    amljsCreationTime,
    amljsLastModifiedTime,
    amljsEndTime,
    amljsFailureReason,
  )
where

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
  { -- | The name of the object you are requesting.
    autoMLJobName :: Types.AutoMLJobName,
    -- | The ARN of the job.
    autoMLJobArn :: Types.AutoMLJobArn,
    -- | The job's status.
    autoMLJobStatus :: Types.AutoMLJobStatus,
    -- | The job's secondary status.
    autoMLJobSecondaryStatus :: Types.AutoMLJobSecondaryStatus,
    -- | When the job was created.
    creationTime :: Core.NominalDiffTime,
    -- | When the job was last modified.
    lastModifiedTime :: Core.NominalDiffTime,
    -- | The end time of an AutoML job.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The failure reason of a job.
    failureReason :: Core.Maybe Types.AutoMLFailureReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AutoMLJobSummary' value with any optional fields omitted.
mkAutoMLJobSummary ::
  -- | 'autoMLJobName'
  Types.AutoMLJobName ->
  -- | 'autoMLJobArn'
  Types.AutoMLJobArn ->
  -- | 'autoMLJobStatus'
  Types.AutoMLJobStatus ->
  -- | 'autoMLJobSecondaryStatus'
  Types.AutoMLJobSecondaryStatus ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'lastModifiedTime'
  Core.NominalDiffTime ->
  AutoMLJobSummary
mkAutoMLJobSummary
  autoMLJobName
  autoMLJobArn
  autoMLJobStatus
  autoMLJobSecondaryStatus
  creationTime
  lastModifiedTime =
    AutoMLJobSummary'
      { autoMLJobName,
        autoMLJobArn,
        autoMLJobStatus,
        autoMLJobSecondaryStatus,
        creationTime,
        lastModifiedTime,
        endTime = Core.Nothing,
        failureReason = Core.Nothing
      }

-- | The name of the object you are requesting.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobName :: Lens.Lens' AutoMLJobSummary Types.AutoMLJobName
amljsAutoMLJobName = Lens.field @"autoMLJobName"
{-# DEPRECATED amljsAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

-- | The ARN of the job.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobArn :: Lens.Lens' AutoMLJobSummary Types.AutoMLJobArn
amljsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# DEPRECATED amljsAutoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead." #-}

-- | The job's status.
--
-- /Note:/ Consider using 'autoMLJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobStatus :: Lens.Lens' AutoMLJobSummary Types.AutoMLJobStatus
amljsAutoMLJobStatus = Lens.field @"autoMLJobStatus"
{-# DEPRECATED amljsAutoMLJobStatus "Use generic-lens or generic-optics with 'autoMLJobStatus' instead." #-}

-- | The job's secondary status.
--
-- /Note:/ Consider using 'autoMLJobSecondaryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobSecondaryStatus :: Lens.Lens' AutoMLJobSummary Types.AutoMLJobSecondaryStatus
amljsAutoMLJobSecondaryStatus = Lens.field @"autoMLJobSecondaryStatus"
{-# DEPRECATED amljsAutoMLJobSecondaryStatus "Use generic-lens or generic-optics with 'autoMLJobSecondaryStatus' instead." #-}

-- | When the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsCreationTime :: Lens.Lens' AutoMLJobSummary Core.NominalDiffTime
amljsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED amljsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | When the job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsLastModifiedTime :: Lens.Lens' AutoMLJobSummary Core.NominalDiffTime
amljsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED amljsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The end time of an AutoML job.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsEndTime :: Lens.Lens' AutoMLJobSummary (Core.Maybe Core.NominalDiffTime)
amljsEndTime = Lens.field @"endTime"
{-# DEPRECATED amljsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The failure reason of a job.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsFailureReason :: Lens.Lens' AutoMLJobSummary (Core.Maybe Types.AutoMLFailureReason)
amljsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED amljsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

instance Core.FromJSON AutoMLJobSummary where
  parseJSON =
    Core.withObject "AutoMLJobSummary" Core.$
      \x ->
        AutoMLJobSummary'
          Core.<$> (x Core..: "AutoMLJobName")
          Core.<*> (x Core..: "AutoMLJobArn")
          Core.<*> (x Core..: "AutoMLJobStatus")
          Core.<*> (x Core..: "AutoMLJobSecondaryStatus")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "LastModifiedTime")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "FailureReason")
