{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.AttemptDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.AttemptDetail
  ( AttemptDetail (..),

    -- * Smart constructor
    mkAttemptDetail,

    -- * Lenses
    adContainer,
    adStartedAt,
    adStatusReason,
    adStoppedAt,
  )
where

import qualified Network.AWS.Batch.Types.AttemptContainerDetail as Types
import qualified Network.AWS.Batch.Types.StatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a job attempt.
--
-- /See:/ 'mkAttemptDetail' smart constructor.
data AttemptDetail = AttemptDetail'
  { -- | Details about the container in this job attempt.
    container :: Core.Maybe Types.AttemptContainerDetail,
    -- | The Unix timestamp (in milliseconds) for when the attempt was started (when the attempt transitioned from the @STARTING@ state to the @RUNNING@ state).
    startedAt :: Core.Maybe Core.Integer,
    -- | A short, human-readable string to provide additional details about the current status of the job attempt.
    statusReason :: Core.Maybe Types.StatusReason,
    -- | The Unix timestamp (in milliseconds) for when the attempt was stopped (when the attempt transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
    stoppedAt :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttemptDetail' value with any optional fields omitted.
mkAttemptDetail ::
  AttemptDetail
mkAttemptDetail =
  AttemptDetail'
    { container = Core.Nothing,
      startedAt = Core.Nothing,
      statusReason = Core.Nothing,
      stoppedAt = Core.Nothing
    }

-- | Details about the container in this job attempt.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adContainer :: Lens.Lens' AttemptDetail (Core.Maybe Types.AttemptContainerDetail)
adContainer = Lens.field @"container"
{-# DEPRECATED adContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | The Unix timestamp (in milliseconds) for when the attempt was started (when the attempt transitioned from the @STARTING@ state to the @RUNNING@ state).
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStartedAt :: Lens.Lens' AttemptDetail (Core.Maybe Core.Integer)
adStartedAt = Lens.field @"startedAt"
{-# DEPRECATED adStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the job attempt.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStatusReason :: Lens.Lens' AttemptDetail (Core.Maybe Types.StatusReason)
adStatusReason = Lens.field @"statusReason"
{-# DEPRECATED adStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The Unix timestamp (in milliseconds) for when the attempt was stopped (when the attempt transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStoppedAt :: Lens.Lens' AttemptDetail (Core.Maybe Core.Integer)
adStoppedAt = Lens.field @"stoppedAt"
{-# DEPRECATED adStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

instance Core.FromJSON AttemptDetail where
  parseJSON =
    Core.withObject "AttemptDetail" Core.$
      \x ->
        AttemptDetail'
          Core.<$> (x Core..:? "container")
          Core.<*> (x Core..:? "startedAt")
          Core.<*> (x Core..:? "statusReason")
          Core.<*> (x Core..:? "stoppedAt")
