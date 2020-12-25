{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AbortCriteria
  ( AbortCriteria (..),

    -- * Smart constructor
    mkAbortCriteria,

    -- * Lenses
    acFailureType,
    acAction,
    acThresholdPercentage,
    acMinNumberOfExecutedThings,
  )
where

import qualified Network.AWS.IoT.Types.AbortAction as Types
import qualified Network.AWS.IoT.Types.JobExecutionFailureType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'mkAbortCriteria' smart constructor.
data AbortCriteria = AbortCriteria'
  { -- | The type of job execution failures that can initiate a job abort.
    failureType :: Types.JobExecutionFailureType,
    -- | The type of job action to take to initiate the job abort.
    action :: Types.AbortAction,
    -- | The minimum percentage of job execution failures that must occur to initiate the job abort.
    --
    -- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
    thresholdPercentage :: Core.Double,
    -- | The minimum number of things which must receive job execution notifications before the job can be aborted.
    minNumberOfExecutedThings :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortCriteria' value with any optional fields omitted.
mkAbortCriteria ::
  -- | 'failureType'
  Types.JobExecutionFailureType ->
  -- | 'action'
  Types.AbortAction ->
  -- | 'thresholdPercentage'
  Core.Double ->
  -- | 'minNumberOfExecutedThings'
  Core.Natural ->
  AbortCriteria
mkAbortCriteria
  failureType
  action
  thresholdPercentage
  minNumberOfExecutedThings =
    AbortCriteria'
      { failureType,
        action,
        thresholdPercentage,
        minNumberOfExecutedThings
      }

-- | The type of job execution failures that can initiate a job abort.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acFailureType :: Lens.Lens' AbortCriteria Types.JobExecutionFailureType
acFailureType = Lens.field @"failureType"
{-# DEPRECATED acFailureType "Use generic-lens or generic-optics with 'failureType' instead." #-}

-- | The type of job action to take to initiate the job abort.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAction :: Lens.Lens' AbortCriteria Types.AbortAction
acAction = Lens.field @"action"
{-# DEPRECATED acAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
--
-- /Note:/ Consider using 'thresholdPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acThresholdPercentage :: Lens.Lens' AbortCriteria Core.Double
acThresholdPercentage = Lens.field @"thresholdPercentage"
{-# DEPRECATED acThresholdPercentage "Use generic-lens or generic-optics with 'thresholdPercentage' instead." #-}

-- | The minimum number of things which must receive job execution notifications before the job can be aborted.
--
-- /Note:/ Consider using 'minNumberOfExecutedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acMinNumberOfExecutedThings :: Lens.Lens' AbortCriteria Core.Natural
acMinNumberOfExecutedThings = Lens.field @"minNumberOfExecutedThings"
{-# DEPRECATED acMinNumberOfExecutedThings "Use generic-lens or generic-optics with 'minNumberOfExecutedThings' instead." #-}

instance Core.FromJSON AbortCriteria where
  toJSON AbortCriteria {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("failureType" Core..= failureType),
            Core.Just ("action" Core..= action),
            Core.Just ("thresholdPercentage" Core..= thresholdPercentage),
            Core.Just
              ("minNumberOfExecutedThings" Core..= minNumberOfExecutedThings)
          ]
      )

instance Core.FromJSON AbortCriteria where
  parseJSON =
    Core.withObject "AbortCriteria" Core.$
      \x ->
        AbortCriteria'
          Core.<$> (x Core..: "failureType")
          Core.<*> (x Core..: "action")
          Core.<*> (x Core..: "thresholdPercentage")
          Core.<*> (x Core..: "minNumberOfExecutedThings")
