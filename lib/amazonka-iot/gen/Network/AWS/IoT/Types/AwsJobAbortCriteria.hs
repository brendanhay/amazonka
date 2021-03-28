{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AwsJobAbortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AwsJobAbortCriteria
  ( AwsJobAbortCriteria (..)
  -- * Smart constructor
  , mkAwsJobAbortCriteria
  -- * Lenses
  , ajacFailureType
  , ajacAction
  , ajacThresholdPercentage
  , ajacMinNumberOfExecutedThings
  ) where

import qualified Network.AWS.IoT.Types.AwsJobAbortCriteriaAbortAction as Types
import qualified Network.AWS.IoT.Types.AwsJobAbortCriteriaFailureType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'mkAwsJobAbortCriteria' smart constructor.
data AwsJobAbortCriteria = AwsJobAbortCriteria'
  { failureType :: Types.AwsJobAbortCriteriaFailureType
    -- ^ The type of job execution failures that can initiate a job abort.
  , action :: Types.AwsJobAbortCriteriaAbortAction
    -- ^ The type of job action to take to initiate the job abort.
  , thresholdPercentage :: Core.Double
    -- ^ The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
  , minNumberOfExecutedThings :: Core.Natural
    -- ^ The minimum number of things which must receive job execution notifications before the job can be aborted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsJobAbortCriteria' value with any optional fields omitted.
mkAwsJobAbortCriteria
    :: Types.AwsJobAbortCriteriaFailureType -- ^ 'failureType'
    -> Types.AwsJobAbortCriteriaAbortAction -- ^ 'action'
    -> Core.Double -- ^ 'thresholdPercentage'
    -> Core.Natural -- ^ 'minNumberOfExecutedThings'
    -> AwsJobAbortCriteria
mkAwsJobAbortCriteria failureType action thresholdPercentage
  minNumberOfExecutedThings
  = AwsJobAbortCriteria'{failureType, action, thresholdPercentage,
                         minNumberOfExecutedThings}

-- | The type of job execution failures that can initiate a job abort.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacFailureType :: Lens.Lens' AwsJobAbortCriteria Types.AwsJobAbortCriteriaFailureType
ajacFailureType = Lens.field @"failureType"
{-# INLINEABLE ajacFailureType #-}
{-# DEPRECATED failureType "Use generic-lens or generic-optics with 'failureType' instead"  #-}

-- | The type of job action to take to initiate the job abort.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacAction :: Lens.Lens' AwsJobAbortCriteria Types.AwsJobAbortCriteriaAbortAction
ajacAction = Lens.field @"action"
{-# INLINEABLE ajacAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
--
-- /Note:/ Consider using 'thresholdPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacThresholdPercentage :: Lens.Lens' AwsJobAbortCriteria Core.Double
ajacThresholdPercentage = Lens.field @"thresholdPercentage"
{-# INLINEABLE ajacThresholdPercentage #-}
{-# DEPRECATED thresholdPercentage "Use generic-lens or generic-optics with 'thresholdPercentage' instead"  #-}

-- | The minimum number of things which must receive job execution notifications before the job can be aborted.
--
-- /Note:/ Consider using 'minNumberOfExecutedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacMinNumberOfExecutedThings :: Lens.Lens' AwsJobAbortCriteria Core.Natural
ajacMinNumberOfExecutedThings = Lens.field @"minNumberOfExecutedThings"
{-# INLINEABLE ajacMinNumberOfExecutedThings #-}
{-# DEPRECATED minNumberOfExecutedThings "Use generic-lens or generic-optics with 'minNumberOfExecutedThings' instead"  #-}

instance Core.FromJSON AwsJobAbortCriteria where
        toJSON AwsJobAbortCriteria{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("failureType" Core..= failureType),
                  Core.Just ("action" Core..= action),
                  Core.Just ("thresholdPercentage" Core..= thresholdPercentage),
                  Core.Just
                    ("minNumberOfExecutedThings" Core..= minNumberOfExecutedThings)])
