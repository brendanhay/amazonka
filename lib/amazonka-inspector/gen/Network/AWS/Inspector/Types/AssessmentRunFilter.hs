{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentRunFilter
  ( AssessmentRunFilter (..)
  -- * Smart constructor
  , mkAssessmentRunFilter
  -- * Lenses
  , arfCompletionTimeRange
  , arfDurationRange
  , arfNamePattern
  , arfRulesPackageArns
  , arfStartTimeRange
  , arfStateChangeTimeRange
  , arfStates
  ) where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.AssessmentRunState as Types
import qualified Network.AWS.Inspector.Types.DurationRange as Types
import qualified Network.AWS.Inspector.Types.NamePattern as Types
import qualified Network.AWS.Inspector.Types.TimestampRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used as the request parameter in the 'ListAssessmentRuns' action.
--
-- /See:/ 'mkAssessmentRunFilter' smart constructor.
data AssessmentRunFilter = AssessmentRunFilter'
  { completionTimeRange :: Core.Maybe Types.TimestampRange
    -- ^ For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __completedAt__ property of the 'AssessmentRun' data type.
  , durationRange :: Core.Maybe Types.DurationRange
    -- ^ For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentRun' data type.
  , namePattern :: Core.Maybe Types.NamePattern
    -- ^ For a record to match a filter, an explicit value or a string containing a wildcard that is specified for this data type property must match the value of the __assessmentRunName__ property of the 'AssessmentRun' data type.
  , rulesPackageArns :: Core.Maybe [Types.Arn]
    -- ^ For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __rulesPackages__ property of the 'AssessmentRun' data type.
  , startTimeRange :: Core.Maybe Types.TimestampRange
    -- ^ For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __startTime__ property of the 'AssessmentRun' data type.
  , stateChangeTimeRange :: Core.Maybe Types.TimestampRange
    -- ^ For a record to match a filter, the value that is specified for this data type property must match the __stateChangedAt__ property of the 'AssessmentRun' data type.
  , states :: Core.Maybe [Types.AssessmentRunState]
    -- ^ For a record to match a filter, one of the values specified for this data type property must be the exact match of the value of the __assessmentRunState__ property of the 'AssessmentRun' data type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AssessmentRunFilter' value with any optional fields omitted.
mkAssessmentRunFilter
    :: AssessmentRunFilter
mkAssessmentRunFilter
  = AssessmentRunFilter'{completionTimeRange = Core.Nothing,
                         durationRange = Core.Nothing, namePattern = Core.Nothing,
                         rulesPackageArns = Core.Nothing, startTimeRange = Core.Nothing,
                         stateChangeTimeRange = Core.Nothing, states = Core.Nothing}

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __completedAt__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'completionTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfCompletionTimeRange :: Lens.Lens' AssessmentRunFilter (Core.Maybe Types.TimestampRange)
arfCompletionTimeRange = Lens.field @"completionTimeRange"
{-# INLINEABLE arfCompletionTimeRange #-}
{-# DEPRECATED completionTimeRange "Use generic-lens or generic-optics with 'completionTimeRange' instead"  #-}

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'durationRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfDurationRange :: Lens.Lens' AssessmentRunFilter (Core.Maybe Types.DurationRange)
arfDurationRange = Lens.field @"durationRange"
{-# INLINEABLE arfDurationRange #-}
{-# DEPRECATED durationRange "Use generic-lens or generic-optics with 'durationRange' instead"  #-}

-- | For a record to match a filter, an explicit value or a string containing a wildcard that is specified for this data type property must match the value of the __assessmentRunName__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'namePattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfNamePattern :: Lens.Lens' AssessmentRunFilter (Core.Maybe Types.NamePattern)
arfNamePattern = Lens.field @"namePattern"
{-# INLINEABLE arfNamePattern #-}
{-# DEPRECATED namePattern "Use generic-lens or generic-optics with 'namePattern' instead"  #-}

-- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __rulesPackages__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'rulesPackageArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfRulesPackageArns :: Lens.Lens' AssessmentRunFilter (Core.Maybe [Types.Arn])
arfRulesPackageArns = Lens.field @"rulesPackageArns"
{-# INLINEABLE arfRulesPackageArns #-}
{-# DEPRECATED rulesPackageArns "Use generic-lens or generic-optics with 'rulesPackageArns' instead"  #-}

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __startTime__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'startTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfStartTimeRange :: Lens.Lens' AssessmentRunFilter (Core.Maybe Types.TimestampRange)
arfStartTimeRange = Lens.field @"startTimeRange"
{-# INLINEABLE arfStartTimeRange #-}
{-# DEPRECATED startTimeRange "Use generic-lens or generic-optics with 'startTimeRange' instead"  #-}

-- | For a record to match a filter, the value that is specified for this data type property must match the __stateChangedAt__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'stateChangeTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfStateChangeTimeRange :: Lens.Lens' AssessmentRunFilter (Core.Maybe Types.TimestampRange)
arfStateChangeTimeRange = Lens.field @"stateChangeTimeRange"
{-# INLINEABLE arfStateChangeTimeRange #-}
{-# DEPRECATED stateChangeTimeRange "Use generic-lens or generic-optics with 'stateChangeTimeRange' instead"  #-}

-- | For a record to match a filter, one of the values specified for this data type property must be the exact match of the value of the __assessmentRunState__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'states' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfStates :: Lens.Lens' AssessmentRunFilter (Core.Maybe [Types.AssessmentRunState])
arfStates = Lens.field @"states"
{-# INLINEABLE arfStates #-}
{-# DEPRECATED states "Use generic-lens or generic-optics with 'states' instead"  #-}

instance Core.FromJSON AssessmentRunFilter where
        toJSON AssessmentRunFilter{..}
          = Core.object
              (Core.catMaybes
                 [("completionTimeRange" Core..=) Core.<$> completionTimeRange,
                  ("durationRange" Core..=) Core.<$> durationRange,
                  ("namePattern" Core..=) Core.<$> namePattern,
                  ("rulesPackageArns" Core..=) Core.<$> rulesPackageArns,
                  ("startTimeRange" Core..=) Core.<$> startTimeRange,
                  ("stateChangeTimeRange" Core..=) Core.<$> stateChangeTimeRange,
                  ("states" Core..=) Core.<$> states])
