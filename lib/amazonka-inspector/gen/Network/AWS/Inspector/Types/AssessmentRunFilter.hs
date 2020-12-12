{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunFilter
  ( AssessmentRunFilter (..),

    -- * Smart constructor
    mkAssessmentRunFilter,

    -- * Lenses
    arfStates,
    arfNamePattern,
    arfStartTimeRange,
    arfStateChangeTimeRange,
    arfRulesPackageARNs,
    arfCompletionTimeRange,
    arfDurationRange,
  )
where

import Network.AWS.Inspector.Types.AssessmentRunState
import Network.AWS.Inspector.Types.DurationRange
import Network.AWS.Inspector.Types.TimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used as the request parameter in the 'ListAssessmentRuns' action.
--
-- /See:/ 'mkAssessmentRunFilter' smart constructor.
data AssessmentRunFilter = AssessmentRunFilter'
  { states ::
      Lude.Maybe [AssessmentRunState],
    namePattern :: Lude.Maybe Lude.Text,
    startTimeRange :: Lude.Maybe TimestampRange,
    stateChangeTimeRange :: Lude.Maybe TimestampRange,
    rulesPackageARNs :: Lude.Maybe [Lude.Text],
    completionTimeRange :: Lude.Maybe TimestampRange,
    durationRange :: Lude.Maybe DurationRange
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentRunFilter' with the minimum fields required to make a request.
--
-- * 'completionTimeRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __completedAt__ property of the 'AssessmentRun' data type.
-- * 'durationRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentRun' data type.
-- * 'namePattern' - For a record to match a filter, an explicit value or a string containing a wildcard that is specified for this data type property must match the value of the __assessmentRunName__ property of the 'AssessmentRun' data type.
-- * 'rulesPackageARNs' - For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __rulesPackages__ property of the 'AssessmentRun' data type.
-- * 'startTimeRange' - For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __startTime__ property of the 'AssessmentRun' data type.
-- * 'stateChangeTimeRange' - For a record to match a filter, the value that is specified for this data type property must match the __stateChangedAt__ property of the 'AssessmentRun' data type.
-- * 'states' - For a record to match a filter, one of the values specified for this data type property must be the exact match of the value of the __assessmentRunState__ property of the 'AssessmentRun' data type.
mkAssessmentRunFilter ::
  AssessmentRunFilter
mkAssessmentRunFilter =
  AssessmentRunFilter'
    { states = Lude.Nothing,
      namePattern = Lude.Nothing,
      startTimeRange = Lude.Nothing,
      stateChangeTimeRange = Lude.Nothing,
      rulesPackageARNs = Lude.Nothing,
      completionTimeRange = Lude.Nothing,
      durationRange = Lude.Nothing
    }

-- | For a record to match a filter, one of the values specified for this data type property must be the exact match of the value of the __assessmentRunState__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'states' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfStates :: Lens.Lens' AssessmentRunFilter (Lude.Maybe [AssessmentRunState])
arfStates = Lens.lens (states :: AssessmentRunFilter -> Lude.Maybe [AssessmentRunState]) (\s a -> s {states = a} :: AssessmentRunFilter)
{-# DEPRECATED arfStates "Use generic-lens or generic-optics with 'states' instead." #-}

-- | For a record to match a filter, an explicit value or a string containing a wildcard that is specified for this data type property must match the value of the __assessmentRunName__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'namePattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfNamePattern :: Lens.Lens' AssessmentRunFilter (Lude.Maybe Lude.Text)
arfNamePattern = Lens.lens (namePattern :: AssessmentRunFilter -> Lude.Maybe Lude.Text) (\s a -> s {namePattern = a} :: AssessmentRunFilter)
{-# DEPRECATED arfNamePattern "Use generic-lens or generic-optics with 'namePattern' instead." #-}

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __startTime__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'startTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfStartTimeRange :: Lens.Lens' AssessmentRunFilter (Lude.Maybe TimestampRange)
arfStartTimeRange = Lens.lens (startTimeRange :: AssessmentRunFilter -> Lude.Maybe TimestampRange) (\s a -> s {startTimeRange = a} :: AssessmentRunFilter)
{-# DEPRECATED arfStartTimeRange "Use generic-lens or generic-optics with 'startTimeRange' instead." #-}

-- | For a record to match a filter, the value that is specified for this data type property must match the __stateChangedAt__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'stateChangeTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfStateChangeTimeRange :: Lens.Lens' AssessmentRunFilter (Lude.Maybe TimestampRange)
arfStateChangeTimeRange = Lens.lens (stateChangeTimeRange :: AssessmentRunFilter -> Lude.Maybe TimestampRange) (\s a -> s {stateChangeTimeRange = a} :: AssessmentRunFilter)
{-# DEPRECATED arfStateChangeTimeRange "Use generic-lens or generic-optics with 'stateChangeTimeRange' instead." #-}

-- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __rulesPackages__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'rulesPackageARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfRulesPackageARNs :: Lens.Lens' AssessmentRunFilter (Lude.Maybe [Lude.Text])
arfRulesPackageARNs = Lens.lens (rulesPackageARNs :: AssessmentRunFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {rulesPackageARNs = a} :: AssessmentRunFilter)
{-# DEPRECATED arfRulesPackageARNs "Use generic-lens or generic-optics with 'rulesPackageARNs' instead." #-}

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __completedAt__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'completionTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfCompletionTimeRange :: Lens.Lens' AssessmentRunFilter (Lude.Maybe TimestampRange)
arfCompletionTimeRange = Lens.lens (completionTimeRange :: AssessmentRunFilter -> Lude.Maybe TimestampRange) (\s a -> s {completionTimeRange = a} :: AssessmentRunFilter)
{-# DEPRECATED arfCompletionTimeRange "Use generic-lens or generic-optics with 'completionTimeRange' instead." #-}

-- | For a record to match a filter, the value that is specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentRun' data type.
--
-- /Note:/ Consider using 'durationRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arfDurationRange :: Lens.Lens' AssessmentRunFilter (Lude.Maybe DurationRange)
arfDurationRange = Lens.lens (durationRange :: AssessmentRunFilter -> Lude.Maybe DurationRange) (\s a -> s {durationRange = a} :: AssessmentRunFilter)
{-# DEPRECATED arfDurationRange "Use generic-lens or generic-optics with 'durationRange' instead." #-}

instance Lude.ToJSON AssessmentRunFilter where
  toJSON AssessmentRunFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("states" Lude..=) Lude.<$> states,
            ("namePattern" Lude..=) Lude.<$> namePattern,
            ("startTimeRange" Lude..=) Lude.<$> startTimeRange,
            ("stateChangeTimeRange" Lude..=) Lude.<$> stateChangeTimeRange,
            ("rulesPackageArns" Lude..=) Lude.<$> rulesPackageARNs,
            ("completionTimeRange" Lude..=) Lude.<$> completionTimeRange,
            ("durationRange" Lude..=) Lude.<$> durationRange
          ]
      )
