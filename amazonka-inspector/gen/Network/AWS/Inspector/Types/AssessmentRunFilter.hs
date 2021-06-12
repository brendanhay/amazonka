{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.AssessmentRunState
import Network.AWS.Inspector.Types.DurationRange
import Network.AWS.Inspector.Types.TimestampRange
import qualified Network.AWS.Lens as Lens

-- | Used as the request parameter in the ListAssessmentRuns action.
--
-- /See:/ 'newAssessmentRunFilter' smart constructor.
data AssessmentRunFilter = AssessmentRunFilter'
  { -- | For a record to match a filter, one of the values specified for this
    -- data type property must be the exact match of the value of the
    -- __assessmentRunState__ property of the AssessmentRun data type.
    states :: Core.Maybe [AssessmentRunState],
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must be contained in the list of values of the
    -- __rulesPackages__ property of the AssessmentRun data type.
    rulesPackageArns :: Core.Maybe [Core.Text],
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must inclusively match any value between the
    -- specified minimum and maximum values of the __durationInSeconds__
    -- property of the AssessmentRun data type.
    durationRange :: Core.Maybe DurationRange,
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must match the __stateChangedAt__ property of the
    -- AssessmentRun data type.
    stateChangeTimeRange :: Core.Maybe TimestampRange,
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must inclusively match any value between the
    -- specified minimum and maximum values of the __startTime__ property of
    -- the AssessmentRun data type.
    startTimeRange :: Core.Maybe TimestampRange,
    -- | For a record to match a filter, an explicit value or a string containing
    -- a wildcard that is specified for this data type property must match the
    -- value of the __assessmentRunName__ property of the AssessmentRun data
    -- type.
    namePattern :: Core.Maybe Core.Text,
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must inclusively match any value between the
    -- specified minimum and maximum values of the __completedAt__ property of
    -- the AssessmentRun data type.
    completionTimeRange :: Core.Maybe TimestampRange
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssessmentRunFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'states', 'assessmentRunFilter_states' - For a record to match a filter, one of the values specified for this
-- data type property must be the exact match of the value of the
-- __assessmentRunState__ property of the AssessmentRun data type.
--
-- 'rulesPackageArns', 'assessmentRunFilter_rulesPackageArns' - For a record to match a filter, the value that is specified for this
-- data type property must be contained in the list of values of the
-- __rulesPackages__ property of the AssessmentRun data type.
--
-- 'durationRange', 'assessmentRunFilter_durationRange' - For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __durationInSeconds__
-- property of the AssessmentRun data type.
--
-- 'stateChangeTimeRange', 'assessmentRunFilter_stateChangeTimeRange' - For a record to match a filter, the value that is specified for this
-- data type property must match the __stateChangedAt__ property of the
-- AssessmentRun data type.
--
-- 'startTimeRange', 'assessmentRunFilter_startTimeRange' - For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __startTime__ property of
-- the AssessmentRun data type.
--
-- 'namePattern', 'assessmentRunFilter_namePattern' - For a record to match a filter, an explicit value or a string containing
-- a wildcard that is specified for this data type property must match the
-- value of the __assessmentRunName__ property of the AssessmentRun data
-- type.
--
-- 'completionTimeRange', 'assessmentRunFilter_completionTimeRange' - For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __completedAt__ property of
-- the AssessmentRun data type.
newAssessmentRunFilter ::
  AssessmentRunFilter
newAssessmentRunFilter =
  AssessmentRunFilter'
    { states = Core.Nothing,
      rulesPackageArns = Core.Nothing,
      durationRange = Core.Nothing,
      stateChangeTimeRange = Core.Nothing,
      startTimeRange = Core.Nothing,
      namePattern = Core.Nothing,
      completionTimeRange = Core.Nothing
    }

-- | For a record to match a filter, one of the values specified for this
-- data type property must be the exact match of the value of the
-- __assessmentRunState__ property of the AssessmentRun data type.
assessmentRunFilter_states :: Lens.Lens' AssessmentRunFilter (Core.Maybe [AssessmentRunState])
assessmentRunFilter_states = Lens.lens (\AssessmentRunFilter' {states} -> states) (\s@AssessmentRunFilter' {} a -> s {states = a} :: AssessmentRunFilter) Core.. Lens.mapping Lens._Coerce

-- | For a record to match a filter, the value that is specified for this
-- data type property must be contained in the list of values of the
-- __rulesPackages__ property of the AssessmentRun data type.
assessmentRunFilter_rulesPackageArns :: Lens.Lens' AssessmentRunFilter (Core.Maybe [Core.Text])
assessmentRunFilter_rulesPackageArns = Lens.lens (\AssessmentRunFilter' {rulesPackageArns} -> rulesPackageArns) (\s@AssessmentRunFilter' {} a -> s {rulesPackageArns = a} :: AssessmentRunFilter) Core.. Lens.mapping Lens._Coerce

-- | For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __durationInSeconds__
-- property of the AssessmentRun data type.
assessmentRunFilter_durationRange :: Lens.Lens' AssessmentRunFilter (Core.Maybe DurationRange)
assessmentRunFilter_durationRange = Lens.lens (\AssessmentRunFilter' {durationRange} -> durationRange) (\s@AssessmentRunFilter' {} a -> s {durationRange = a} :: AssessmentRunFilter)

-- | For a record to match a filter, the value that is specified for this
-- data type property must match the __stateChangedAt__ property of the
-- AssessmentRun data type.
assessmentRunFilter_stateChangeTimeRange :: Lens.Lens' AssessmentRunFilter (Core.Maybe TimestampRange)
assessmentRunFilter_stateChangeTimeRange = Lens.lens (\AssessmentRunFilter' {stateChangeTimeRange} -> stateChangeTimeRange) (\s@AssessmentRunFilter' {} a -> s {stateChangeTimeRange = a} :: AssessmentRunFilter)

-- | For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __startTime__ property of
-- the AssessmentRun data type.
assessmentRunFilter_startTimeRange :: Lens.Lens' AssessmentRunFilter (Core.Maybe TimestampRange)
assessmentRunFilter_startTimeRange = Lens.lens (\AssessmentRunFilter' {startTimeRange} -> startTimeRange) (\s@AssessmentRunFilter' {} a -> s {startTimeRange = a} :: AssessmentRunFilter)

-- | For a record to match a filter, an explicit value or a string containing
-- a wildcard that is specified for this data type property must match the
-- value of the __assessmentRunName__ property of the AssessmentRun data
-- type.
assessmentRunFilter_namePattern :: Lens.Lens' AssessmentRunFilter (Core.Maybe Core.Text)
assessmentRunFilter_namePattern = Lens.lens (\AssessmentRunFilter' {namePattern} -> namePattern) (\s@AssessmentRunFilter' {} a -> s {namePattern = a} :: AssessmentRunFilter)

-- | For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __completedAt__ property of
-- the AssessmentRun data type.
assessmentRunFilter_completionTimeRange :: Lens.Lens' AssessmentRunFilter (Core.Maybe TimestampRange)
assessmentRunFilter_completionTimeRange = Lens.lens (\AssessmentRunFilter' {completionTimeRange} -> completionTimeRange) (\s@AssessmentRunFilter' {} a -> s {completionTimeRange = a} :: AssessmentRunFilter)

instance Core.Hashable AssessmentRunFilter

instance Core.NFData AssessmentRunFilter

instance Core.ToJSON AssessmentRunFilter where
  toJSON AssessmentRunFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("states" Core..=) Core.<$> states,
            ("rulesPackageArns" Core..=)
              Core.<$> rulesPackageArns,
            ("durationRange" Core..=) Core.<$> durationRange,
            ("stateChangeTimeRange" Core..=)
              Core.<$> stateChangeTimeRange,
            ("startTimeRange" Core..=) Core.<$> startTimeRange,
            ("namePattern" Core..=) Core.<$> namePattern,
            ("completionTimeRange" Core..=)
              Core.<$> completionTimeRange
          ]
      )
