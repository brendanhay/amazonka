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
-- Module      : Amazonka.Inspector.Types.AssessmentRunFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AssessmentRunFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.AssessmentRunState
import Amazonka.Inspector.Types.DurationRange
import Amazonka.Inspector.Types.TimestampRange
import qualified Amazonka.Prelude as Prelude

-- | Used as the request parameter in the ListAssessmentRuns action.
--
-- /See:/ 'newAssessmentRunFilter' smart constructor.
data AssessmentRunFilter = AssessmentRunFilter'
  { -- | For a record to match a filter, the value that is specified for this
    -- data type property must inclusively match any value between the
    -- specified minimum and maximum values of the __completedAt__ property of
    -- the AssessmentRun data type.
    completionTimeRange :: Prelude.Maybe TimestampRange,
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must inclusively match any value between the
    -- specified minimum and maximum values of the __durationInSeconds__
    -- property of the AssessmentRun data type.
    durationRange :: Prelude.Maybe DurationRange,
    -- | For a record to match a filter, an explicit value or a string containing
    -- a wildcard that is specified for this data type property must match the
    -- value of the __assessmentRunName__ property of the AssessmentRun data
    -- type.
    namePattern :: Prelude.Maybe Prelude.Text,
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must be contained in the list of values of the
    -- __rulesPackages__ property of the AssessmentRun data type.
    rulesPackageArns :: Prelude.Maybe [Prelude.Text],
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must inclusively match any value between the
    -- specified minimum and maximum values of the __startTime__ property of
    -- the AssessmentRun data type.
    startTimeRange :: Prelude.Maybe TimestampRange,
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must match the __stateChangedAt__ property of the
    -- AssessmentRun data type.
    stateChangeTimeRange :: Prelude.Maybe TimestampRange,
    -- | For a record to match a filter, one of the values specified for this
    -- data type property must be the exact match of the value of the
    -- __assessmentRunState__ property of the AssessmentRun data type.
    states :: Prelude.Maybe [AssessmentRunState]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentRunFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTimeRange', 'assessmentRunFilter_completionTimeRange' - For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __completedAt__ property of
-- the AssessmentRun data type.
--
-- 'durationRange', 'assessmentRunFilter_durationRange' - For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __durationInSeconds__
-- property of the AssessmentRun data type.
--
-- 'namePattern', 'assessmentRunFilter_namePattern' - For a record to match a filter, an explicit value or a string containing
-- a wildcard that is specified for this data type property must match the
-- value of the __assessmentRunName__ property of the AssessmentRun data
-- type.
--
-- 'rulesPackageArns', 'assessmentRunFilter_rulesPackageArns' - For a record to match a filter, the value that is specified for this
-- data type property must be contained in the list of values of the
-- __rulesPackages__ property of the AssessmentRun data type.
--
-- 'startTimeRange', 'assessmentRunFilter_startTimeRange' - For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __startTime__ property of
-- the AssessmentRun data type.
--
-- 'stateChangeTimeRange', 'assessmentRunFilter_stateChangeTimeRange' - For a record to match a filter, the value that is specified for this
-- data type property must match the __stateChangedAt__ property of the
-- AssessmentRun data type.
--
-- 'states', 'assessmentRunFilter_states' - For a record to match a filter, one of the values specified for this
-- data type property must be the exact match of the value of the
-- __assessmentRunState__ property of the AssessmentRun data type.
newAssessmentRunFilter ::
  AssessmentRunFilter
newAssessmentRunFilter =
  AssessmentRunFilter'
    { completionTimeRange =
        Prelude.Nothing,
      durationRange = Prelude.Nothing,
      namePattern = Prelude.Nothing,
      rulesPackageArns = Prelude.Nothing,
      startTimeRange = Prelude.Nothing,
      stateChangeTimeRange = Prelude.Nothing,
      states = Prelude.Nothing
    }

-- | For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __completedAt__ property of
-- the AssessmentRun data type.
assessmentRunFilter_completionTimeRange :: Lens.Lens' AssessmentRunFilter (Prelude.Maybe TimestampRange)
assessmentRunFilter_completionTimeRange = Lens.lens (\AssessmentRunFilter' {completionTimeRange} -> completionTimeRange) (\s@AssessmentRunFilter' {} a -> s {completionTimeRange = a} :: AssessmentRunFilter)

-- | For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __durationInSeconds__
-- property of the AssessmentRun data type.
assessmentRunFilter_durationRange :: Lens.Lens' AssessmentRunFilter (Prelude.Maybe DurationRange)
assessmentRunFilter_durationRange = Lens.lens (\AssessmentRunFilter' {durationRange} -> durationRange) (\s@AssessmentRunFilter' {} a -> s {durationRange = a} :: AssessmentRunFilter)

-- | For a record to match a filter, an explicit value or a string containing
-- a wildcard that is specified for this data type property must match the
-- value of the __assessmentRunName__ property of the AssessmentRun data
-- type.
assessmentRunFilter_namePattern :: Lens.Lens' AssessmentRunFilter (Prelude.Maybe Prelude.Text)
assessmentRunFilter_namePattern = Lens.lens (\AssessmentRunFilter' {namePattern} -> namePattern) (\s@AssessmentRunFilter' {} a -> s {namePattern = a} :: AssessmentRunFilter)

-- | For a record to match a filter, the value that is specified for this
-- data type property must be contained in the list of values of the
-- __rulesPackages__ property of the AssessmentRun data type.
assessmentRunFilter_rulesPackageArns :: Lens.Lens' AssessmentRunFilter (Prelude.Maybe [Prelude.Text])
assessmentRunFilter_rulesPackageArns = Lens.lens (\AssessmentRunFilter' {rulesPackageArns} -> rulesPackageArns) (\s@AssessmentRunFilter' {} a -> s {rulesPackageArns = a} :: AssessmentRunFilter) Prelude.. Lens.mapping Lens.coerced

-- | For a record to match a filter, the value that is specified for this
-- data type property must inclusively match any value between the
-- specified minimum and maximum values of the __startTime__ property of
-- the AssessmentRun data type.
assessmentRunFilter_startTimeRange :: Lens.Lens' AssessmentRunFilter (Prelude.Maybe TimestampRange)
assessmentRunFilter_startTimeRange = Lens.lens (\AssessmentRunFilter' {startTimeRange} -> startTimeRange) (\s@AssessmentRunFilter' {} a -> s {startTimeRange = a} :: AssessmentRunFilter)

-- | For a record to match a filter, the value that is specified for this
-- data type property must match the __stateChangedAt__ property of the
-- AssessmentRun data type.
assessmentRunFilter_stateChangeTimeRange :: Lens.Lens' AssessmentRunFilter (Prelude.Maybe TimestampRange)
assessmentRunFilter_stateChangeTimeRange = Lens.lens (\AssessmentRunFilter' {stateChangeTimeRange} -> stateChangeTimeRange) (\s@AssessmentRunFilter' {} a -> s {stateChangeTimeRange = a} :: AssessmentRunFilter)

-- | For a record to match a filter, one of the values specified for this
-- data type property must be the exact match of the value of the
-- __assessmentRunState__ property of the AssessmentRun data type.
assessmentRunFilter_states :: Lens.Lens' AssessmentRunFilter (Prelude.Maybe [AssessmentRunState])
assessmentRunFilter_states = Lens.lens (\AssessmentRunFilter' {states} -> states) (\s@AssessmentRunFilter' {} a -> s {states = a} :: AssessmentRunFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AssessmentRunFilter where
  hashWithSalt _salt AssessmentRunFilter' {..} =
    _salt
      `Prelude.hashWithSalt` completionTimeRange
      `Prelude.hashWithSalt` durationRange
      `Prelude.hashWithSalt` namePattern
      `Prelude.hashWithSalt` rulesPackageArns
      `Prelude.hashWithSalt` startTimeRange
      `Prelude.hashWithSalt` stateChangeTimeRange
      `Prelude.hashWithSalt` states

instance Prelude.NFData AssessmentRunFilter where
  rnf AssessmentRunFilter' {..} =
    Prelude.rnf completionTimeRange
      `Prelude.seq` Prelude.rnf durationRange
      `Prelude.seq` Prelude.rnf namePattern
      `Prelude.seq` Prelude.rnf rulesPackageArns
      `Prelude.seq` Prelude.rnf startTimeRange
      `Prelude.seq` Prelude.rnf stateChangeTimeRange
      `Prelude.seq` Prelude.rnf states

instance Data.ToJSON AssessmentRunFilter where
  toJSON AssessmentRunFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("completionTimeRange" Data..=)
              Prelude.<$> completionTimeRange,
            ("durationRange" Data..=) Prelude.<$> durationRange,
            ("namePattern" Data..=) Prelude.<$> namePattern,
            ("rulesPackageArns" Data..=)
              Prelude.<$> rulesPackageArns,
            ("startTimeRange" Data..=)
              Prelude.<$> startTimeRange,
            ("stateChangeTimeRange" Data..=)
              Prelude.<$> stateChangeTimeRange,
            ("states" Data..=) Prelude.<$> states
          ]
      )
