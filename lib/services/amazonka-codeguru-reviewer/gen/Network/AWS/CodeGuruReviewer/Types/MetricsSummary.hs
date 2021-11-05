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
-- Module      : Network.AWS.CodeGuruReviewer.Types.MetricsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruReviewer.Types.MetricsSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about metrics summaries.
--
-- /See:/ 'newMetricsSummary' smart constructor.
data MetricsSummary = MetricsSummary'
  { -- | Total number of recommendations found in the code review.
    findingsCount :: Prelude.Maybe Prelude.Integer,
    -- | Lines of code metered in the code review. For the initial code review
    -- pull request and all subsequent revisions, this includes all lines of
    -- code in the files added to the pull request. In subsequent revisions,
    -- for files that already existed in the pull request, this includes only
    -- the changed lines of code. In both cases, this does not include non-code
    -- lines such as comments and import statements. For example, if you submit
    -- a pull request containing 5 files, each with 500 lines of code, and in a
    -- subsequent revision you added a new file with 200 lines of code, and
    -- also modified a total of 25 lines across the initial 5 files,
    -- @MeteredLinesOfCodeCount@ includes the first 5 files (5 * 500 = 2,500
    -- lines), the new file (200 lines) and the 25 changed lines of code for a
    -- total of 2,725 lines of code.
    meteredLinesOfCodeCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingsCount', 'metricsSummary_findingsCount' - Total number of recommendations found in the code review.
--
-- 'meteredLinesOfCodeCount', 'metricsSummary_meteredLinesOfCodeCount' - Lines of code metered in the code review. For the initial code review
-- pull request and all subsequent revisions, this includes all lines of
-- code in the files added to the pull request. In subsequent revisions,
-- for files that already existed in the pull request, this includes only
-- the changed lines of code. In both cases, this does not include non-code
-- lines such as comments and import statements. For example, if you submit
-- a pull request containing 5 files, each with 500 lines of code, and in a
-- subsequent revision you added a new file with 200 lines of code, and
-- also modified a total of 25 lines across the initial 5 files,
-- @MeteredLinesOfCodeCount@ includes the first 5 files (5 * 500 = 2,500
-- lines), the new file (200 lines) and the 25 changed lines of code for a
-- total of 2,725 lines of code.
newMetricsSummary ::
  MetricsSummary
newMetricsSummary =
  MetricsSummary'
    { findingsCount = Prelude.Nothing,
      meteredLinesOfCodeCount = Prelude.Nothing
    }

-- | Total number of recommendations found in the code review.
metricsSummary_findingsCount :: Lens.Lens' MetricsSummary (Prelude.Maybe Prelude.Integer)
metricsSummary_findingsCount = Lens.lens (\MetricsSummary' {findingsCount} -> findingsCount) (\s@MetricsSummary' {} a -> s {findingsCount = a} :: MetricsSummary)

-- | Lines of code metered in the code review. For the initial code review
-- pull request and all subsequent revisions, this includes all lines of
-- code in the files added to the pull request. In subsequent revisions,
-- for files that already existed in the pull request, this includes only
-- the changed lines of code. In both cases, this does not include non-code
-- lines such as comments and import statements. For example, if you submit
-- a pull request containing 5 files, each with 500 lines of code, and in a
-- subsequent revision you added a new file with 200 lines of code, and
-- also modified a total of 25 lines across the initial 5 files,
-- @MeteredLinesOfCodeCount@ includes the first 5 files (5 * 500 = 2,500
-- lines), the new file (200 lines) and the 25 changed lines of code for a
-- total of 2,725 lines of code.
metricsSummary_meteredLinesOfCodeCount :: Lens.Lens' MetricsSummary (Prelude.Maybe Prelude.Integer)
metricsSummary_meteredLinesOfCodeCount = Lens.lens (\MetricsSummary' {meteredLinesOfCodeCount} -> meteredLinesOfCodeCount) (\s@MetricsSummary' {} a -> s {meteredLinesOfCodeCount = a} :: MetricsSummary)

instance Core.FromJSON MetricsSummary where
  parseJSON =
    Core.withObject
      "MetricsSummary"
      ( \x ->
          MetricsSummary'
            Prelude.<$> (x Core..:? "FindingsCount")
            Prelude.<*> (x Core..:? "MeteredLinesOfCodeCount")
      )

instance Prelude.Hashable MetricsSummary

instance Prelude.NFData MetricsSummary
