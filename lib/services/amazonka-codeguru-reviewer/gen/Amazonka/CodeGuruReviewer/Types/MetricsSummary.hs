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
-- Module      : Amazonka.CodeGuruReviewer.Types.MetricsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.MetricsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    meteredLinesOfCodeCount :: Prelude.Maybe Prelude.Integer,
    -- | Lines of code suppressed in the code review based on the @excludeFiles@
    -- element in the @aws-codeguru-reviewer.yml@ file. For full repository
    -- analyses, this number includes all lines of code in the files that are
    -- suppressed. For pull requests, this number only includes the /changed/
    -- lines of code that are suppressed. In both cases, this number does not
    -- include non-code lines such as comments and import statements. For
    -- example, if you initiate a full repository analysis on a repository
    -- containing 5 files, each file with 100 lines of code, and 2 files are
    -- listed as excluded in the @aws-codeguru-reviewer.yml@ file, then
    -- @SuppressedLinesOfCodeCount@ returns 200 (2 * 100) as the total number
    -- of lines of code suppressed. However, if you submit a pull request for
    -- the same repository, then @SuppressedLinesOfCodeCount@ only includes the
    -- lines in the 2 files that changed. If only 1 of the 2 files changed in
    -- the pull request, then @SuppressedLinesOfCodeCount@ returns 100 (1 *
    -- 100) as the total number of lines of code suppressed.
    suppressedLinesOfCodeCount :: Prelude.Maybe Prelude.Integer
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
--
-- 'suppressedLinesOfCodeCount', 'metricsSummary_suppressedLinesOfCodeCount' - Lines of code suppressed in the code review based on the @excludeFiles@
-- element in the @aws-codeguru-reviewer.yml@ file. For full repository
-- analyses, this number includes all lines of code in the files that are
-- suppressed. For pull requests, this number only includes the /changed/
-- lines of code that are suppressed. In both cases, this number does not
-- include non-code lines such as comments and import statements. For
-- example, if you initiate a full repository analysis on a repository
-- containing 5 files, each file with 100 lines of code, and 2 files are
-- listed as excluded in the @aws-codeguru-reviewer.yml@ file, then
-- @SuppressedLinesOfCodeCount@ returns 200 (2 * 100) as the total number
-- of lines of code suppressed. However, if you submit a pull request for
-- the same repository, then @SuppressedLinesOfCodeCount@ only includes the
-- lines in the 2 files that changed. If only 1 of the 2 files changed in
-- the pull request, then @SuppressedLinesOfCodeCount@ returns 100 (1 *
-- 100) as the total number of lines of code suppressed.
newMetricsSummary ::
  MetricsSummary
newMetricsSummary =
  MetricsSummary'
    { findingsCount = Prelude.Nothing,
      meteredLinesOfCodeCount = Prelude.Nothing,
      suppressedLinesOfCodeCount = Prelude.Nothing
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

-- | Lines of code suppressed in the code review based on the @excludeFiles@
-- element in the @aws-codeguru-reviewer.yml@ file. For full repository
-- analyses, this number includes all lines of code in the files that are
-- suppressed. For pull requests, this number only includes the /changed/
-- lines of code that are suppressed. In both cases, this number does not
-- include non-code lines such as comments and import statements. For
-- example, if you initiate a full repository analysis on a repository
-- containing 5 files, each file with 100 lines of code, and 2 files are
-- listed as excluded in the @aws-codeguru-reviewer.yml@ file, then
-- @SuppressedLinesOfCodeCount@ returns 200 (2 * 100) as the total number
-- of lines of code suppressed. However, if you submit a pull request for
-- the same repository, then @SuppressedLinesOfCodeCount@ only includes the
-- lines in the 2 files that changed. If only 1 of the 2 files changed in
-- the pull request, then @SuppressedLinesOfCodeCount@ returns 100 (1 *
-- 100) as the total number of lines of code suppressed.
metricsSummary_suppressedLinesOfCodeCount :: Lens.Lens' MetricsSummary (Prelude.Maybe Prelude.Integer)
metricsSummary_suppressedLinesOfCodeCount = Lens.lens (\MetricsSummary' {suppressedLinesOfCodeCount} -> suppressedLinesOfCodeCount) (\s@MetricsSummary' {} a -> s {suppressedLinesOfCodeCount = a} :: MetricsSummary)

instance Data.FromJSON MetricsSummary where
  parseJSON =
    Data.withObject
      "MetricsSummary"
      ( \x ->
          MetricsSummary'
            Prelude.<$> (x Data..:? "FindingsCount")
            Prelude.<*> (x Data..:? "MeteredLinesOfCodeCount")
            Prelude.<*> (x Data..:? "SuppressedLinesOfCodeCount")
      )

instance Prelude.Hashable MetricsSummary where
  hashWithSalt _salt MetricsSummary' {..} =
    _salt
      `Prelude.hashWithSalt` findingsCount
      `Prelude.hashWithSalt` meteredLinesOfCodeCount
      `Prelude.hashWithSalt` suppressedLinesOfCodeCount

instance Prelude.NFData MetricsSummary where
  rnf MetricsSummary' {..} =
    Prelude.rnf findingsCount `Prelude.seq`
      Prelude.rnf meteredLinesOfCodeCount `Prelude.seq`
        Prelude.rnf suppressedLinesOfCodeCount
