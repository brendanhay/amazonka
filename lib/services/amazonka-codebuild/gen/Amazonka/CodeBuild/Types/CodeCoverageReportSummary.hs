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
-- Module      : Amazonka.CodeBuild.Types.CodeCoverageReportSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.CodeCoverageReportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of a code coverage report.
--
-- Line coverage measures how many statements your tests cover. A statement
-- is a single instruction, not including comments, conditionals, etc.
--
-- Branch coverage determines if your tests cover every possible branch of
-- a control structure, such as an @if@ or @case@ statement.
--
-- /See:/ 'newCodeCoverageReportSummary' smart constructor.
data CodeCoverageReportSummary = CodeCoverageReportSummary'
  { -- | The percentage of branches that are covered by your tests.
    branchCoveragePercentage :: Prelude.Maybe Prelude.Double,
    -- | The number of conditional branches that are covered by your tests.
    branchesCovered :: Prelude.Maybe Prelude.Natural,
    -- | The number of conditional branches that are not covered by your tests.
    branchesMissed :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of lines that are covered by your tests.
    lineCoveragePercentage :: Prelude.Maybe Prelude.Double,
    -- | The number of lines that are covered by your tests.
    linesCovered :: Prelude.Maybe Prelude.Natural,
    -- | The number of lines that are not covered by your tests.
    linesMissed :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeCoverageReportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchCoveragePercentage', 'codeCoverageReportSummary_branchCoveragePercentage' - The percentage of branches that are covered by your tests.
--
-- 'branchesCovered', 'codeCoverageReportSummary_branchesCovered' - The number of conditional branches that are covered by your tests.
--
-- 'branchesMissed', 'codeCoverageReportSummary_branchesMissed' - The number of conditional branches that are not covered by your tests.
--
-- 'lineCoveragePercentage', 'codeCoverageReportSummary_lineCoveragePercentage' - The percentage of lines that are covered by your tests.
--
-- 'linesCovered', 'codeCoverageReportSummary_linesCovered' - The number of lines that are covered by your tests.
--
-- 'linesMissed', 'codeCoverageReportSummary_linesMissed' - The number of lines that are not covered by your tests.
newCodeCoverageReportSummary ::
  CodeCoverageReportSummary
newCodeCoverageReportSummary =
  CodeCoverageReportSummary'
    { branchCoveragePercentage =
        Prelude.Nothing,
      branchesCovered = Prelude.Nothing,
      branchesMissed = Prelude.Nothing,
      lineCoveragePercentage = Prelude.Nothing,
      linesCovered = Prelude.Nothing,
      linesMissed = Prelude.Nothing
    }

-- | The percentage of branches that are covered by your tests.
codeCoverageReportSummary_branchCoveragePercentage :: Lens.Lens' CodeCoverageReportSummary (Prelude.Maybe Prelude.Double)
codeCoverageReportSummary_branchCoveragePercentage = Lens.lens (\CodeCoverageReportSummary' {branchCoveragePercentage} -> branchCoveragePercentage) (\s@CodeCoverageReportSummary' {} a -> s {branchCoveragePercentage = a} :: CodeCoverageReportSummary)

-- | The number of conditional branches that are covered by your tests.
codeCoverageReportSummary_branchesCovered :: Lens.Lens' CodeCoverageReportSummary (Prelude.Maybe Prelude.Natural)
codeCoverageReportSummary_branchesCovered = Lens.lens (\CodeCoverageReportSummary' {branchesCovered} -> branchesCovered) (\s@CodeCoverageReportSummary' {} a -> s {branchesCovered = a} :: CodeCoverageReportSummary)

-- | The number of conditional branches that are not covered by your tests.
codeCoverageReportSummary_branchesMissed :: Lens.Lens' CodeCoverageReportSummary (Prelude.Maybe Prelude.Natural)
codeCoverageReportSummary_branchesMissed = Lens.lens (\CodeCoverageReportSummary' {branchesMissed} -> branchesMissed) (\s@CodeCoverageReportSummary' {} a -> s {branchesMissed = a} :: CodeCoverageReportSummary)

-- | The percentage of lines that are covered by your tests.
codeCoverageReportSummary_lineCoveragePercentage :: Lens.Lens' CodeCoverageReportSummary (Prelude.Maybe Prelude.Double)
codeCoverageReportSummary_lineCoveragePercentage = Lens.lens (\CodeCoverageReportSummary' {lineCoveragePercentage} -> lineCoveragePercentage) (\s@CodeCoverageReportSummary' {} a -> s {lineCoveragePercentage = a} :: CodeCoverageReportSummary)

-- | The number of lines that are covered by your tests.
codeCoverageReportSummary_linesCovered :: Lens.Lens' CodeCoverageReportSummary (Prelude.Maybe Prelude.Natural)
codeCoverageReportSummary_linesCovered = Lens.lens (\CodeCoverageReportSummary' {linesCovered} -> linesCovered) (\s@CodeCoverageReportSummary' {} a -> s {linesCovered = a} :: CodeCoverageReportSummary)

-- | The number of lines that are not covered by your tests.
codeCoverageReportSummary_linesMissed :: Lens.Lens' CodeCoverageReportSummary (Prelude.Maybe Prelude.Natural)
codeCoverageReportSummary_linesMissed = Lens.lens (\CodeCoverageReportSummary' {linesMissed} -> linesMissed) (\s@CodeCoverageReportSummary' {} a -> s {linesMissed = a} :: CodeCoverageReportSummary)

instance Data.FromJSON CodeCoverageReportSummary where
  parseJSON =
    Data.withObject
      "CodeCoverageReportSummary"
      ( \x ->
          CodeCoverageReportSummary'
            Prelude.<$> (x Data..:? "branchCoveragePercentage")
            Prelude.<*> (x Data..:? "branchesCovered")
            Prelude.<*> (x Data..:? "branchesMissed")
            Prelude.<*> (x Data..:? "lineCoveragePercentage")
            Prelude.<*> (x Data..:? "linesCovered")
            Prelude.<*> (x Data..:? "linesMissed")
      )

instance Prelude.Hashable CodeCoverageReportSummary where
  hashWithSalt _salt CodeCoverageReportSummary' {..} =
    _salt
      `Prelude.hashWithSalt` branchCoveragePercentage
      `Prelude.hashWithSalt` branchesCovered
      `Prelude.hashWithSalt` branchesMissed
      `Prelude.hashWithSalt` lineCoveragePercentage
      `Prelude.hashWithSalt` linesCovered
      `Prelude.hashWithSalt` linesMissed

instance Prelude.NFData CodeCoverageReportSummary where
  rnf CodeCoverageReportSummary' {..} =
    Prelude.rnf branchCoveragePercentage
      `Prelude.seq` Prelude.rnf branchesCovered
      `Prelude.seq` Prelude.rnf branchesMissed
      `Prelude.seq` Prelude.rnf lineCoveragePercentage
      `Prelude.seq` Prelude.rnf linesCovered
      `Prelude.seq` Prelude.rnf linesMissed
