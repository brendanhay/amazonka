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
-- Module      : Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CodeCoverageReportSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
  { -- | The number of conditional branches that are not covered by your tests.
    branchesMissed :: Core.Maybe Core.Natural,
    -- | The number of lines that are covered by your tests.
    linesCovered :: Core.Maybe Core.Natural,
    -- | The number of conditional branches that are covered by your tests.
    branchesCovered :: Core.Maybe Core.Natural,
    -- | The percentage of lines that are covered by your tests.
    lineCoveragePercentage :: Core.Maybe Core.Double,
    -- | The number of lines that are not covered by your tests.
    linesMissed :: Core.Maybe Core.Natural,
    -- | The percentage of branches that are covered by your tests.
    branchCoveragePercentage :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeCoverageReportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchesMissed', 'codeCoverageReportSummary_branchesMissed' - The number of conditional branches that are not covered by your tests.
--
-- 'linesCovered', 'codeCoverageReportSummary_linesCovered' - The number of lines that are covered by your tests.
--
-- 'branchesCovered', 'codeCoverageReportSummary_branchesCovered' - The number of conditional branches that are covered by your tests.
--
-- 'lineCoveragePercentage', 'codeCoverageReportSummary_lineCoveragePercentage' - The percentage of lines that are covered by your tests.
--
-- 'linesMissed', 'codeCoverageReportSummary_linesMissed' - The number of lines that are not covered by your tests.
--
-- 'branchCoveragePercentage', 'codeCoverageReportSummary_branchCoveragePercentage' - The percentage of branches that are covered by your tests.
newCodeCoverageReportSummary ::
  CodeCoverageReportSummary
newCodeCoverageReportSummary =
  CodeCoverageReportSummary'
    { branchesMissed =
        Core.Nothing,
      linesCovered = Core.Nothing,
      branchesCovered = Core.Nothing,
      lineCoveragePercentage = Core.Nothing,
      linesMissed = Core.Nothing,
      branchCoveragePercentage = Core.Nothing
    }

-- | The number of conditional branches that are not covered by your tests.
codeCoverageReportSummary_branchesMissed :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Natural)
codeCoverageReportSummary_branchesMissed = Lens.lens (\CodeCoverageReportSummary' {branchesMissed} -> branchesMissed) (\s@CodeCoverageReportSummary' {} a -> s {branchesMissed = a} :: CodeCoverageReportSummary)

-- | The number of lines that are covered by your tests.
codeCoverageReportSummary_linesCovered :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Natural)
codeCoverageReportSummary_linesCovered = Lens.lens (\CodeCoverageReportSummary' {linesCovered} -> linesCovered) (\s@CodeCoverageReportSummary' {} a -> s {linesCovered = a} :: CodeCoverageReportSummary)

-- | The number of conditional branches that are covered by your tests.
codeCoverageReportSummary_branchesCovered :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Natural)
codeCoverageReportSummary_branchesCovered = Lens.lens (\CodeCoverageReportSummary' {branchesCovered} -> branchesCovered) (\s@CodeCoverageReportSummary' {} a -> s {branchesCovered = a} :: CodeCoverageReportSummary)

-- | The percentage of lines that are covered by your tests.
codeCoverageReportSummary_lineCoveragePercentage :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Double)
codeCoverageReportSummary_lineCoveragePercentage = Lens.lens (\CodeCoverageReportSummary' {lineCoveragePercentage} -> lineCoveragePercentage) (\s@CodeCoverageReportSummary' {} a -> s {lineCoveragePercentage = a} :: CodeCoverageReportSummary)

-- | The number of lines that are not covered by your tests.
codeCoverageReportSummary_linesMissed :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Natural)
codeCoverageReportSummary_linesMissed = Lens.lens (\CodeCoverageReportSummary' {linesMissed} -> linesMissed) (\s@CodeCoverageReportSummary' {} a -> s {linesMissed = a} :: CodeCoverageReportSummary)

-- | The percentage of branches that are covered by your tests.
codeCoverageReportSummary_branchCoveragePercentage :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Double)
codeCoverageReportSummary_branchCoveragePercentage = Lens.lens (\CodeCoverageReportSummary' {branchCoveragePercentage} -> branchCoveragePercentage) (\s@CodeCoverageReportSummary' {} a -> s {branchCoveragePercentage = a} :: CodeCoverageReportSummary)

instance Core.FromJSON CodeCoverageReportSummary where
  parseJSON =
    Core.withObject
      "CodeCoverageReportSummary"
      ( \x ->
          CodeCoverageReportSummary'
            Core.<$> (x Core..:? "branchesMissed")
            Core.<*> (x Core..:? "linesCovered")
            Core.<*> (x Core..:? "branchesCovered")
            Core.<*> (x Core..:? "lineCoveragePercentage")
            Core.<*> (x Core..:? "linesMissed")
            Core.<*> (x Core..:? "branchCoveragePercentage")
      )

instance Core.Hashable CodeCoverageReportSummary

instance Core.NFData CodeCoverageReportSummary
