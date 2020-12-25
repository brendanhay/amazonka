{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
  ( CodeCoverageReportSummary (..),

    -- * Smart constructor
    mkCodeCoverageReportSummary,

    -- * Lenses
    ccrsBranchCoveragePercentage,
    ccrsBranchesCovered,
    ccrsBranchesMissed,
    ccrsLineCoveragePercentage,
    ccrsLinesCovered,
    ccrsLinesMissed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a summary of a code coverage report.
--
-- Line coverage measures how many statements your tests cover. A statement is a single instruction, not including comments, conditionals, etc.
-- Branch coverage determines if your tests cover every possible branch of a control structure, such as an @if@ or @case@ statement.
--
-- /See:/ 'mkCodeCoverageReportSummary' smart constructor.
data CodeCoverageReportSummary = CodeCoverageReportSummary'
  { -- | The percentage of branches that are covered by your tests.
    branchCoveragePercentage :: Core.Maybe Core.Double,
    -- | The number of conditional branches that are covered by your tests.
    branchesCovered :: Core.Maybe Core.Natural,
    -- | The number of conditional branches that are not covered by your tests.
    branchesMissed :: Core.Maybe Core.Natural,
    -- | The percentage of lines that are covered by your tests.
    lineCoveragePercentage :: Core.Maybe Core.Double,
    -- | The number of lines that are covered by your tests.
    linesCovered :: Core.Maybe Core.Natural,
    -- | The number of lines that are not covered by your tests.
    linesMissed :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeCoverageReportSummary' value with any optional fields omitted.
mkCodeCoverageReportSummary ::
  CodeCoverageReportSummary
mkCodeCoverageReportSummary =
  CodeCoverageReportSummary'
    { branchCoveragePercentage =
        Core.Nothing,
      branchesCovered = Core.Nothing,
      branchesMissed = Core.Nothing,
      lineCoveragePercentage = Core.Nothing,
      linesCovered = Core.Nothing,
      linesMissed = Core.Nothing
    }

-- | The percentage of branches that are covered by your tests.
--
-- /Note:/ Consider using 'branchCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsBranchCoveragePercentage :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Double)
ccrsBranchCoveragePercentage = Lens.field @"branchCoveragePercentage"
{-# DEPRECATED ccrsBranchCoveragePercentage "Use generic-lens or generic-optics with 'branchCoveragePercentage' instead." #-}

-- | The number of conditional branches that are covered by your tests.
--
-- /Note:/ Consider using 'branchesCovered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsBranchesCovered :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Natural)
ccrsBranchesCovered = Lens.field @"branchesCovered"
{-# DEPRECATED ccrsBranchesCovered "Use generic-lens or generic-optics with 'branchesCovered' instead." #-}

-- | The number of conditional branches that are not covered by your tests.
--
-- /Note:/ Consider using 'branchesMissed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsBranchesMissed :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Natural)
ccrsBranchesMissed = Lens.field @"branchesMissed"
{-# DEPRECATED ccrsBranchesMissed "Use generic-lens or generic-optics with 'branchesMissed' instead." #-}

-- | The percentage of lines that are covered by your tests.
--
-- /Note:/ Consider using 'lineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsLineCoveragePercentage :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Double)
ccrsLineCoveragePercentage = Lens.field @"lineCoveragePercentage"
{-# DEPRECATED ccrsLineCoveragePercentage "Use generic-lens or generic-optics with 'lineCoveragePercentage' instead." #-}

-- | The number of lines that are covered by your tests.
--
-- /Note:/ Consider using 'linesCovered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsLinesCovered :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Natural)
ccrsLinesCovered = Lens.field @"linesCovered"
{-# DEPRECATED ccrsLinesCovered "Use generic-lens or generic-optics with 'linesCovered' instead." #-}

-- | The number of lines that are not covered by your tests.
--
-- /Note:/ Consider using 'linesMissed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsLinesMissed :: Lens.Lens' CodeCoverageReportSummary (Core.Maybe Core.Natural)
ccrsLinesMissed = Lens.field @"linesMissed"
{-# DEPRECATED ccrsLinesMissed "Use generic-lens or generic-optics with 'linesMissed' instead." #-}

instance Core.FromJSON CodeCoverageReportSummary where
  parseJSON =
    Core.withObject "CodeCoverageReportSummary" Core.$
      \x ->
        CodeCoverageReportSummary'
          Core.<$> (x Core..:? "branchCoveragePercentage")
          Core.<*> (x Core..:? "branchesCovered")
          Core.<*> (x Core..:? "branchesMissed")
          Core.<*> (x Core..:? "lineCoveragePercentage")
          Core.<*> (x Core..:? "linesCovered")
          Core.<*> (x Core..:? "linesMissed")
