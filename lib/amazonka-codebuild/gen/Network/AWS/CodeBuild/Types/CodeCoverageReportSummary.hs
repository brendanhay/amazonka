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
    ccrsBranchesMissed,
    ccrsLinesMissed,
    ccrsBranchesCovered,
    ccrsLinesCovered,
    ccrsBranchCoveragePercentage,
    ccrsLineCoveragePercentage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a summary of a code coverage report.
--
-- Line coverage measures how many statements your tests cover. A statement is a single instruction, not including comments, conditionals, etc.
-- Branch coverage determines if your tests cover every possible branch of a control structure, such as an @if@ or @case@ statement.
--
-- /See:/ 'mkCodeCoverageReportSummary' smart constructor.
data CodeCoverageReportSummary = CodeCoverageReportSummary'
  { branchesMissed ::
      Lude.Maybe Lude.Natural,
    linesMissed :: Lude.Maybe Lude.Natural,
    branchesCovered ::
      Lude.Maybe Lude.Natural,
    linesCovered :: Lude.Maybe Lude.Natural,
    branchCoveragePercentage ::
      Lude.Maybe Lude.Double,
    lineCoveragePercentage ::
      Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeCoverageReportSummary' with the minimum fields required to make a request.
--
-- * 'branchCoveragePercentage' - The percentage of branches that are covered by your tests.
-- * 'branchesCovered' - The number of conditional branches that are covered by your tests.
-- * 'branchesMissed' - The number of conditional branches that are not covered by your tests.
-- * 'lineCoveragePercentage' - The percentage of lines that are covered by your tests.
-- * 'linesCovered' - The number of lines that are covered by your tests.
-- * 'linesMissed' - The number of lines that are not covered by your tests.
mkCodeCoverageReportSummary ::
  CodeCoverageReportSummary
mkCodeCoverageReportSummary =
  CodeCoverageReportSummary'
    { branchesMissed = Lude.Nothing,
      linesMissed = Lude.Nothing,
      branchesCovered = Lude.Nothing,
      linesCovered = Lude.Nothing,
      branchCoveragePercentage = Lude.Nothing,
      lineCoveragePercentage = Lude.Nothing
    }

-- | The number of conditional branches that are not covered by your tests.
--
-- /Note:/ Consider using 'branchesMissed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsBranchesMissed :: Lens.Lens' CodeCoverageReportSummary (Lude.Maybe Lude.Natural)
ccrsBranchesMissed = Lens.lens (branchesMissed :: CodeCoverageReportSummary -> Lude.Maybe Lude.Natural) (\s a -> s {branchesMissed = a} :: CodeCoverageReportSummary)
{-# DEPRECATED ccrsBranchesMissed "Use generic-lens or generic-optics with 'branchesMissed' instead." #-}

-- | The number of lines that are not covered by your tests.
--
-- /Note:/ Consider using 'linesMissed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsLinesMissed :: Lens.Lens' CodeCoverageReportSummary (Lude.Maybe Lude.Natural)
ccrsLinesMissed = Lens.lens (linesMissed :: CodeCoverageReportSummary -> Lude.Maybe Lude.Natural) (\s a -> s {linesMissed = a} :: CodeCoverageReportSummary)
{-# DEPRECATED ccrsLinesMissed "Use generic-lens or generic-optics with 'linesMissed' instead." #-}

-- | The number of conditional branches that are covered by your tests.
--
-- /Note:/ Consider using 'branchesCovered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsBranchesCovered :: Lens.Lens' CodeCoverageReportSummary (Lude.Maybe Lude.Natural)
ccrsBranchesCovered = Lens.lens (branchesCovered :: CodeCoverageReportSummary -> Lude.Maybe Lude.Natural) (\s a -> s {branchesCovered = a} :: CodeCoverageReportSummary)
{-# DEPRECATED ccrsBranchesCovered "Use generic-lens or generic-optics with 'branchesCovered' instead." #-}

-- | The number of lines that are covered by your tests.
--
-- /Note:/ Consider using 'linesCovered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsLinesCovered :: Lens.Lens' CodeCoverageReportSummary (Lude.Maybe Lude.Natural)
ccrsLinesCovered = Lens.lens (linesCovered :: CodeCoverageReportSummary -> Lude.Maybe Lude.Natural) (\s a -> s {linesCovered = a} :: CodeCoverageReportSummary)
{-# DEPRECATED ccrsLinesCovered "Use generic-lens or generic-optics with 'linesCovered' instead." #-}

-- | The percentage of branches that are covered by your tests.
--
-- /Note:/ Consider using 'branchCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsBranchCoveragePercentage :: Lens.Lens' CodeCoverageReportSummary (Lude.Maybe Lude.Double)
ccrsBranchCoveragePercentage = Lens.lens (branchCoveragePercentage :: CodeCoverageReportSummary -> Lude.Maybe Lude.Double) (\s a -> s {branchCoveragePercentage = a} :: CodeCoverageReportSummary)
{-# DEPRECATED ccrsBranchCoveragePercentage "Use generic-lens or generic-optics with 'branchCoveragePercentage' instead." #-}

-- | The percentage of lines that are covered by your tests.
--
-- /Note:/ Consider using 'lineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsLineCoveragePercentage :: Lens.Lens' CodeCoverageReportSummary (Lude.Maybe Lude.Double)
ccrsLineCoveragePercentage = Lens.lens (lineCoveragePercentage :: CodeCoverageReportSummary -> Lude.Maybe Lude.Double) (\s a -> s {lineCoveragePercentage = a} :: CodeCoverageReportSummary)
{-# DEPRECATED ccrsLineCoveragePercentage "Use generic-lens or generic-optics with 'lineCoveragePercentage' instead." #-}

instance Lude.FromJSON CodeCoverageReportSummary where
  parseJSON =
    Lude.withObject
      "CodeCoverageReportSummary"
      ( \x ->
          CodeCoverageReportSummary'
            Lude.<$> (x Lude..:? "branchesMissed")
            Lude.<*> (x Lude..:? "linesMissed")
            Lude.<*> (x Lude..:? "branchesCovered")
            Lude.<*> (x Lude..:? "linesCovered")
            Lude.<*> (x Lude..:? "branchCoveragePercentage")
            Lude.<*> (x Lude..:? "lineCoveragePercentage")
      )
