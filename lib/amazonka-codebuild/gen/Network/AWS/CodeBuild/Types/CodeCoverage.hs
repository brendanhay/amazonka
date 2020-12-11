-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CodeCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CodeCoverage
  ( CodeCoverage (..),

    -- * Smart constructor
    mkCodeCoverage,

    -- * Lenses
    ccExpired,
    ccBranchesMissed,
    ccLinesMissed,
    ccFilePath,
    ccBranchesCovered,
    ccLinesCovered,
    ccBranchCoveragePercentage,
    ccId,
    ccLineCoveragePercentage,
    ccReportARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains code coverage report information.
--
-- Line coverage measures how many statements your tests cover. A statement is a single instruction, not including comments, conditionals, etc.
-- Branch coverage determines if your tests cover every possible branch of a control structure, such as an @if@ or @case@ statement.
--
-- /See:/ 'mkCodeCoverage' smart constructor.
data CodeCoverage = CodeCoverage'
  { expired ::
      Lude.Maybe Lude.Timestamp,
    branchesMissed :: Lude.Maybe Lude.Natural,
    linesMissed :: Lude.Maybe Lude.Natural,
    filePath :: Lude.Maybe Lude.Text,
    branchesCovered :: Lude.Maybe Lude.Natural,
    linesCovered :: Lude.Maybe Lude.Natural,
    branchCoveragePercentage :: Lude.Maybe Lude.Double,
    id :: Lude.Maybe Lude.Text,
    lineCoveragePercentage :: Lude.Maybe Lude.Double,
    reportARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeCoverage' with the minimum fields required to make a request.
--
-- * 'branchCoveragePercentage' - The percentage of branches that are covered by your tests.
-- * 'branchesCovered' - The number of conditional branches that are covered by your tests.
-- * 'branchesMissed' - The number of conditional branches that are not covered by your tests.
-- * 'expired' - The date and time that the tests were run.
-- * 'filePath' - The path of the test report file.
-- * 'id' - The identifier of the code coverage report.
-- * 'lineCoveragePercentage' - The percentage of lines that are covered by your tests.
-- * 'linesCovered' - The number of lines that are covered by your tests.
-- * 'linesMissed' - The number of lines that are not covered by your tests.
-- * 'reportARN' - The ARN of the report.
mkCodeCoverage ::
  CodeCoverage
mkCodeCoverage =
  CodeCoverage'
    { expired = Lude.Nothing,
      branchesMissed = Lude.Nothing,
      linesMissed = Lude.Nothing,
      filePath = Lude.Nothing,
      branchesCovered = Lude.Nothing,
      linesCovered = Lude.Nothing,
      branchCoveragePercentage = Lude.Nothing,
      id = Lude.Nothing,
      lineCoveragePercentage = Lude.Nothing,
      reportARN = Lude.Nothing
    }

-- | The date and time that the tests were run.
--
-- /Note:/ Consider using 'expired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpired :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Timestamp)
ccExpired = Lens.lens (expired :: CodeCoverage -> Lude.Maybe Lude.Timestamp) (\s a -> s {expired = a} :: CodeCoverage)
{-# DEPRECATED ccExpired "Use generic-lens or generic-optics with 'expired' instead." #-}

-- | The number of conditional branches that are not covered by your tests.
--
-- /Note:/ Consider using 'branchesMissed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBranchesMissed :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Natural)
ccBranchesMissed = Lens.lens (branchesMissed :: CodeCoverage -> Lude.Maybe Lude.Natural) (\s a -> s {branchesMissed = a} :: CodeCoverage)
{-# DEPRECATED ccBranchesMissed "Use generic-lens or generic-optics with 'branchesMissed' instead." #-}

-- | The number of lines that are not covered by your tests.
--
-- /Note:/ Consider using 'linesMissed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLinesMissed :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Natural)
ccLinesMissed = Lens.lens (linesMissed :: CodeCoverage -> Lude.Maybe Lude.Natural) (\s a -> s {linesMissed = a} :: CodeCoverage)
{-# DEPRECATED ccLinesMissed "Use generic-lens or generic-optics with 'linesMissed' instead." #-}

-- | The path of the test report file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccFilePath :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Text)
ccFilePath = Lens.lens (filePath :: CodeCoverage -> Lude.Maybe Lude.Text) (\s a -> s {filePath = a} :: CodeCoverage)
{-# DEPRECATED ccFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The number of conditional branches that are covered by your tests.
--
-- /Note:/ Consider using 'branchesCovered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBranchesCovered :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Natural)
ccBranchesCovered = Lens.lens (branchesCovered :: CodeCoverage -> Lude.Maybe Lude.Natural) (\s a -> s {branchesCovered = a} :: CodeCoverage)
{-# DEPRECATED ccBranchesCovered "Use generic-lens or generic-optics with 'branchesCovered' instead." #-}

-- | The number of lines that are covered by your tests.
--
-- /Note:/ Consider using 'linesCovered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLinesCovered :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Natural)
ccLinesCovered = Lens.lens (linesCovered :: CodeCoverage -> Lude.Maybe Lude.Natural) (\s a -> s {linesCovered = a} :: CodeCoverage)
{-# DEPRECATED ccLinesCovered "Use generic-lens or generic-optics with 'linesCovered' instead." #-}

-- | The percentage of branches that are covered by your tests.
--
-- /Note:/ Consider using 'branchCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBranchCoveragePercentage :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Double)
ccBranchCoveragePercentage = Lens.lens (branchCoveragePercentage :: CodeCoverage -> Lude.Maybe Lude.Double) (\s a -> s {branchCoveragePercentage = a} :: CodeCoverage)
{-# DEPRECATED ccBranchCoveragePercentage "Use generic-lens or generic-optics with 'branchCoveragePercentage' instead." #-}

-- | The identifier of the code coverage report.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccId :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Text)
ccId = Lens.lens (id :: CodeCoverage -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CodeCoverage)
{-# DEPRECATED ccId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The percentage of lines that are covered by your tests.
--
-- /Note:/ Consider using 'lineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLineCoveragePercentage :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Double)
ccLineCoveragePercentage = Lens.lens (lineCoveragePercentage :: CodeCoverage -> Lude.Maybe Lude.Double) (\s a -> s {lineCoveragePercentage = a} :: CodeCoverage)
{-# DEPRECATED ccLineCoveragePercentage "Use generic-lens or generic-optics with 'lineCoveragePercentage' instead." #-}

-- | The ARN of the report.
--
-- /Note:/ Consider using 'reportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReportARN :: Lens.Lens' CodeCoverage (Lude.Maybe Lude.Text)
ccReportARN = Lens.lens (reportARN :: CodeCoverage -> Lude.Maybe Lude.Text) (\s a -> s {reportARN = a} :: CodeCoverage)
{-# DEPRECATED ccReportARN "Use generic-lens or generic-optics with 'reportARN' instead." #-}

instance Lude.FromJSON CodeCoverage where
  parseJSON =
    Lude.withObject
      "CodeCoverage"
      ( \x ->
          CodeCoverage'
            Lude.<$> (x Lude..:? "expired")
            Lude.<*> (x Lude..:? "branchesMissed")
            Lude.<*> (x Lude..:? "linesMissed")
            Lude.<*> (x Lude..:? "filePath")
            Lude.<*> (x Lude..:? "branchesCovered")
            Lude.<*> (x Lude..:? "linesCovered")
            Lude.<*> (x Lude..:? "branchCoveragePercentage")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "lineCoveragePercentage")
            Lude.<*> (x Lude..:? "reportARN")
      )
