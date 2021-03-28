{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CodeCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.CodeCoverage
  ( CodeCoverage (..)
  -- * Smart constructor
  , mkCodeCoverage
  -- * Lenses
  , ccBranchCoveragePercentage
  , ccBranchesCovered
  , ccBranchesMissed
  , ccExpired
  , ccFilePath
  , ccId
  , ccLineCoveragePercentage
  , ccLinesCovered
  , ccLinesMissed
  , ccReportARN
  ) where

import qualified Network.AWS.CodeBuild.Types.FilePath as Types
import qualified Network.AWS.CodeBuild.Types.Id as Types
import qualified Network.AWS.CodeBuild.Types.ReportARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains code coverage report information.
--
-- Line coverage measures how many statements your tests cover. A statement is a single instruction, not including comments, conditionals, etc.
-- Branch coverage determines if your tests cover every possible branch of a control structure, such as an @if@ or @case@ statement.
--
-- /See:/ 'mkCodeCoverage' smart constructor.
data CodeCoverage = CodeCoverage'
  { branchCoveragePercentage :: Core.Maybe Core.Double
    -- ^ The percentage of branches that are covered by your tests.
  , branchesCovered :: Core.Maybe Core.Natural
    -- ^ The number of conditional branches that are covered by your tests.
  , branchesMissed :: Core.Maybe Core.Natural
    -- ^ The number of conditional branches that are not covered by your tests.
  , expired :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the tests were run.
  , filePath :: Core.Maybe Types.FilePath
    -- ^ The path of the test report file.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the code coverage report.
  , lineCoveragePercentage :: Core.Maybe Core.Double
    -- ^ The percentage of lines that are covered by your tests.
  , linesCovered :: Core.Maybe Core.Natural
    -- ^ The number of lines that are covered by your tests.
  , linesMissed :: Core.Maybe Core.Natural
    -- ^ The number of lines that are not covered by your tests.
  , reportARN :: Core.Maybe Types.ReportARN
    -- ^ The ARN of the report.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CodeCoverage' value with any optional fields omitted.
mkCodeCoverage
    :: CodeCoverage
mkCodeCoverage
  = CodeCoverage'{branchCoveragePercentage = Core.Nothing,
                  branchesCovered = Core.Nothing, branchesMissed = Core.Nothing,
                  expired = Core.Nothing, filePath = Core.Nothing, id = Core.Nothing,
                  lineCoveragePercentage = Core.Nothing, linesCovered = Core.Nothing,
                  linesMissed = Core.Nothing, reportARN = Core.Nothing}

-- | The percentage of branches that are covered by your tests.
--
-- /Note:/ Consider using 'branchCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBranchCoveragePercentage :: Lens.Lens' CodeCoverage (Core.Maybe Core.Double)
ccBranchCoveragePercentage = Lens.field @"branchCoveragePercentage"
{-# INLINEABLE ccBranchCoveragePercentage #-}
{-# DEPRECATED branchCoveragePercentage "Use generic-lens or generic-optics with 'branchCoveragePercentage' instead"  #-}

-- | The number of conditional branches that are covered by your tests.
--
-- /Note:/ Consider using 'branchesCovered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBranchesCovered :: Lens.Lens' CodeCoverage (Core.Maybe Core.Natural)
ccBranchesCovered = Lens.field @"branchesCovered"
{-# INLINEABLE ccBranchesCovered #-}
{-# DEPRECATED branchesCovered "Use generic-lens or generic-optics with 'branchesCovered' instead"  #-}

-- | The number of conditional branches that are not covered by your tests.
--
-- /Note:/ Consider using 'branchesMissed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBranchesMissed :: Lens.Lens' CodeCoverage (Core.Maybe Core.Natural)
ccBranchesMissed = Lens.field @"branchesMissed"
{-# INLINEABLE ccBranchesMissed #-}
{-# DEPRECATED branchesMissed "Use generic-lens or generic-optics with 'branchesMissed' instead"  #-}

-- | The date and time that the tests were run.
--
-- /Note:/ Consider using 'expired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpired :: Lens.Lens' CodeCoverage (Core.Maybe Core.NominalDiffTime)
ccExpired = Lens.field @"expired"
{-# INLINEABLE ccExpired #-}
{-# DEPRECATED expired "Use generic-lens or generic-optics with 'expired' instead"  #-}

-- | The path of the test report file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccFilePath :: Lens.Lens' CodeCoverage (Core.Maybe Types.FilePath)
ccFilePath = Lens.field @"filePath"
{-# INLINEABLE ccFilePath #-}
{-# DEPRECATED filePath "Use generic-lens or generic-optics with 'filePath' instead"  #-}

-- | The identifier of the code coverage report.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccId :: Lens.Lens' CodeCoverage (Core.Maybe Types.Id)
ccId = Lens.field @"id"
{-# INLINEABLE ccId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The percentage of lines that are covered by your tests.
--
-- /Note:/ Consider using 'lineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLineCoveragePercentage :: Lens.Lens' CodeCoverage (Core.Maybe Core.Double)
ccLineCoveragePercentage = Lens.field @"lineCoveragePercentage"
{-# INLINEABLE ccLineCoveragePercentage #-}
{-# DEPRECATED lineCoveragePercentage "Use generic-lens or generic-optics with 'lineCoveragePercentage' instead"  #-}

-- | The number of lines that are covered by your tests.
--
-- /Note:/ Consider using 'linesCovered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLinesCovered :: Lens.Lens' CodeCoverage (Core.Maybe Core.Natural)
ccLinesCovered = Lens.field @"linesCovered"
{-# INLINEABLE ccLinesCovered #-}
{-# DEPRECATED linesCovered "Use generic-lens or generic-optics with 'linesCovered' instead"  #-}

-- | The number of lines that are not covered by your tests.
--
-- /Note:/ Consider using 'linesMissed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLinesMissed :: Lens.Lens' CodeCoverage (Core.Maybe Core.Natural)
ccLinesMissed = Lens.field @"linesMissed"
{-# INLINEABLE ccLinesMissed #-}
{-# DEPRECATED linesMissed "Use generic-lens or generic-optics with 'linesMissed' instead"  #-}

-- | The ARN of the report.
--
-- /Note:/ Consider using 'reportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReportARN :: Lens.Lens' CodeCoverage (Core.Maybe Types.ReportARN)
ccReportARN = Lens.field @"reportARN"
{-# INLINEABLE ccReportARN #-}
{-# DEPRECATED reportARN "Use generic-lens or generic-optics with 'reportARN' instead"  #-}

instance Core.FromJSON CodeCoverage where
        parseJSON
          = Core.withObject "CodeCoverage" Core.$
              \ x ->
                CodeCoverage' Core.<$>
                  (x Core..:? "branchCoveragePercentage") Core.<*>
                    x Core..:? "branchesCovered"
                    Core.<*> x Core..:? "branchesMissed"
                    Core.<*> x Core..:? "expired"
                    Core.<*> x Core..:? "filePath"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "lineCoveragePercentage"
                    Core.<*> x Core..:? "linesCovered"
                    Core.<*> x Core..:? "linesMissed"
                    Core.<*> x Core..:? "reportARN"
