{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.TestReportSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestReportSummary
  ( TestReportSummary (..),

    -- * Smart constructor
    mkTestReportSummary,

    -- * Lenses
    trsTotal,
    trsStatusCounts,
    trsDurationInNanoSeconds,
  )
where

import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a test report.
--
-- /See:/ 'mkTestReportSummary' smart constructor.
data TestReportSummary = TestReportSummary'
  { -- | The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
    total :: Core.Int,
    -- | A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
    statusCounts :: Core.HashMap Types.String Core.Int,
    -- | The number of nanoseconds it took to run all of the test cases in this report.
    durationInNanoSeconds :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestReportSummary' value with any optional fields omitted.
mkTestReportSummary ::
  -- | 'total'
  Core.Int ->
  -- | 'durationInNanoSeconds'
  Core.Integer ->
  TestReportSummary
mkTestReportSummary total durationInNanoSeconds =
  TestReportSummary'
    { total,
      statusCounts = Core.mempty,
      durationInNanoSeconds
    }

-- | The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTotal :: Lens.Lens' TestReportSummary Core.Int
trsTotal = Lens.field @"total"
{-# DEPRECATED trsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
--
-- /Note:/ Consider using 'statusCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsStatusCounts :: Lens.Lens' TestReportSummary (Core.HashMap Types.String Core.Int)
trsStatusCounts = Lens.field @"statusCounts"
{-# DEPRECATED trsStatusCounts "Use generic-lens or generic-optics with 'statusCounts' instead." #-}

-- | The number of nanoseconds it took to run all of the test cases in this report.
--
-- /Note:/ Consider using 'durationInNanoSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsDurationInNanoSeconds :: Lens.Lens' TestReportSummary Core.Integer
trsDurationInNanoSeconds = Lens.field @"durationInNanoSeconds"
{-# DEPRECATED trsDurationInNanoSeconds "Use generic-lens or generic-optics with 'durationInNanoSeconds' instead." #-}

instance Core.FromJSON TestReportSummary where
  parseJSON =
    Core.withObject "TestReportSummary" Core.$
      \x ->
        TestReportSummary'
          Core.<$> (x Core..: "total")
          Core.<*> (x Core..:? "statusCounts" Core..!= Core.mempty)
          Core.<*> (x Core..: "durationInNanoSeconds")
