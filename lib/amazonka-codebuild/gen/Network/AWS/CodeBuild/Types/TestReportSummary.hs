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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a test report.
--
-- /See:/ 'mkTestReportSummary' smart constructor.
data TestReportSummary = TestReportSummary'
  { total :: Lude.Int,
    statusCounts :: Lude.HashMap Lude.Text (Lude.Int),
    durationInNanoSeconds :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestReportSummary' with the minimum fields required to make a request.
--
-- * 'durationInNanoSeconds' - The number of nanoseconds it took to run all of the test cases in this report.
-- * 'statusCounts' - A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
-- * 'total' - The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
mkTestReportSummary ::
  -- | 'total'
  Lude.Int ->
  -- | 'durationInNanoSeconds'
  Lude.Integer ->
  TestReportSummary
mkTestReportSummary pTotal_ pDurationInNanoSeconds_ =
  TestReportSummary'
    { total = pTotal_,
      statusCounts = Lude.mempty,
      durationInNanoSeconds = pDurationInNanoSeconds_
    }

-- | The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTotal :: Lens.Lens' TestReportSummary Lude.Int
trsTotal = Lens.lens (total :: TestReportSummary -> Lude.Int) (\s a -> s {total = a} :: TestReportSummary)
{-# DEPRECATED trsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
--
-- /Note:/ Consider using 'statusCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsStatusCounts :: Lens.Lens' TestReportSummary (Lude.HashMap Lude.Text (Lude.Int))
trsStatusCounts = Lens.lens (statusCounts :: TestReportSummary -> Lude.HashMap Lude.Text (Lude.Int)) (\s a -> s {statusCounts = a} :: TestReportSummary)
{-# DEPRECATED trsStatusCounts "Use generic-lens or generic-optics with 'statusCounts' instead." #-}

-- | The number of nanoseconds it took to run all of the test cases in this report.
--
-- /Note:/ Consider using 'durationInNanoSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsDurationInNanoSeconds :: Lens.Lens' TestReportSummary Lude.Integer
trsDurationInNanoSeconds = Lens.lens (durationInNanoSeconds :: TestReportSummary -> Lude.Integer) (\s a -> s {durationInNanoSeconds = a} :: TestReportSummary)
{-# DEPRECATED trsDurationInNanoSeconds "Use generic-lens or generic-optics with 'durationInNanoSeconds' instead." #-}

instance Lude.FromJSON TestReportSummary where
  parseJSON =
    Lude.withObject
      "TestReportSummary"
      ( \x ->
          TestReportSummary'
            Lude.<$> (x Lude..: "total")
            Lude.<*> (x Lude..:? "statusCounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "durationInNanoSeconds")
      )
