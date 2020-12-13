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
    trsDurationInNanoSeconds,
    trsStatusCounts,
    trsTotal,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a test report.
--
-- /See:/ 'mkTestReportSummary' smart constructor.
data TestReportSummary = TestReportSummary'
  { -- | The number of nanoseconds it took to run all of the test cases in this report.
    durationInNanoSeconds :: Lude.Integer,
    -- | A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
    statusCounts :: Lude.HashMap Lude.Text (Lude.Int),
    -- | The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
    total :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestReportSummary' with the minimum fields required to make a request.
--
-- * 'durationInNanoSeconds' - The number of nanoseconds it took to run all of the test cases in this report.
-- * 'statusCounts' - A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
-- * 'total' - The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
mkTestReportSummary ::
  -- | 'durationInNanoSeconds'
  Lude.Integer ->
  -- | 'total'
  Lude.Int ->
  TestReportSummary
mkTestReportSummary pDurationInNanoSeconds_ pTotal_ =
  TestReportSummary'
    { durationInNanoSeconds =
        pDurationInNanoSeconds_,
      statusCounts = Lude.mempty,
      total = pTotal_
    }

-- | The number of nanoseconds it took to run all of the test cases in this report.
--
-- /Note:/ Consider using 'durationInNanoSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsDurationInNanoSeconds :: Lens.Lens' TestReportSummary Lude.Integer
trsDurationInNanoSeconds = Lens.lens (durationInNanoSeconds :: TestReportSummary -> Lude.Integer) (\s a -> s {durationInNanoSeconds = a} :: TestReportSummary)
{-# DEPRECATED trsDurationInNanoSeconds "Use generic-lens or generic-optics with 'durationInNanoSeconds' instead." #-}

-- | A map that contains the number of each type of status returned by the test results in this @TestReportSummary@ .
--
-- /Note:/ Consider using 'statusCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsStatusCounts :: Lens.Lens' TestReportSummary (Lude.HashMap Lude.Text (Lude.Int))
trsStatusCounts = Lens.lens (statusCounts :: TestReportSummary -> Lude.HashMap Lude.Text (Lude.Int)) (\s a -> s {statusCounts = a} :: TestReportSummary)
{-# DEPRECATED trsStatusCounts "Use generic-lens or generic-optics with 'statusCounts' instead." #-}

-- | The number of test cases in this @TestReportSummary@ . The total includes truncated test cases.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trsTotal :: Lens.Lens' TestReportSummary Lude.Int
trsTotal = Lens.lens (total :: TestReportSummary -> Lude.Int) (\s a -> s {total = a} :: TestReportSummary)
{-# DEPRECATED trsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Lude.FromJSON TestReportSummary where
  parseJSON =
    Lude.withObject
      "TestReportSummary"
      ( \x ->
          TestReportSummary'
            Lude.<$> (x Lude..: "durationInNanoSeconds")
            Lude.<*> (x Lude..:? "statusCounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "total")
      )
