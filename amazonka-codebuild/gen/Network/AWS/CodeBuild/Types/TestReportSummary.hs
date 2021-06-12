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
-- Module      : Network.AWS.CodeBuild.Types.TestReportSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestReportSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a test report.
--
-- /See:/ 'newTestReportSummary' smart constructor.
data TestReportSummary = TestReportSummary'
  { -- | The number of test cases in this @TestReportSummary@. The total includes
    -- truncated test cases.
    total :: Core.Int,
    -- | A map that contains the number of each type of status returned by the
    -- test results in this @TestReportSummary@.
    statusCounts :: Core.HashMap Core.Text Core.Int,
    -- | The number of nanoseconds it took to run all of the test cases in this
    -- report.
    durationInNanoSeconds :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestReportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'testReportSummary_total' - The number of test cases in this @TestReportSummary@. The total includes
-- truncated test cases.
--
-- 'statusCounts', 'testReportSummary_statusCounts' - A map that contains the number of each type of status returned by the
-- test results in this @TestReportSummary@.
--
-- 'durationInNanoSeconds', 'testReportSummary_durationInNanoSeconds' - The number of nanoseconds it took to run all of the test cases in this
-- report.
newTestReportSummary ::
  -- | 'total'
  Core.Int ->
  -- | 'durationInNanoSeconds'
  Core.Integer ->
  TestReportSummary
newTestReportSummary pTotal_ pDurationInNanoSeconds_ =
  TestReportSummary'
    { total = pTotal_,
      statusCounts = Core.mempty,
      durationInNanoSeconds = pDurationInNanoSeconds_
    }

-- | The number of test cases in this @TestReportSummary@. The total includes
-- truncated test cases.
testReportSummary_total :: Lens.Lens' TestReportSummary Core.Int
testReportSummary_total = Lens.lens (\TestReportSummary' {total} -> total) (\s@TestReportSummary' {} a -> s {total = a} :: TestReportSummary)

-- | A map that contains the number of each type of status returned by the
-- test results in this @TestReportSummary@.
testReportSummary_statusCounts :: Lens.Lens' TestReportSummary (Core.HashMap Core.Text Core.Int)
testReportSummary_statusCounts = Lens.lens (\TestReportSummary' {statusCounts} -> statusCounts) (\s@TestReportSummary' {} a -> s {statusCounts = a} :: TestReportSummary) Core.. Lens._Coerce

-- | The number of nanoseconds it took to run all of the test cases in this
-- report.
testReportSummary_durationInNanoSeconds :: Lens.Lens' TestReportSummary Core.Integer
testReportSummary_durationInNanoSeconds = Lens.lens (\TestReportSummary' {durationInNanoSeconds} -> durationInNanoSeconds) (\s@TestReportSummary' {} a -> s {durationInNanoSeconds = a} :: TestReportSummary)

instance Core.FromJSON TestReportSummary where
  parseJSON =
    Core.withObject
      "TestReportSummary"
      ( \x ->
          TestReportSummary'
            Core.<$> (x Core..: "total")
            Core.<*> (x Core..:? "statusCounts" Core..!= Core.mempty)
            Core.<*> (x Core..: "durationInNanoSeconds")
      )

instance Core.Hashable TestReportSummary

instance Core.NFData TestReportSummary
