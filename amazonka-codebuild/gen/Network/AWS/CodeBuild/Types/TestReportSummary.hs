{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a test report.
--
-- /See:/ 'newTestReportSummary' smart constructor.
data TestReportSummary = TestReportSummary'
  { -- | The number of test cases in this @TestReportSummary@. The total includes
    -- truncated test cases.
    total :: Prelude.Int,
    -- | A map that contains the number of each type of status returned by the
    -- test results in this @TestReportSummary@.
    statusCounts :: Prelude.HashMap Prelude.Text Prelude.Int,
    -- | The number of nanoseconds it took to run all of the test cases in this
    -- report.
    durationInNanoSeconds :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'durationInNanoSeconds'
  Prelude.Integer ->
  TestReportSummary
newTestReportSummary pTotal_ pDurationInNanoSeconds_ =
  TestReportSummary'
    { total = pTotal_,
      statusCounts = Prelude.mempty,
      durationInNanoSeconds = pDurationInNanoSeconds_
    }

-- | The number of test cases in this @TestReportSummary@. The total includes
-- truncated test cases.
testReportSummary_total :: Lens.Lens' TestReportSummary Prelude.Int
testReportSummary_total = Lens.lens (\TestReportSummary' {total} -> total) (\s@TestReportSummary' {} a -> s {total = a} :: TestReportSummary)

-- | A map that contains the number of each type of status returned by the
-- test results in this @TestReportSummary@.
testReportSummary_statusCounts :: Lens.Lens' TestReportSummary (Prelude.HashMap Prelude.Text Prelude.Int)
testReportSummary_statusCounts = Lens.lens (\TestReportSummary' {statusCounts} -> statusCounts) (\s@TestReportSummary' {} a -> s {statusCounts = a} :: TestReportSummary) Prelude.. Prelude._Coerce

-- | The number of nanoseconds it took to run all of the test cases in this
-- report.
testReportSummary_durationInNanoSeconds :: Lens.Lens' TestReportSummary Prelude.Integer
testReportSummary_durationInNanoSeconds = Lens.lens (\TestReportSummary' {durationInNanoSeconds} -> durationInNanoSeconds) (\s@TestReportSummary' {} a -> s {durationInNanoSeconds = a} :: TestReportSummary)

instance Prelude.FromJSON TestReportSummary where
  parseJSON =
    Prelude.withObject
      "TestReportSummary"
      ( \x ->
          TestReportSummary'
            Prelude.<$> (x Prelude..: "total")
            Prelude.<*> ( x Prelude..:? "statusCounts"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "durationInNanoSeconds")
      )

instance Prelude.Hashable TestReportSummary

instance Prelude.NFData TestReportSummary
