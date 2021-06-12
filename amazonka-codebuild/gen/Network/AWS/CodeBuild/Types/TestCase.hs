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
-- Module      : Network.AWS.CodeBuild.Types.TestCase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestCase where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a test case created using a framework such as NUnit or
-- Cucumber. A test case might be a unit test or a configuration test.
--
-- /See:/ 'newTestCase' smart constructor.
data TestCase = TestCase'
  { -- | The path to the raw data file that contains the test result.
    testRawDataPath :: Core.Maybe Core.Text,
    -- | The status returned by the test case after it was run. Valid statuses
    -- are @SUCCEEDED@, @FAILED@, @ERROR@, @SKIPPED@, and @UNKNOWN@.
    status :: Core.Maybe Core.Text,
    -- | A message associated with a test case. For example, an error message or
    -- stack trace.
    message :: Core.Maybe Core.Text,
    -- | The ARN of the report to which the test case belongs.
    reportArn :: Core.Maybe Core.Text,
    -- | A string that is applied to a series of related test cases. CodeBuild
    -- generates the prefix. The prefix depends on the framework used to
    -- generate the tests.
    prefix :: Core.Maybe Core.Text,
    -- | The name of the test case.
    name :: Core.Maybe Core.Text,
    -- | The date and time a test case expires. A test case expires 30 days after
    -- it is created. An expired test case is not available to view in
    -- CodeBuild.
    expired :: Core.Maybe Core.POSIX,
    -- | The number of nanoseconds it took to run this test case.
    durationInNanoSeconds :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testRawDataPath', 'testCase_testRawDataPath' - The path to the raw data file that contains the test result.
--
-- 'status', 'testCase_status' - The status returned by the test case after it was run. Valid statuses
-- are @SUCCEEDED@, @FAILED@, @ERROR@, @SKIPPED@, and @UNKNOWN@.
--
-- 'message', 'testCase_message' - A message associated with a test case. For example, an error message or
-- stack trace.
--
-- 'reportArn', 'testCase_reportArn' - The ARN of the report to which the test case belongs.
--
-- 'prefix', 'testCase_prefix' - A string that is applied to a series of related test cases. CodeBuild
-- generates the prefix. The prefix depends on the framework used to
-- generate the tests.
--
-- 'name', 'testCase_name' - The name of the test case.
--
-- 'expired', 'testCase_expired' - The date and time a test case expires. A test case expires 30 days after
-- it is created. An expired test case is not available to view in
-- CodeBuild.
--
-- 'durationInNanoSeconds', 'testCase_durationInNanoSeconds' - The number of nanoseconds it took to run this test case.
newTestCase ::
  TestCase
newTestCase =
  TestCase'
    { testRawDataPath = Core.Nothing,
      status = Core.Nothing,
      message = Core.Nothing,
      reportArn = Core.Nothing,
      prefix = Core.Nothing,
      name = Core.Nothing,
      expired = Core.Nothing,
      durationInNanoSeconds = Core.Nothing
    }

-- | The path to the raw data file that contains the test result.
testCase_testRawDataPath :: Lens.Lens' TestCase (Core.Maybe Core.Text)
testCase_testRawDataPath = Lens.lens (\TestCase' {testRawDataPath} -> testRawDataPath) (\s@TestCase' {} a -> s {testRawDataPath = a} :: TestCase)

-- | The status returned by the test case after it was run. Valid statuses
-- are @SUCCEEDED@, @FAILED@, @ERROR@, @SKIPPED@, and @UNKNOWN@.
testCase_status :: Lens.Lens' TestCase (Core.Maybe Core.Text)
testCase_status = Lens.lens (\TestCase' {status} -> status) (\s@TestCase' {} a -> s {status = a} :: TestCase)

-- | A message associated with a test case. For example, an error message or
-- stack trace.
testCase_message :: Lens.Lens' TestCase (Core.Maybe Core.Text)
testCase_message = Lens.lens (\TestCase' {message} -> message) (\s@TestCase' {} a -> s {message = a} :: TestCase)

-- | The ARN of the report to which the test case belongs.
testCase_reportArn :: Lens.Lens' TestCase (Core.Maybe Core.Text)
testCase_reportArn = Lens.lens (\TestCase' {reportArn} -> reportArn) (\s@TestCase' {} a -> s {reportArn = a} :: TestCase)

-- | A string that is applied to a series of related test cases. CodeBuild
-- generates the prefix. The prefix depends on the framework used to
-- generate the tests.
testCase_prefix :: Lens.Lens' TestCase (Core.Maybe Core.Text)
testCase_prefix = Lens.lens (\TestCase' {prefix} -> prefix) (\s@TestCase' {} a -> s {prefix = a} :: TestCase)

-- | The name of the test case.
testCase_name :: Lens.Lens' TestCase (Core.Maybe Core.Text)
testCase_name = Lens.lens (\TestCase' {name} -> name) (\s@TestCase' {} a -> s {name = a} :: TestCase)

-- | The date and time a test case expires. A test case expires 30 days after
-- it is created. An expired test case is not available to view in
-- CodeBuild.
testCase_expired :: Lens.Lens' TestCase (Core.Maybe Core.UTCTime)
testCase_expired = Lens.lens (\TestCase' {expired} -> expired) (\s@TestCase' {} a -> s {expired = a} :: TestCase) Core.. Lens.mapping Core._Time

-- | The number of nanoseconds it took to run this test case.
testCase_durationInNanoSeconds :: Lens.Lens' TestCase (Core.Maybe Core.Integer)
testCase_durationInNanoSeconds = Lens.lens (\TestCase' {durationInNanoSeconds} -> durationInNanoSeconds) (\s@TestCase' {} a -> s {durationInNanoSeconds = a} :: TestCase)

instance Core.FromJSON TestCase where
  parseJSON =
    Core.withObject
      "TestCase"
      ( \x ->
          TestCase'
            Core.<$> (x Core..:? "testRawDataPath")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "reportArn")
            Core.<*> (x Core..:? "prefix")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "expired")
            Core.<*> (x Core..:? "durationInNanoSeconds")
      )

instance Core.Hashable TestCase

instance Core.NFData TestCase
