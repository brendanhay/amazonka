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
-- Module      : Amazonka.CodeBuild.Types.TestCase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.TestCase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a test case created using a framework such as NUnit or
-- Cucumber. A test case might be a unit test or a configuration test.
--
-- /See:/ 'newTestCase' smart constructor.
data TestCase = TestCase'
  { -- | The number of nanoseconds it took to run this test case.
    durationInNanoSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The date and time a test case expires. A test case expires 30 days after
    -- it is created. An expired test case is not available to view in
    -- CodeBuild.
    expired :: Prelude.Maybe Data.POSIX,
    -- | A message associated with a test case. For example, an error message or
    -- stack trace.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the test case.
    name :: Prelude.Maybe Prelude.Text,
    -- | A string that is applied to a series of related test cases. CodeBuild
    -- generates the prefix. The prefix depends on the framework used to
    -- generate the tests.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the report to which the test case belongs.
    reportArn :: Prelude.Maybe Prelude.Text,
    -- | The status returned by the test case after it was run. Valid statuses
    -- are @SUCCEEDED@, @FAILED@, @ERROR@, @SKIPPED@, and @UNKNOWN@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The path to the raw data file that contains the test result.
    testRawDataPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInNanoSeconds', 'testCase_durationInNanoSeconds' - The number of nanoseconds it took to run this test case.
--
-- 'expired', 'testCase_expired' - The date and time a test case expires. A test case expires 30 days after
-- it is created. An expired test case is not available to view in
-- CodeBuild.
--
-- 'message', 'testCase_message' - A message associated with a test case. For example, an error message or
-- stack trace.
--
-- 'name', 'testCase_name' - The name of the test case.
--
-- 'prefix', 'testCase_prefix' - A string that is applied to a series of related test cases. CodeBuild
-- generates the prefix. The prefix depends on the framework used to
-- generate the tests.
--
-- 'reportArn', 'testCase_reportArn' - The ARN of the report to which the test case belongs.
--
-- 'status', 'testCase_status' - The status returned by the test case after it was run. Valid statuses
-- are @SUCCEEDED@, @FAILED@, @ERROR@, @SKIPPED@, and @UNKNOWN@.
--
-- 'testRawDataPath', 'testCase_testRawDataPath' - The path to the raw data file that contains the test result.
newTestCase ::
  TestCase
newTestCase =
  TestCase'
    { durationInNanoSeconds = Prelude.Nothing,
      expired = Prelude.Nothing,
      message = Prelude.Nothing,
      name = Prelude.Nothing,
      prefix = Prelude.Nothing,
      reportArn = Prelude.Nothing,
      status = Prelude.Nothing,
      testRawDataPath = Prelude.Nothing
    }

-- | The number of nanoseconds it took to run this test case.
testCase_durationInNanoSeconds :: Lens.Lens' TestCase (Prelude.Maybe Prelude.Integer)
testCase_durationInNanoSeconds = Lens.lens (\TestCase' {durationInNanoSeconds} -> durationInNanoSeconds) (\s@TestCase' {} a -> s {durationInNanoSeconds = a} :: TestCase)

-- | The date and time a test case expires. A test case expires 30 days after
-- it is created. An expired test case is not available to view in
-- CodeBuild.
testCase_expired :: Lens.Lens' TestCase (Prelude.Maybe Prelude.UTCTime)
testCase_expired = Lens.lens (\TestCase' {expired} -> expired) (\s@TestCase' {} a -> s {expired = a} :: TestCase) Prelude.. Lens.mapping Data._Time

-- | A message associated with a test case. For example, an error message or
-- stack trace.
testCase_message :: Lens.Lens' TestCase (Prelude.Maybe Prelude.Text)
testCase_message = Lens.lens (\TestCase' {message} -> message) (\s@TestCase' {} a -> s {message = a} :: TestCase)

-- | The name of the test case.
testCase_name :: Lens.Lens' TestCase (Prelude.Maybe Prelude.Text)
testCase_name = Lens.lens (\TestCase' {name} -> name) (\s@TestCase' {} a -> s {name = a} :: TestCase)

-- | A string that is applied to a series of related test cases. CodeBuild
-- generates the prefix. The prefix depends on the framework used to
-- generate the tests.
testCase_prefix :: Lens.Lens' TestCase (Prelude.Maybe Prelude.Text)
testCase_prefix = Lens.lens (\TestCase' {prefix} -> prefix) (\s@TestCase' {} a -> s {prefix = a} :: TestCase)

-- | The ARN of the report to which the test case belongs.
testCase_reportArn :: Lens.Lens' TestCase (Prelude.Maybe Prelude.Text)
testCase_reportArn = Lens.lens (\TestCase' {reportArn} -> reportArn) (\s@TestCase' {} a -> s {reportArn = a} :: TestCase)

-- | The status returned by the test case after it was run. Valid statuses
-- are @SUCCEEDED@, @FAILED@, @ERROR@, @SKIPPED@, and @UNKNOWN@.
testCase_status :: Lens.Lens' TestCase (Prelude.Maybe Prelude.Text)
testCase_status = Lens.lens (\TestCase' {status} -> status) (\s@TestCase' {} a -> s {status = a} :: TestCase)

-- | The path to the raw data file that contains the test result.
testCase_testRawDataPath :: Lens.Lens' TestCase (Prelude.Maybe Prelude.Text)
testCase_testRawDataPath = Lens.lens (\TestCase' {testRawDataPath} -> testRawDataPath) (\s@TestCase' {} a -> s {testRawDataPath = a} :: TestCase)

instance Data.FromJSON TestCase where
  parseJSON =
    Data.withObject
      "TestCase"
      ( \x ->
          TestCase'
            Prelude.<$> (x Data..:? "durationInNanoSeconds")
            Prelude.<*> (x Data..:? "expired")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "prefix")
            Prelude.<*> (x Data..:? "reportArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "testRawDataPath")
      )

instance Prelude.Hashable TestCase where
  hashWithSalt _salt TestCase' {..} =
    _salt
      `Prelude.hashWithSalt` durationInNanoSeconds
      `Prelude.hashWithSalt` expired
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` reportArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` testRawDataPath

instance Prelude.NFData TestCase where
  rnf TestCase' {..} =
    Prelude.rnf durationInNanoSeconds `Prelude.seq`
      Prelude.rnf expired `Prelude.seq`
        Prelude.rnf message `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf prefix `Prelude.seq`
              Prelude.rnf reportArn `Prelude.seq`
                Prelude.rnf status `Prelude.seq`
                  Prelude.rnf testRawDataPath
