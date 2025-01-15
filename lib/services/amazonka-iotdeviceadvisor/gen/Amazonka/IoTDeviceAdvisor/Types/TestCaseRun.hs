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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.TestCaseRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.TestCaseRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types.Status
import Amazonka.IoTDeviceAdvisor.Types.TestCaseScenario
import qualified Amazonka.Prelude as Prelude

-- | Provides the test case run.
--
-- /See:/ 'newTestCaseRun' smart constructor.
data TestCaseRun = TestCaseRun'
  { -- | Provides test case run end time.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Provides test case run failure result.
    failure :: Prelude.Maybe Prelude.Text,
    -- | Provides test case run log URL.
    logUrl :: Prelude.Maybe Prelude.Text,
    -- | Provides test case run start time.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Provides the test case run status. Status is one of the following:
    --
    -- -   @PASS@: Test passed.
    --
    -- -   @FAIL@: Test failed.
    --
    -- -   @PENDING@: Test has not started running but is scheduled.
    --
    -- -   @RUNNING@: Test is running.
    --
    -- -   @STOPPING@: Test is performing cleanup steps. You will see this
    --     status only if you stop a suite run.
    --
    -- -   @STOPPED@ Test is stopped. You will see this status only if you stop
    --     a suite run.
    --
    -- -   @PASS_WITH_WARNINGS@: Test passed with warnings.
    --
    -- -   @ERORR@: Test faced an error when running due to an internal issue.
    status :: Prelude.Maybe Status,
    -- | Provides the test case run definition ID.
    testCaseDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | Provides the test case run definition name.
    testCaseDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Provides the test case run ID.
    testCaseRunId :: Prelude.Maybe Prelude.Text,
    -- | Provides the test scenarios for the test case run.
    testScenarios :: Prelude.Maybe [TestCaseScenario],
    -- | Provides test case run warnings.
    warnings :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestCaseRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'testCaseRun_endTime' - Provides test case run end time.
--
-- 'failure', 'testCaseRun_failure' - Provides test case run failure result.
--
-- 'logUrl', 'testCaseRun_logUrl' - Provides test case run log URL.
--
-- 'startTime', 'testCaseRun_startTime' - Provides test case run start time.
--
-- 'status', 'testCaseRun_status' - Provides the test case run status. Status is one of the following:
--
-- -   @PASS@: Test passed.
--
-- -   @FAIL@: Test failed.
--
-- -   @PENDING@: Test has not started running but is scheduled.
--
-- -   @RUNNING@: Test is running.
--
-- -   @STOPPING@: Test is performing cleanup steps. You will see this
--     status only if you stop a suite run.
--
-- -   @STOPPED@ Test is stopped. You will see this status only if you stop
--     a suite run.
--
-- -   @PASS_WITH_WARNINGS@: Test passed with warnings.
--
-- -   @ERORR@: Test faced an error when running due to an internal issue.
--
-- 'testCaseDefinitionId', 'testCaseRun_testCaseDefinitionId' - Provides the test case run definition ID.
--
-- 'testCaseDefinitionName', 'testCaseRun_testCaseDefinitionName' - Provides the test case run definition name.
--
-- 'testCaseRunId', 'testCaseRun_testCaseRunId' - Provides the test case run ID.
--
-- 'testScenarios', 'testCaseRun_testScenarios' - Provides the test scenarios for the test case run.
--
-- 'warnings', 'testCaseRun_warnings' - Provides test case run warnings.
newTestCaseRun ::
  TestCaseRun
newTestCaseRun =
  TestCaseRun'
    { endTime = Prelude.Nothing,
      failure = Prelude.Nothing,
      logUrl = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      testCaseDefinitionId = Prelude.Nothing,
      testCaseDefinitionName = Prelude.Nothing,
      testCaseRunId = Prelude.Nothing,
      testScenarios = Prelude.Nothing,
      warnings = Prelude.Nothing
    }

-- | Provides test case run end time.
testCaseRun_endTime :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.UTCTime)
testCaseRun_endTime = Lens.lens (\TestCaseRun' {endTime} -> endTime) (\s@TestCaseRun' {} a -> s {endTime = a} :: TestCaseRun) Prelude.. Lens.mapping Data._Time

-- | Provides test case run failure result.
testCaseRun_failure :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_failure = Lens.lens (\TestCaseRun' {failure} -> failure) (\s@TestCaseRun' {} a -> s {failure = a} :: TestCaseRun)

-- | Provides test case run log URL.
testCaseRun_logUrl :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_logUrl = Lens.lens (\TestCaseRun' {logUrl} -> logUrl) (\s@TestCaseRun' {} a -> s {logUrl = a} :: TestCaseRun)

-- | Provides test case run start time.
testCaseRun_startTime :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.UTCTime)
testCaseRun_startTime = Lens.lens (\TestCaseRun' {startTime} -> startTime) (\s@TestCaseRun' {} a -> s {startTime = a} :: TestCaseRun) Prelude.. Lens.mapping Data._Time

-- | Provides the test case run status. Status is one of the following:
--
-- -   @PASS@: Test passed.
--
-- -   @FAIL@: Test failed.
--
-- -   @PENDING@: Test has not started running but is scheduled.
--
-- -   @RUNNING@: Test is running.
--
-- -   @STOPPING@: Test is performing cleanup steps. You will see this
--     status only if you stop a suite run.
--
-- -   @STOPPED@ Test is stopped. You will see this status only if you stop
--     a suite run.
--
-- -   @PASS_WITH_WARNINGS@: Test passed with warnings.
--
-- -   @ERORR@: Test faced an error when running due to an internal issue.
testCaseRun_status :: Lens.Lens' TestCaseRun (Prelude.Maybe Status)
testCaseRun_status = Lens.lens (\TestCaseRun' {status} -> status) (\s@TestCaseRun' {} a -> s {status = a} :: TestCaseRun)

-- | Provides the test case run definition ID.
testCaseRun_testCaseDefinitionId :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_testCaseDefinitionId = Lens.lens (\TestCaseRun' {testCaseDefinitionId} -> testCaseDefinitionId) (\s@TestCaseRun' {} a -> s {testCaseDefinitionId = a} :: TestCaseRun)

-- | Provides the test case run definition name.
testCaseRun_testCaseDefinitionName :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_testCaseDefinitionName = Lens.lens (\TestCaseRun' {testCaseDefinitionName} -> testCaseDefinitionName) (\s@TestCaseRun' {} a -> s {testCaseDefinitionName = a} :: TestCaseRun)

-- | Provides the test case run ID.
testCaseRun_testCaseRunId :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_testCaseRunId = Lens.lens (\TestCaseRun' {testCaseRunId} -> testCaseRunId) (\s@TestCaseRun' {} a -> s {testCaseRunId = a} :: TestCaseRun)

-- | Provides the test scenarios for the test case run.
testCaseRun_testScenarios :: Lens.Lens' TestCaseRun (Prelude.Maybe [TestCaseScenario])
testCaseRun_testScenarios = Lens.lens (\TestCaseRun' {testScenarios} -> testScenarios) (\s@TestCaseRun' {} a -> s {testScenarios = a} :: TestCaseRun) Prelude.. Lens.mapping Lens.coerced

-- | Provides test case run warnings.
testCaseRun_warnings :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_warnings = Lens.lens (\TestCaseRun' {warnings} -> warnings) (\s@TestCaseRun' {} a -> s {warnings = a} :: TestCaseRun)

instance Data.FromJSON TestCaseRun where
  parseJSON =
    Data.withObject
      "TestCaseRun"
      ( \x ->
          TestCaseRun'
            Prelude.<$> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "failure")
            Prelude.<*> (x Data..:? "logUrl")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "testCaseDefinitionId")
            Prelude.<*> (x Data..:? "testCaseDefinitionName")
            Prelude.<*> (x Data..:? "testCaseRunId")
            Prelude.<*> (x Data..:? "testScenarios" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "warnings")
      )

instance Prelude.Hashable TestCaseRun where
  hashWithSalt _salt TestCaseRun' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` failure
      `Prelude.hashWithSalt` logUrl
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` testCaseDefinitionId
      `Prelude.hashWithSalt` testCaseDefinitionName
      `Prelude.hashWithSalt` testCaseRunId
      `Prelude.hashWithSalt` testScenarios
      `Prelude.hashWithSalt` warnings

instance Prelude.NFData TestCaseRun where
  rnf TestCaseRun' {..} =
    Prelude.rnf endTime `Prelude.seq`
      Prelude.rnf failure `Prelude.seq`
        Prelude.rnf logUrl `Prelude.seq`
          Prelude.rnf startTime `Prelude.seq`
            Prelude.rnf status `Prelude.seq`
              Prelude.rnf testCaseDefinitionId `Prelude.seq`
                Prelude.rnf testCaseDefinitionName `Prelude.seq`
                  Prelude.rnf testCaseRunId `Prelude.seq`
                    Prelude.rnf testScenarios `Prelude.seq`
                      Prelude.rnf warnings
