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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.TestCaseScenario
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.TestCaseScenario where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types.TestCaseScenarioStatus
import Amazonka.IoTDeviceAdvisor.Types.TestCaseScenarioType
import qualified Amazonka.Prelude as Prelude

-- | Provides test case scenario.
--
-- /See:/ 'newTestCaseScenario' smart constructor.
data TestCaseScenario = TestCaseScenario'
  { -- | Provides the test case scenario status. Status is one of the following:
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
    status :: Prelude.Maybe TestCaseScenarioStatus,
    -- | Provides test case scenario type. Type is one of the following:
    --
    -- -   Advanced
    --
    -- -   Basic
    testCaseScenarioType :: Prelude.Maybe TestCaseScenarioType,
    -- | Provides test case scenario failure result.
    failure :: Prelude.Maybe Prelude.Text,
    systemMessage :: Prelude.Maybe Prelude.Text,
    -- | Provides test case scenario ID.
    testCaseScenarioId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestCaseScenario' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'testCaseScenario_status' - Provides the test case scenario status. Status is one of the following:
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
-- 'testCaseScenarioType', 'testCaseScenario_testCaseScenarioType' - Provides test case scenario type. Type is one of the following:
--
-- -   Advanced
--
-- -   Basic
--
-- 'failure', 'testCaseScenario_failure' - Provides test case scenario failure result.
--
-- 'systemMessage', 'testCaseScenario_systemMessage' -
--
-- 'testCaseScenarioId', 'testCaseScenario_testCaseScenarioId' - Provides test case scenario ID.
newTestCaseScenario ::
  TestCaseScenario
newTestCaseScenario =
  TestCaseScenario'
    { status = Prelude.Nothing,
      testCaseScenarioType = Prelude.Nothing,
      failure = Prelude.Nothing,
      systemMessage = Prelude.Nothing,
      testCaseScenarioId = Prelude.Nothing
    }

-- | Provides the test case scenario status. Status is one of the following:
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
testCaseScenario_status :: Lens.Lens' TestCaseScenario (Prelude.Maybe TestCaseScenarioStatus)
testCaseScenario_status = Lens.lens (\TestCaseScenario' {status} -> status) (\s@TestCaseScenario' {} a -> s {status = a} :: TestCaseScenario)

-- | Provides test case scenario type. Type is one of the following:
--
-- -   Advanced
--
-- -   Basic
testCaseScenario_testCaseScenarioType :: Lens.Lens' TestCaseScenario (Prelude.Maybe TestCaseScenarioType)
testCaseScenario_testCaseScenarioType = Lens.lens (\TestCaseScenario' {testCaseScenarioType} -> testCaseScenarioType) (\s@TestCaseScenario' {} a -> s {testCaseScenarioType = a} :: TestCaseScenario)

-- | Provides test case scenario failure result.
testCaseScenario_failure :: Lens.Lens' TestCaseScenario (Prelude.Maybe Prelude.Text)
testCaseScenario_failure = Lens.lens (\TestCaseScenario' {failure} -> failure) (\s@TestCaseScenario' {} a -> s {failure = a} :: TestCaseScenario)

-- |
testCaseScenario_systemMessage :: Lens.Lens' TestCaseScenario (Prelude.Maybe Prelude.Text)
testCaseScenario_systemMessage = Lens.lens (\TestCaseScenario' {systemMessage} -> systemMessage) (\s@TestCaseScenario' {} a -> s {systemMessage = a} :: TestCaseScenario)

-- | Provides test case scenario ID.
testCaseScenario_testCaseScenarioId :: Lens.Lens' TestCaseScenario (Prelude.Maybe Prelude.Text)
testCaseScenario_testCaseScenarioId = Lens.lens (\TestCaseScenario' {testCaseScenarioId} -> testCaseScenarioId) (\s@TestCaseScenario' {} a -> s {testCaseScenarioId = a} :: TestCaseScenario)

instance Data.FromJSON TestCaseScenario where
  parseJSON =
    Data.withObject
      "TestCaseScenario"
      ( \x ->
          TestCaseScenario'
            Prelude.<$> (x Data..:? "status")
            Prelude.<*> (x Data..:? "testCaseScenarioType")
            Prelude.<*> (x Data..:? "failure")
            Prelude.<*> (x Data..:? "systemMessage")
            Prelude.<*> (x Data..:? "testCaseScenarioId")
      )

instance Prelude.Hashable TestCaseScenario where
  hashWithSalt _salt TestCaseScenario' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` testCaseScenarioType
      `Prelude.hashWithSalt` failure
      `Prelude.hashWithSalt` systemMessage
      `Prelude.hashWithSalt` testCaseScenarioId

instance Prelude.NFData TestCaseScenario where
  rnf TestCaseScenario' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf testCaseScenarioType
      `Prelude.seq` Prelude.rnf failure
      `Prelude.seq` Prelude.rnf systemMessage
      `Prelude.seq` Prelude.rnf testCaseScenarioId
