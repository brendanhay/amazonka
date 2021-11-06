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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.TestCaseRun where

import qualified Amazonka.Core as Core
import Amazonka.IoTDeviceAdvisor.Types.Status
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides test case run.
--
-- /See:/ 'newTestCaseRun' smart constructor.
data TestCaseRun = TestCaseRun'
  { -- | Provides test case run status.
    status :: Prelude.Maybe Status,
    -- | Provides test case run log Url.
    logUrl :: Prelude.Maybe Prelude.Text,
    -- | Provides test case run start time.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Provides test case run Id.
    testCaseRunId :: Prelude.Maybe Prelude.Text,
    -- | Provides test case run warnings.
    warnings :: Prelude.Maybe Prelude.Text,
    -- | Provides test case run end time.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Provides test case run definition Id.
    testCaseDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | Provides test case run failure result.
    failure :: Prelude.Maybe Prelude.Text,
    -- | Provides test case run definition Name.
    testCaseDefinitionName :: Prelude.Maybe Prelude.Text
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
-- 'status', 'testCaseRun_status' - Provides test case run status.
--
-- 'logUrl', 'testCaseRun_logUrl' - Provides test case run log Url.
--
-- 'startTime', 'testCaseRun_startTime' - Provides test case run start time.
--
-- 'testCaseRunId', 'testCaseRun_testCaseRunId' - Provides test case run Id.
--
-- 'warnings', 'testCaseRun_warnings' - Provides test case run warnings.
--
-- 'endTime', 'testCaseRun_endTime' - Provides test case run end time.
--
-- 'testCaseDefinitionId', 'testCaseRun_testCaseDefinitionId' - Provides test case run definition Id.
--
-- 'failure', 'testCaseRun_failure' - Provides test case run failure result.
--
-- 'testCaseDefinitionName', 'testCaseRun_testCaseDefinitionName' - Provides test case run definition Name.
newTestCaseRun ::
  TestCaseRun
newTestCaseRun =
  TestCaseRun'
    { status = Prelude.Nothing,
      logUrl = Prelude.Nothing,
      startTime = Prelude.Nothing,
      testCaseRunId = Prelude.Nothing,
      warnings = Prelude.Nothing,
      endTime = Prelude.Nothing,
      testCaseDefinitionId = Prelude.Nothing,
      failure = Prelude.Nothing,
      testCaseDefinitionName = Prelude.Nothing
    }

-- | Provides test case run status.
testCaseRun_status :: Lens.Lens' TestCaseRun (Prelude.Maybe Status)
testCaseRun_status = Lens.lens (\TestCaseRun' {status} -> status) (\s@TestCaseRun' {} a -> s {status = a} :: TestCaseRun)

-- | Provides test case run log Url.
testCaseRun_logUrl :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_logUrl = Lens.lens (\TestCaseRun' {logUrl} -> logUrl) (\s@TestCaseRun' {} a -> s {logUrl = a} :: TestCaseRun)

-- | Provides test case run start time.
testCaseRun_startTime :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.UTCTime)
testCaseRun_startTime = Lens.lens (\TestCaseRun' {startTime} -> startTime) (\s@TestCaseRun' {} a -> s {startTime = a} :: TestCaseRun) Prelude.. Lens.mapping Core._Time

-- | Provides test case run Id.
testCaseRun_testCaseRunId :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_testCaseRunId = Lens.lens (\TestCaseRun' {testCaseRunId} -> testCaseRunId) (\s@TestCaseRun' {} a -> s {testCaseRunId = a} :: TestCaseRun)

-- | Provides test case run warnings.
testCaseRun_warnings :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_warnings = Lens.lens (\TestCaseRun' {warnings} -> warnings) (\s@TestCaseRun' {} a -> s {warnings = a} :: TestCaseRun)

-- | Provides test case run end time.
testCaseRun_endTime :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.UTCTime)
testCaseRun_endTime = Lens.lens (\TestCaseRun' {endTime} -> endTime) (\s@TestCaseRun' {} a -> s {endTime = a} :: TestCaseRun) Prelude.. Lens.mapping Core._Time

-- | Provides test case run definition Id.
testCaseRun_testCaseDefinitionId :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_testCaseDefinitionId = Lens.lens (\TestCaseRun' {testCaseDefinitionId} -> testCaseDefinitionId) (\s@TestCaseRun' {} a -> s {testCaseDefinitionId = a} :: TestCaseRun)

-- | Provides test case run failure result.
testCaseRun_failure :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_failure = Lens.lens (\TestCaseRun' {failure} -> failure) (\s@TestCaseRun' {} a -> s {failure = a} :: TestCaseRun)

-- | Provides test case run definition Name.
testCaseRun_testCaseDefinitionName :: Lens.Lens' TestCaseRun (Prelude.Maybe Prelude.Text)
testCaseRun_testCaseDefinitionName = Lens.lens (\TestCaseRun' {testCaseDefinitionName} -> testCaseDefinitionName) (\s@TestCaseRun' {} a -> s {testCaseDefinitionName = a} :: TestCaseRun)

instance Core.FromJSON TestCaseRun where
  parseJSON =
    Core.withObject
      "TestCaseRun"
      ( \x ->
          TestCaseRun'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "logUrl")
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> (x Core..:? "testCaseRunId")
            Prelude.<*> (x Core..:? "warnings")
            Prelude.<*> (x Core..:? "endTime")
            Prelude.<*> (x Core..:? "testCaseDefinitionId")
            Prelude.<*> (x Core..:? "failure")
            Prelude.<*> (x Core..:? "testCaseDefinitionName")
      )

instance Prelude.Hashable TestCaseRun

instance Prelude.NFData TestCaseRun
