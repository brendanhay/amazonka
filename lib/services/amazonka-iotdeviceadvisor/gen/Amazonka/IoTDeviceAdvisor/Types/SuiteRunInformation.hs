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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.SuiteRunInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.SuiteRunInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the suite run.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions SuiteRunInformation>
-- action.
--
-- /See:/ 'newSuiteRunInformation' smart constructor.
data SuiteRunInformation = SuiteRunInformation'
  { -- | Date (in Unix epoch time) when the suite run was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Date (in Unix epoch time) when the suite run ended.
    endAt :: Prelude.Maybe Data.POSIX,
    -- | Number of test cases that failed in the suite run.
    failed :: Prelude.Maybe Prelude.Natural,
    -- | Number of test cases that passed in the suite run.
    passed :: Prelude.Maybe Prelude.Natural,
    -- | Date (in Unix epoch time) when the suite run was started.
    startedAt :: Prelude.Maybe Data.POSIX,
    -- | Status of the suite run.
    status :: Prelude.Maybe SuiteRunStatus,
    -- | Suite definition ID of the suite run.
    suiteDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | Suite definition name of the suite run.
    suiteDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Suite definition version of the suite run.
    suiteDefinitionVersion :: Prelude.Maybe Prelude.Text,
    -- | Suite run ID of the suite run.
    suiteRunId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuiteRunInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'suiteRunInformation_createdAt' - Date (in Unix epoch time) when the suite run was created.
--
-- 'endAt', 'suiteRunInformation_endAt' - Date (in Unix epoch time) when the suite run ended.
--
-- 'failed', 'suiteRunInformation_failed' - Number of test cases that failed in the suite run.
--
-- 'passed', 'suiteRunInformation_passed' - Number of test cases that passed in the suite run.
--
-- 'startedAt', 'suiteRunInformation_startedAt' - Date (in Unix epoch time) when the suite run was started.
--
-- 'status', 'suiteRunInformation_status' - Status of the suite run.
--
-- 'suiteDefinitionId', 'suiteRunInformation_suiteDefinitionId' - Suite definition ID of the suite run.
--
-- 'suiteDefinitionName', 'suiteRunInformation_suiteDefinitionName' - Suite definition name of the suite run.
--
-- 'suiteDefinitionVersion', 'suiteRunInformation_suiteDefinitionVersion' - Suite definition version of the suite run.
--
-- 'suiteRunId', 'suiteRunInformation_suiteRunId' - Suite run ID of the suite run.
newSuiteRunInformation ::
  SuiteRunInformation
newSuiteRunInformation =
  SuiteRunInformation'
    { createdAt = Prelude.Nothing,
      endAt = Prelude.Nothing,
      failed = Prelude.Nothing,
      passed = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      suiteDefinitionId = Prelude.Nothing,
      suiteDefinitionName = Prelude.Nothing,
      suiteDefinitionVersion = Prelude.Nothing,
      suiteRunId = Prelude.Nothing
    }

-- | Date (in Unix epoch time) when the suite run was created.
suiteRunInformation_createdAt :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.UTCTime)
suiteRunInformation_createdAt = Lens.lens (\SuiteRunInformation' {createdAt} -> createdAt) (\s@SuiteRunInformation' {} a -> s {createdAt = a} :: SuiteRunInformation) Prelude.. Lens.mapping Data._Time

-- | Date (in Unix epoch time) when the suite run ended.
suiteRunInformation_endAt :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.UTCTime)
suiteRunInformation_endAt = Lens.lens (\SuiteRunInformation' {endAt} -> endAt) (\s@SuiteRunInformation' {} a -> s {endAt = a} :: SuiteRunInformation) Prelude.. Lens.mapping Data._Time

-- | Number of test cases that failed in the suite run.
suiteRunInformation_failed :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.Natural)
suiteRunInformation_failed = Lens.lens (\SuiteRunInformation' {failed} -> failed) (\s@SuiteRunInformation' {} a -> s {failed = a} :: SuiteRunInformation)

-- | Number of test cases that passed in the suite run.
suiteRunInformation_passed :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.Natural)
suiteRunInformation_passed = Lens.lens (\SuiteRunInformation' {passed} -> passed) (\s@SuiteRunInformation' {} a -> s {passed = a} :: SuiteRunInformation)

-- | Date (in Unix epoch time) when the suite run was started.
suiteRunInformation_startedAt :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.UTCTime)
suiteRunInformation_startedAt = Lens.lens (\SuiteRunInformation' {startedAt} -> startedAt) (\s@SuiteRunInformation' {} a -> s {startedAt = a} :: SuiteRunInformation) Prelude.. Lens.mapping Data._Time

-- | Status of the suite run.
suiteRunInformation_status :: Lens.Lens' SuiteRunInformation (Prelude.Maybe SuiteRunStatus)
suiteRunInformation_status = Lens.lens (\SuiteRunInformation' {status} -> status) (\s@SuiteRunInformation' {} a -> s {status = a} :: SuiteRunInformation)

-- | Suite definition ID of the suite run.
suiteRunInformation_suiteDefinitionId :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.Text)
suiteRunInformation_suiteDefinitionId = Lens.lens (\SuiteRunInformation' {suiteDefinitionId} -> suiteDefinitionId) (\s@SuiteRunInformation' {} a -> s {suiteDefinitionId = a} :: SuiteRunInformation)

-- | Suite definition name of the suite run.
suiteRunInformation_suiteDefinitionName :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.Text)
suiteRunInformation_suiteDefinitionName = Lens.lens (\SuiteRunInformation' {suiteDefinitionName} -> suiteDefinitionName) (\s@SuiteRunInformation' {} a -> s {suiteDefinitionName = a} :: SuiteRunInformation)

-- | Suite definition version of the suite run.
suiteRunInformation_suiteDefinitionVersion :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.Text)
suiteRunInformation_suiteDefinitionVersion = Lens.lens (\SuiteRunInformation' {suiteDefinitionVersion} -> suiteDefinitionVersion) (\s@SuiteRunInformation' {} a -> s {suiteDefinitionVersion = a} :: SuiteRunInformation)

-- | Suite run ID of the suite run.
suiteRunInformation_suiteRunId :: Lens.Lens' SuiteRunInformation (Prelude.Maybe Prelude.Text)
suiteRunInformation_suiteRunId = Lens.lens (\SuiteRunInformation' {suiteRunId} -> suiteRunId) (\s@SuiteRunInformation' {} a -> s {suiteRunId = a} :: SuiteRunInformation)

instance Data.FromJSON SuiteRunInformation where
  parseJSON =
    Data.withObject
      "SuiteRunInformation"
      ( \x ->
          SuiteRunInformation'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "endAt")
            Prelude.<*> (x Data..:? "failed")
            Prelude.<*> (x Data..:? "passed")
            Prelude.<*> (x Data..:? "startedAt")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "suiteDefinitionId")
            Prelude.<*> (x Data..:? "suiteDefinitionName")
            Prelude.<*> (x Data..:? "suiteDefinitionVersion")
            Prelude.<*> (x Data..:? "suiteRunId")
      )

instance Prelude.Hashable SuiteRunInformation where
  hashWithSalt _salt SuiteRunInformation' {..} =
    _salt `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` endAt
      `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` passed
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` suiteDefinitionId
      `Prelude.hashWithSalt` suiteDefinitionName
      `Prelude.hashWithSalt` suiteDefinitionVersion
      `Prelude.hashWithSalt` suiteRunId

instance Prelude.NFData SuiteRunInformation where
  rnf SuiteRunInformation' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf endAt
      `Prelude.seq` Prelude.rnf failed
      `Prelude.seq` Prelude.rnf passed
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf suiteDefinitionId
      `Prelude.seq` Prelude.rnf suiteDefinitionName
      `Prelude.seq` Prelude.rnf suiteDefinitionVersion
      `Prelude.seq` Prelude.rnf suiteRunId
