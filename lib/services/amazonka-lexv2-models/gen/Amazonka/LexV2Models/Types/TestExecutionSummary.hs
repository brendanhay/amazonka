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
-- Module      : Amazonka.LexV2Models.Types.TestExecutionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestExecutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestExecutionApiMode
import Amazonka.LexV2Models.Types.TestExecutionModality
import Amazonka.LexV2Models.Types.TestExecutionStatus
import Amazonka.LexV2Models.Types.TestExecutionTarget
import qualified Amazonka.Prelude as Prelude

-- | Summarizes metadata about the test execution.
--
-- /See:/ 'newTestExecutionSummary' smart constructor.
data TestExecutionSummary = TestExecutionSummary'
  { -- | Specifies whether the API mode for the test execution is streaming or
    -- non-streaming.
    apiMode :: Prelude.Maybe TestExecutionApiMode,
    -- | The date and time at which the test execution was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time at which the test execution was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Contains information about the bot used for the test execution..
    target :: Prelude.Maybe TestExecutionTarget,
    -- | The unique identifier of the test execution.
    testExecutionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the data used for the test execution is written or
    -- spoken.
    testExecutionModality :: Prelude.Maybe TestExecutionModality,
    -- | The current status of the test execution.
    testExecutionStatus :: Prelude.Maybe TestExecutionStatus,
    -- | The unique identifier of the test set used in the test execution.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the test set used in the test execution.
    testSetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMode', 'testExecutionSummary_apiMode' - Specifies whether the API mode for the test execution is streaming or
-- non-streaming.
--
-- 'creationDateTime', 'testExecutionSummary_creationDateTime' - The date and time at which the test execution was created.
--
-- 'lastUpdatedDateTime', 'testExecutionSummary_lastUpdatedDateTime' - The date and time at which the test execution was last updated.
--
-- 'target', 'testExecutionSummary_target' - Contains information about the bot used for the test execution..
--
-- 'testExecutionId', 'testExecutionSummary_testExecutionId' - The unique identifier of the test execution.
--
-- 'testExecutionModality', 'testExecutionSummary_testExecutionModality' - Specifies whether the data used for the test execution is written or
-- spoken.
--
-- 'testExecutionStatus', 'testExecutionSummary_testExecutionStatus' - The current status of the test execution.
--
-- 'testSetId', 'testExecutionSummary_testSetId' - The unique identifier of the test set used in the test execution.
--
-- 'testSetName', 'testExecutionSummary_testSetName' - The name of the test set used in the test execution.
newTestExecutionSummary ::
  TestExecutionSummary
newTestExecutionSummary =
  TestExecutionSummary'
    { apiMode = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      target = Prelude.Nothing,
      testExecutionId = Prelude.Nothing,
      testExecutionModality = Prelude.Nothing,
      testExecutionStatus = Prelude.Nothing,
      testSetId = Prelude.Nothing,
      testSetName = Prelude.Nothing
    }

-- | Specifies whether the API mode for the test execution is streaming or
-- non-streaming.
testExecutionSummary_apiMode :: Lens.Lens' TestExecutionSummary (Prelude.Maybe TestExecutionApiMode)
testExecutionSummary_apiMode = Lens.lens (\TestExecutionSummary' {apiMode} -> apiMode) (\s@TestExecutionSummary' {} a -> s {apiMode = a} :: TestExecutionSummary)

-- | The date and time at which the test execution was created.
testExecutionSummary_creationDateTime :: Lens.Lens' TestExecutionSummary (Prelude.Maybe Prelude.UTCTime)
testExecutionSummary_creationDateTime = Lens.lens (\TestExecutionSummary' {creationDateTime} -> creationDateTime) (\s@TestExecutionSummary' {} a -> s {creationDateTime = a} :: TestExecutionSummary) Prelude.. Lens.mapping Data._Time

-- | The date and time at which the test execution was last updated.
testExecutionSummary_lastUpdatedDateTime :: Lens.Lens' TestExecutionSummary (Prelude.Maybe Prelude.UTCTime)
testExecutionSummary_lastUpdatedDateTime = Lens.lens (\TestExecutionSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@TestExecutionSummary' {} a -> s {lastUpdatedDateTime = a} :: TestExecutionSummary) Prelude.. Lens.mapping Data._Time

-- | Contains information about the bot used for the test execution..
testExecutionSummary_target :: Lens.Lens' TestExecutionSummary (Prelude.Maybe TestExecutionTarget)
testExecutionSummary_target = Lens.lens (\TestExecutionSummary' {target} -> target) (\s@TestExecutionSummary' {} a -> s {target = a} :: TestExecutionSummary)

-- | The unique identifier of the test execution.
testExecutionSummary_testExecutionId :: Lens.Lens' TestExecutionSummary (Prelude.Maybe Prelude.Text)
testExecutionSummary_testExecutionId = Lens.lens (\TestExecutionSummary' {testExecutionId} -> testExecutionId) (\s@TestExecutionSummary' {} a -> s {testExecutionId = a} :: TestExecutionSummary)

-- | Specifies whether the data used for the test execution is written or
-- spoken.
testExecutionSummary_testExecutionModality :: Lens.Lens' TestExecutionSummary (Prelude.Maybe TestExecutionModality)
testExecutionSummary_testExecutionModality = Lens.lens (\TestExecutionSummary' {testExecutionModality} -> testExecutionModality) (\s@TestExecutionSummary' {} a -> s {testExecutionModality = a} :: TestExecutionSummary)

-- | The current status of the test execution.
testExecutionSummary_testExecutionStatus :: Lens.Lens' TestExecutionSummary (Prelude.Maybe TestExecutionStatus)
testExecutionSummary_testExecutionStatus = Lens.lens (\TestExecutionSummary' {testExecutionStatus} -> testExecutionStatus) (\s@TestExecutionSummary' {} a -> s {testExecutionStatus = a} :: TestExecutionSummary)

-- | The unique identifier of the test set used in the test execution.
testExecutionSummary_testSetId :: Lens.Lens' TestExecutionSummary (Prelude.Maybe Prelude.Text)
testExecutionSummary_testSetId = Lens.lens (\TestExecutionSummary' {testSetId} -> testSetId) (\s@TestExecutionSummary' {} a -> s {testSetId = a} :: TestExecutionSummary)

-- | The name of the test set used in the test execution.
testExecutionSummary_testSetName :: Lens.Lens' TestExecutionSummary (Prelude.Maybe Prelude.Text)
testExecutionSummary_testSetName = Lens.lens (\TestExecutionSummary' {testSetName} -> testSetName) (\s@TestExecutionSummary' {} a -> s {testSetName = a} :: TestExecutionSummary)

instance Data.FromJSON TestExecutionSummary where
  parseJSON =
    Data.withObject
      "TestExecutionSummary"
      ( \x ->
          TestExecutionSummary'
            Prelude.<$> (x Data..:? "apiMode")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "target")
            Prelude.<*> (x Data..:? "testExecutionId")
            Prelude.<*> (x Data..:? "testExecutionModality")
            Prelude.<*> (x Data..:? "testExecutionStatus")
            Prelude.<*> (x Data..:? "testSetId")
            Prelude.<*> (x Data..:? "testSetName")
      )

instance Prelude.Hashable TestExecutionSummary where
  hashWithSalt _salt TestExecutionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` apiMode
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` testExecutionId
      `Prelude.hashWithSalt` testExecutionModality
      `Prelude.hashWithSalt` testExecutionStatus
      `Prelude.hashWithSalt` testSetId
      `Prelude.hashWithSalt` testSetName

instance Prelude.NFData TestExecutionSummary where
  rnf TestExecutionSummary' {..} =
    Prelude.rnf apiMode
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf testExecutionId
      `Prelude.seq` Prelude.rnf testExecutionModality
      `Prelude.seq` Prelude.rnf testExecutionStatus
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf testSetName
