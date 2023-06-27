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
-- Module      : Amazonka.LexV2Models.Types.TestSetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestSetModality
import Amazonka.LexV2Models.Types.TestSetStatus
import Amazonka.LexV2Models.Types.TestSetStorageLocation
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about the test set.
--
-- /See:/ 'newTestSetSummary' smart constructor.
data TestSetSummary = TestSetSummary'
  { -- | The date and time at which the test set was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the test set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the test set was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the test set contains written or spoken data.
    modality :: Prelude.Maybe TestSetModality,
    -- | The number of turns in the test set.
    numTurns :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permission to
    -- access the test set.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the test set.
    status :: Prelude.Maybe TestSetStatus,
    -- | Contains information about the location at which the test set is stored.
    storageLocation :: Prelude.Maybe TestSetStorageLocation,
    -- | The unique identifier of the test set.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the test set.
    testSetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'testSetSummary_creationDateTime' - The date and time at which the test set was created.
--
-- 'description', 'testSetSummary_description' - The description of the test set.
--
-- 'lastUpdatedDateTime', 'testSetSummary_lastUpdatedDateTime' - The date and time at which the test set was last updated.
--
-- 'modality', 'testSetSummary_modality' - Specifies whether the test set contains written or spoken data.
--
-- 'numTurns', 'testSetSummary_numTurns' - The number of turns in the test set.
--
-- 'roleArn', 'testSetSummary_roleArn' - The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the test set.
--
-- 'status', 'testSetSummary_status' - The status of the test set.
--
-- 'storageLocation', 'testSetSummary_storageLocation' - Contains information about the location at which the test set is stored.
--
-- 'testSetId', 'testSetSummary_testSetId' - The unique identifier of the test set.
--
-- 'testSetName', 'testSetSummary_testSetName' - The name of the test set.
newTestSetSummary ::
  TestSetSummary
newTestSetSummary =
  TestSetSummary'
    { creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      modality = Prelude.Nothing,
      numTurns = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      testSetId = Prelude.Nothing,
      testSetName = Prelude.Nothing
    }

-- | The date and time at which the test set was created.
testSetSummary_creationDateTime :: Lens.Lens' TestSetSummary (Prelude.Maybe Prelude.UTCTime)
testSetSummary_creationDateTime = Lens.lens (\TestSetSummary' {creationDateTime} -> creationDateTime) (\s@TestSetSummary' {} a -> s {creationDateTime = a} :: TestSetSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the test set.
testSetSummary_description :: Lens.Lens' TestSetSummary (Prelude.Maybe Prelude.Text)
testSetSummary_description = Lens.lens (\TestSetSummary' {description} -> description) (\s@TestSetSummary' {} a -> s {description = a} :: TestSetSummary)

-- | The date and time at which the test set was last updated.
testSetSummary_lastUpdatedDateTime :: Lens.Lens' TestSetSummary (Prelude.Maybe Prelude.UTCTime)
testSetSummary_lastUpdatedDateTime = Lens.lens (\TestSetSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@TestSetSummary' {} a -> s {lastUpdatedDateTime = a} :: TestSetSummary) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the test set contains written or spoken data.
testSetSummary_modality :: Lens.Lens' TestSetSummary (Prelude.Maybe TestSetModality)
testSetSummary_modality = Lens.lens (\TestSetSummary' {modality} -> modality) (\s@TestSetSummary' {} a -> s {modality = a} :: TestSetSummary)

-- | The number of turns in the test set.
testSetSummary_numTurns :: Lens.Lens' TestSetSummary (Prelude.Maybe Prelude.Int)
testSetSummary_numTurns = Lens.lens (\TestSetSummary' {numTurns} -> numTurns) (\s@TestSetSummary' {} a -> s {numTurns = a} :: TestSetSummary)

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the test set.
testSetSummary_roleArn :: Lens.Lens' TestSetSummary (Prelude.Maybe Prelude.Text)
testSetSummary_roleArn = Lens.lens (\TestSetSummary' {roleArn} -> roleArn) (\s@TestSetSummary' {} a -> s {roleArn = a} :: TestSetSummary)

-- | The status of the test set.
testSetSummary_status :: Lens.Lens' TestSetSummary (Prelude.Maybe TestSetStatus)
testSetSummary_status = Lens.lens (\TestSetSummary' {status} -> status) (\s@TestSetSummary' {} a -> s {status = a} :: TestSetSummary)

-- | Contains information about the location at which the test set is stored.
testSetSummary_storageLocation :: Lens.Lens' TestSetSummary (Prelude.Maybe TestSetStorageLocation)
testSetSummary_storageLocation = Lens.lens (\TestSetSummary' {storageLocation} -> storageLocation) (\s@TestSetSummary' {} a -> s {storageLocation = a} :: TestSetSummary)

-- | The unique identifier of the test set.
testSetSummary_testSetId :: Lens.Lens' TestSetSummary (Prelude.Maybe Prelude.Text)
testSetSummary_testSetId = Lens.lens (\TestSetSummary' {testSetId} -> testSetId) (\s@TestSetSummary' {} a -> s {testSetId = a} :: TestSetSummary)

-- | The name of the test set.
testSetSummary_testSetName :: Lens.Lens' TestSetSummary (Prelude.Maybe Prelude.Text)
testSetSummary_testSetName = Lens.lens (\TestSetSummary' {testSetName} -> testSetName) (\s@TestSetSummary' {} a -> s {testSetName = a} :: TestSetSummary)

instance Data.FromJSON TestSetSummary where
  parseJSON =
    Data.withObject
      "TestSetSummary"
      ( \x ->
          TestSetSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "modality")
            Prelude.<*> (x Data..:? "numTurns")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "storageLocation")
            Prelude.<*> (x Data..:? "testSetId")
            Prelude.<*> (x Data..:? "testSetName")
      )

instance Prelude.Hashable TestSetSummary where
  hashWithSalt _salt TestSetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` modality
      `Prelude.hashWithSalt` numTurns
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storageLocation
      `Prelude.hashWithSalt` testSetId
      `Prelude.hashWithSalt` testSetName

instance Prelude.NFData TestSetSummary where
  rnf TestSetSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf modality
      `Prelude.seq` Prelude.rnf numTurns
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf testSetName
