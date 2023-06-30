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
-- Module      : Amazonka.Omics.Types.ImportReadSetJobItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ImportReadSetJobItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetImportJobStatus
import qualified Amazonka.Prelude as Prelude

-- | An import read set job.
--
-- /See:/ 'newImportReadSetJobItem' smart constructor.
data ImportReadSetJobItem = ImportReadSetJobItem'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s status.
    status :: ReadSetImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportReadSetJobItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'importReadSetJobItem_completionTime' - When the job completed.
--
-- 'creationTime', 'importReadSetJobItem_creationTime' - When the job was created.
--
-- 'id', 'importReadSetJobItem_id' - The job\'s ID.
--
-- 'roleArn', 'importReadSetJobItem_roleArn' - The job\'s service role ARN.
--
-- 'sequenceStoreId', 'importReadSetJobItem_sequenceStoreId' - The job\'s sequence store ID.
--
-- 'status', 'importReadSetJobItem_status' - The job\'s status.
newImportReadSetJobItem ::
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'status'
  ReadSetImportJobStatus ->
  ImportReadSetJobItem
newImportReadSetJobItem
  pCreationTime_
  pId_
  pRoleArn_
  pSequenceStoreId_
  pStatus_ =
    ImportReadSetJobItem'
      { completionTime =
          Prelude.Nothing,
        creationTime = Data._Time Lens.# pCreationTime_,
        id = pId_,
        roleArn = pRoleArn_,
        sequenceStoreId = pSequenceStoreId_,
        status = pStatus_
      }

-- | When the job completed.
importReadSetJobItem_completionTime :: Lens.Lens' ImportReadSetJobItem (Prelude.Maybe Prelude.UTCTime)
importReadSetJobItem_completionTime = Lens.lens (\ImportReadSetJobItem' {completionTime} -> completionTime) (\s@ImportReadSetJobItem' {} a -> s {completionTime = a} :: ImportReadSetJobItem) Prelude.. Lens.mapping Data._Time

-- | When the job was created.
importReadSetJobItem_creationTime :: Lens.Lens' ImportReadSetJobItem Prelude.UTCTime
importReadSetJobItem_creationTime = Lens.lens (\ImportReadSetJobItem' {creationTime} -> creationTime) (\s@ImportReadSetJobItem' {} a -> s {creationTime = a} :: ImportReadSetJobItem) Prelude.. Data._Time

-- | The job\'s ID.
importReadSetJobItem_id :: Lens.Lens' ImportReadSetJobItem Prelude.Text
importReadSetJobItem_id = Lens.lens (\ImportReadSetJobItem' {id} -> id) (\s@ImportReadSetJobItem' {} a -> s {id = a} :: ImportReadSetJobItem)

-- | The job\'s service role ARN.
importReadSetJobItem_roleArn :: Lens.Lens' ImportReadSetJobItem Prelude.Text
importReadSetJobItem_roleArn = Lens.lens (\ImportReadSetJobItem' {roleArn} -> roleArn) (\s@ImportReadSetJobItem' {} a -> s {roleArn = a} :: ImportReadSetJobItem)

-- | The job\'s sequence store ID.
importReadSetJobItem_sequenceStoreId :: Lens.Lens' ImportReadSetJobItem Prelude.Text
importReadSetJobItem_sequenceStoreId = Lens.lens (\ImportReadSetJobItem' {sequenceStoreId} -> sequenceStoreId) (\s@ImportReadSetJobItem' {} a -> s {sequenceStoreId = a} :: ImportReadSetJobItem)

-- | The job\'s status.
importReadSetJobItem_status :: Lens.Lens' ImportReadSetJobItem ReadSetImportJobStatus
importReadSetJobItem_status = Lens.lens (\ImportReadSetJobItem' {status} -> status) (\s@ImportReadSetJobItem' {} a -> s {status = a} :: ImportReadSetJobItem)

instance Data.FromJSON ImportReadSetJobItem where
  parseJSON =
    Data.withObject
      "ImportReadSetJobItem"
      ( \x ->
          ImportReadSetJobItem'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "sequenceStoreId")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ImportReadSetJobItem where
  hashWithSalt _salt ImportReadSetJobItem' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImportReadSetJobItem where
  rnf ImportReadSetJobItem' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf status
