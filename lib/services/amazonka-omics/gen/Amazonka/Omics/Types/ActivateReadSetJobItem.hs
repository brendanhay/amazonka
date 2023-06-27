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
-- Module      : Amazonka.Omics.Types.ActivateReadSetJobItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ActivateReadSetJobItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetActivationJobStatus
import qualified Amazonka.Prelude as Prelude

-- | A read set activation job.
--
-- /See:/ 'newActivateReadSetJobItem' smart constructor.
data ActivateReadSetJobItem = ActivateReadSetJobItem'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s status.
    status :: ReadSetActivationJobStatus,
    -- | When the job was created.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateReadSetJobItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'activateReadSetJobItem_completionTime' - When the job completed.
--
-- 'id', 'activateReadSetJobItem_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'activateReadSetJobItem_sequenceStoreId' - The job\'s sequence store ID.
--
-- 'status', 'activateReadSetJobItem_status' - The job\'s status.
--
-- 'creationTime', 'activateReadSetJobItem_creationTime' - When the job was created.
newActivateReadSetJobItem ::
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'status'
  ReadSetActivationJobStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  ActivateReadSetJobItem
newActivateReadSetJobItem
  pId_
  pSequenceStoreId_
  pStatus_
  pCreationTime_ =
    ActivateReadSetJobItem'
      { completionTime =
          Prelude.Nothing,
        id = pId_,
        sequenceStoreId = pSequenceStoreId_,
        status = pStatus_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | When the job completed.
activateReadSetJobItem_completionTime :: Lens.Lens' ActivateReadSetJobItem (Prelude.Maybe Prelude.UTCTime)
activateReadSetJobItem_completionTime = Lens.lens (\ActivateReadSetJobItem' {completionTime} -> completionTime) (\s@ActivateReadSetJobItem' {} a -> s {completionTime = a} :: ActivateReadSetJobItem) Prelude.. Lens.mapping Data._Time

-- | The job\'s ID.
activateReadSetJobItem_id :: Lens.Lens' ActivateReadSetJobItem Prelude.Text
activateReadSetJobItem_id = Lens.lens (\ActivateReadSetJobItem' {id} -> id) (\s@ActivateReadSetJobItem' {} a -> s {id = a} :: ActivateReadSetJobItem)

-- | The job\'s sequence store ID.
activateReadSetJobItem_sequenceStoreId :: Lens.Lens' ActivateReadSetJobItem Prelude.Text
activateReadSetJobItem_sequenceStoreId = Lens.lens (\ActivateReadSetJobItem' {sequenceStoreId} -> sequenceStoreId) (\s@ActivateReadSetJobItem' {} a -> s {sequenceStoreId = a} :: ActivateReadSetJobItem)

-- | The job\'s status.
activateReadSetJobItem_status :: Lens.Lens' ActivateReadSetJobItem ReadSetActivationJobStatus
activateReadSetJobItem_status = Lens.lens (\ActivateReadSetJobItem' {status} -> status) (\s@ActivateReadSetJobItem' {} a -> s {status = a} :: ActivateReadSetJobItem)

-- | When the job was created.
activateReadSetJobItem_creationTime :: Lens.Lens' ActivateReadSetJobItem Prelude.UTCTime
activateReadSetJobItem_creationTime = Lens.lens (\ActivateReadSetJobItem' {creationTime} -> creationTime) (\s@ActivateReadSetJobItem' {} a -> s {creationTime = a} :: ActivateReadSetJobItem) Prelude.. Data._Time

instance Data.FromJSON ActivateReadSetJobItem where
  parseJSON =
    Data.withObject
      "ActivateReadSetJobItem"
      ( \x ->
          ActivateReadSetJobItem'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "sequenceStoreId")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "creationTime")
      )

instance Prelude.Hashable ActivateReadSetJobItem where
  hashWithSalt _salt ActivateReadSetJobItem' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ActivateReadSetJobItem where
  rnf ActivateReadSetJobItem' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
