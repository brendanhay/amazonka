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
-- Module      : Amazonka.Omics.Types.VariantImportJobItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.VariantImportJobItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | A variant import job.
--
-- /See:/ 'newVariantImportJobItem' smart constructor.
data VariantImportJobItem = VariantImportJobItem'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | The job\'s left normalization setting.
    runLeftNormalization :: Prelude.Maybe Prelude.Bool,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s destination variant store.
    destinationName :: Prelude.Text,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s status.
    status :: JobStatus,
    -- | When the job was updated.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariantImportJobItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'variantImportJobItem_completionTime' - When the job completed.
--
-- 'runLeftNormalization', 'variantImportJobItem_runLeftNormalization' - The job\'s left normalization setting.
--
-- 'creationTime', 'variantImportJobItem_creationTime' - When the job was created.
--
-- 'destinationName', 'variantImportJobItem_destinationName' - The job\'s destination variant store.
--
-- 'id', 'variantImportJobItem_id' - The job\'s ID.
--
-- 'roleArn', 'variantImportJobItem_roleArn' - The job\'s service role ARN.
--
-- 'status', 'variantImportJobItem_status' - The job\'s status.
--
-- 'updateTime', 'variantImportJobItem_updateTime' - When the job was updated.
newVariantImportJobItem ::
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  JobStatus ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  VariantImportJobItem
newVariantImportJobItem
  pCreationTime_
  pDestinationName_
  pId_
  pRoleArn_
  pStatus_
  pUpdateTime_ =
    VariantImportJobItem'
      { completionTime =
          Prelude.Nothing,
        runLeftNormalization = Prelude.Nothing,
        creationTime = Data._Time Lens.# pCreationTime_,
        destinationName = pDestinationName_,
        id = pId_,
        roleArn = pRoleArn_,
        status = pStatus_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | When the job completed.
variantImportJobItem_completionTime :: Lens.Lens' VariantImportJobItem (Prelude.Maybe Prelude.UTCTime)
variantImportJobItem_completionTime = Lens.lens (\VariantImportJobItem' {completionTime} -> completionTime) (\s@VariantImportJobItem' {} a -> s {completionTime = a} :: VariantImportJobItem) Prelude.. Lens.mapping Data._Time

-- | The job\'s left normalization setting.
variantImportJobItem_runLeftNormalization :: Lens.Lens' VariantImportJobItem (Prelude.Maybe Prelude.Bool)
variantImportJobItem_runLeftNormalization = Lens.lens (\VariantImportJobItem' {runLeftNormalization} -> runLeftNormalization) (\s@VariantImportJobItem' {} a -> s {runLeftNormalization = a} :: VariantImportJobItem)

-- | When the job was created.
variantImportJobItem_creationTime :: Lens.Lens' VariantImportJobItem Prelude.UTCTime
variantImportJobItem_creationTime = Lens.lens (\VariantImportJobItem' {creationTime} -> creationTime) (\s@VariantImportJobItem' {} a -> s {creationTime = a} :: VariantImportJobItem) Prelude.. Data._Time

-- | The job\'s destination variant store.
variantImportJobItem_destinationName :: Lens.Lens' VariantImportJobItem Prelude.Text
variantImportJobItem_destinationName = Lens.lens (\VariantImportJobItem' {destinationName} -> destinationName) (\s@VariantImportJobItem' {} a -> s {destinationName = a} :: VariantImportJobItem)

-- | The job\'s ID.
variantImportJobItem_id :: Lens.Lens' VariantImportJobItem Prelude.Text
variantImportJobItem_id = Lens.lens (\VariantImportJobItem' {id} -> id) (\s@VariantImportJobItem' {} a -> s {id = a} :: VariantImportJobItem)

-- | The job\'s service role ARN.
variantImportJobItem_roleArn :: Lens.Lens' VariantImportJobItem Prelude.Text
variantImportJobItem_roleArn = Lens.lens (\VariantImportJobItem' {roleArn} -> roleArn) (\s@VariantImportJobItem' {} a -> s {roleArn = a} :: VariantImportJobItem)

-- | The job\'s status.
variantImportJobItem_status :: Lens.Lens' VariantImportJobItem JobStatus
variantImportJobItem_status = Lens.lens (\VariantImportJobItem' {status} -> status) (\s@VariantImportJobItem' {} a -> s {status = a} :: VariantImportJobItem)

-- | When the job was updated.
variantImportJobItem_updateTime :: Lens.Lens' VariantImportJobItem Prelude.UTCTime
variantImportJobItem_updateTime = Lens.lens (\VariantImportJobItem' {updateTime} -> updateTime) (\s@VariantImportJobItem' {} a -> s {updateTime = a} :: VariantImportJobItem) Prelude.. Data._Time

instance Data.FromJSON VariantImportJobItem where
  parseJSON =
    Data.withObject
      "VariantImportJobItem"
      ( \x ->
          VariantImportJobItem'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..:? "runLeftNormalization")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "destinationName")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable VariantImportJobItem where
  hashWithSalt _salt VariantImportJobItem' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` runLeftNormalization
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData VariantImportJobItem where
  rnf VariantImportJobItem' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf runLeftNormalization
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
