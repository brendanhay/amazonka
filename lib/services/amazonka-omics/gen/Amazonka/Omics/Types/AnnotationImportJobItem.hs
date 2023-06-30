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
-- Module      : Amazonka.Omics.Types.AnnotationImportJobItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.AnnotationImportJobItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | An annotation import job.
--
-- /See:/ 'newAnnotationImportJobItem' smart constructor.
data AnnotationImportJobItem = AnnotationImportJobItem'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | The job\'s left normalization setting.
    runLeftNormalization :: Prelude.Maybe Prelude.Bool,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s destination annotation store.
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
-- Create a value of 'AnnotationImportJobItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'annotationImportJobItem_completionTime' - When the job completed.
--
-- 'runLeftNormalization', 'annotationImportJobItem_runLeftNormalization' - The job\'s left normalization setting.
--
-- 'creationTime', 'annotationImportJobItem_creationTime' - When the job was created.
--
-- 'destinationName', 'annotationImportJobItem_destinationName' - The job\'s destination annotation store.
--
-- 'id', 'annotationImportJobItem_id' - The job\'s ID.
--
-- 'roleArn', 'annotationImportJobItem_roleArn' - The job\'s service role ARN.
--
-- 'status', 'annotationImportJobItem_status' - The job\'s status.
--
-- 'updateTime', 'annotationImportJobItem_updateTime' - When the job was updated.
newAnnotationImportJobItem ::
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
  AnnotationImportJobItem
newAnnotationImportJobItem
  pCreationTime_
  pDestinationName_
  pId_
  pRoleArn_
  pStatus_
  pUpdateTime_ =
    AnnotationImportJobItem'
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
annotationImportJobItem_completionTime :: Lens.Lens' AnnotationImportJobItem (Prelude.Maybe Prelude.UTCTime)
annotationImportJobItem_completionTime = Lens.lens (\AnnotationImportJobItem' {completionTime} -> completionTime) (\s@AnnotationImportJobItem' {} a -> s {completionTime = a} :: AnnotationImportJobItem) Prelude.. Lens.mapping Data._Time

-- | The job\'s left normalization setting.
annotationImportJobItem_runLeftNormalization :: Lens.Lens' AnnotationImportJobItem (Prelude.Maybe Prelude.Bool)
annotationImportJobItem_runLeftNormalization = Lens.lens (\AnnotationImportJobItem' {runLeftNormalization} -> runLeftNormalization) (\s@AnnotationImportJobItem' {} a -> s {runLeftNormalization = a} :: AnnotationImportJobItem)

-- | When the job was created.
annotationImportJobItem_creationTime :: Lens.Lens' AnnotationImportJobItem Prelude.UTCTime
annotationImportJobItem_creationTime = Lens.lens (\AnnotationImportJobItem' {creationTime} -> creationTime) (\s@AnnotationImportJobItem' {} a -> s {creationTime = a} :: AnnotationImportJobItem) Prelude.. Data._Time

-- | The job\'s destination annotation store.
annotationImportJobItem_destinationName :: Lens.Lens' AnnotationImportJobItem Prelude.Text
annotationImportJobItem_destinationName = Lens.lens (\AnnotationImportJobItem' {destinationName} -> destinationName) (\s@AnnotationImportJobItem' {} a -> s {destinationName = a} :: AnnotationImportJobItem)

-- | The job\'s ID.
annotationImportJobItem_id :: Lens.Lens' AnnotationImportJobItem Prelude.Text
annotationImportJobItem_id = Lens.lens (\AnnotationImportJobItem' {id} -> id) (\s@AnnotationImportJobItem' {} a -> s {id = a} :: AnnotationImportJobItem)

-- | The job\'s service role ARN.
annotationImportJobItem_roleArn :: Lens.Lens' AnnotationImportJobItem Prelude.Text
annotationImportJobItem_roleArn = Lens.lens (\AnnotationImportJobItem' {roleArn} -> roleArn) (\s@AnnotationImportJobItem' {} a -> s {roleArn = a} :: AnnotationImportJobItem)

-- | The job\'s status.
annotationImportJobItem_status :: Lens.Lens' AnnotationImportJobItem JobStatus
annotationImportJobItem_status = Lens.lens (\AnnotationImportJobItem' {status} -> status) (\s@AnnotationImportJobItem' {} a -> s {status = a} :: AnnotationImportJobItem)

-- | When the job was updated.
annotationImportJobItem_updateTime :: Lens.Lens' AnnotationImportJobItem Prelude.UTCTime
annotationImportJobItem_updateTime = Lens.lens (\AnnotationImportJobItem' {updateTime} -> updateTime) (\s@AnnotationImportJobItem' {} a -> s {updateTime = a} :: AnnotationImportJobItem) Prelude.. Data._Time

instance Data.FromJSON AnnotationImportJobItem where
  parseJSON =
    Data.withObject
      "AnnotationImportJobItem"
      ( \x ->
          AnnotationImportJobItem'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..:? "runLeftNormalization")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "destinationName")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable AnnotationImportJobItem where
  hashWithSalt _salt AnnotationImportJobItem' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` runLeftNormalization
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData AnnotationImportJobItem where
  rnf AnnotationImportJobItem' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf runLeftNormalization
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
