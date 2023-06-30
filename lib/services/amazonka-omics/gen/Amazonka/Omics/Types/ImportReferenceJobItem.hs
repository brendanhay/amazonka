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
-- Module      : Amazonka.Omics.Types.ImportReferenceJobItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ImportReferenceJobItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReferenceImportJobStatus
import qualified Amazonka.Prelude as Prelude

-- | An import reference job.
--
-- /See:/ 'newImportReferenceJobItem' smart constructor.
data ImportReferenceJobItem = ImportReferenceJobItem'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s reference store ID.
    referenceStoreId :: Prelude.Text,
    -- | The job\'s service role ARN.
    roleArn :: Prelude.Text,
    -- | The job\'s status.
    status :: ReferenceImportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportReferenceJobItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'importReferenceJobItem_completionTime' - When the job completed.
--
-- 'creationTime', 'importReferenceJobItem_creationTime' - When the job was created.
--
-- 'id', 'importReferenceJobItem_id' - The job\'s ID.
--
-- 'referenceStoreId', 'importReferenceJobItem_referenceStoreId' - The job\'s reference store ID.
--
-- 'roleArn', 'importReferenceJobItem_roleArn' - The job\'s service role ARN.
--
-- 'status', 'importReferenceJobItem_status' - The job\'s status.
newImportReferenceJobItem ::
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'referenceStoreId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  ReferenceImportJobStatus ->
  ImportReferenceJobItem
newImportReferenceJobItem
  pCreationTime_
  pId_
  pReferenceStoreId_
  pRoleArn_
  pStatus_ =
    ImportReferenceJobItem'
      { completionTime =
          Prelude.Nothing,
        creationTime = Data._Time Lens.# pCreationTime_,
        id = pId_,
        referenceStoreId = pReferenceStoreId_,
        roleArn = pRoleArn_,
        status = pStatus_
      }

-- | When the job completed.
importReferenceJobItem_completionTime :: Lens.Lens' ImportReferenceJobItem (Prelude.Maybe Prelude.UTCTime)
importReferenceJobItem_completionTime = Lens.lens (\ImportReferenceJobItem' {completionTime} -> completionTime) (\s@ImportReferenceJobItem' {} a -> s {completionTime = a} :: ImportReferenceJobItem) Prelude.. Lens.mapping Data._Time

-- | When the job was created.
importReferenceJobItem_creationTime :: Lens.Lens' ImportReferenceJobItem Prelude.UTCTime
importReferenceJobItem_creationTime = Lens.lens (\ImportReferenceJobItem' {creationTime} -> creationTime) (\s@ImportReferenceJobItem' {} a -> s {creationTime = a} :: ImportReferenceJobItem) Prelude.. Data._Time

-- | The job\'s ID.
importReferenceJobItem_id :: Lens.Lens' ImportReferenceJobItem Prelude.Text
importReferenceJobItem_id = Lens.lens (\ImportReferenceJobItem' {id} -> id) (\s@ImportReferenceJobItem' {} a -> s {id = a} :: ImportReferenceJobItem)

-- | The job\'s reference store ID.
importReferenceJobItem_referenceStoreId :: Lens.Lens' ImportReferenceJobItem Prelude.Text
importReferenceJobItem_referenceStoreId = Lens.lens (\ImportReferenceJobItem' {referenceStoreId} -> referenceStoreId) (\s@ImportReferenceJobItem' {} a -> s {referenceStoreId = a} :: ImportReferenceJobItem)

-- | The job\'s service role ARN.
importReferenceJobItem_roleArn :: Lens.Lens' ImportReferenceJobItem Prelude.Text
importReferenceJobItem_roleArn = Lens.lens (\ImportReferenceJobItem' {roleArn} -> roleArn) (\s@ImportReferenceJobItem' {} a -> s {roleArn = a} :: ImportReferenceJobItem)

-- | The job\'s status.
importReferenceJobItem_status :: Lens.Lens' ImportReferenceJobItem ReferenceImportJobStatus
importReferenceJobItem_status = Lens.lens (\ImportReferenceJobItem' {status} -> status) (\s@ImportReferenceJobItem' {} a -> s {status = a} :: ImportReferenceJobItem)

instance Data.FromJSON ImportReferenceJobItem where
  parseJSON =
    Data.withObject
      "ImportReferenceJobItem"
      ( \x ->
          ImportReferenceJobItem'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "referenceStoreId")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ImportReferenceJobItem where
  hashWithSalt _salt ImportReferenceJobItem' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` referenceStoreId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImportReferenceJobItem where
  rnf ImportReferenceJobItem' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf referenceStoreId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
