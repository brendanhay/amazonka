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
-- Module      : Amazonka.Omics.Types.ExportReadSetJobDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ExportReadSetJobDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetExportJobStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about a read set export job.
--
-- /See:/ 'newExportReadSetJobDetail' smart constructor.
data ExportReadSetJobDetail = ExportReadSetJobDetail'
  { -- | When the job completed.
    completionTime :: Prelude.Maybe Data.ISO8601,
    -- | When the job was created.
    creationTime :: Data.ISO8601,
    -- | The job\'s destination in Amazon S3.
    destination :: Prelude.Text,
    -- | The job\'s ID.
    id :: Prelude.Text,
    -- | The job\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The job\'s status.
    status :: ReadSetExportJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportReadSetJobDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'exportReadSetJobDetail_completionTime' - When the job completed.
--
-- 'creationTime', 'exportReadSetJobDetail_creationTime' - When the job was created.
--
-- 'destination', 'exportReadSetJobDetail_destination' - The job\'s destination in Amazon S3.
--
-- 'id', 'exportReadSetJobDetail_id' - The job\'s ID.
--
-- 'sequenceStoreId', 'exportReadSetJobDetail_sequenceStoreId' - The job\'s sequence store ID.
--
-- 'status', 'exportReadSetJobDetail_status' - The job\'s status.
newExportReadSetJobDetail ::
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'destination'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'status'
  ReadSetExportJobStatus ->
  ExportReadSetJobDetail
newExportReadSetJobDetail
  pCreationTime_
  pDestination_
  pId_
  pSequenceStoreId_
  pStatus_ =
    ExportReadSetJobDetail'
      { completionTime =
          Prelude.Nothing,
        creationTime = Data._Time Lens.# pCreationTime_,
        destination = pDestination_,
        id = pId_,
        sequenceStoreId = pSequenceStoreId_,
        status = pStatus_
      }

-- | When the job completed.
exportReadSetJobDetail_completionTime :: Lens.Lens' ExportReadSetJobDetail (Prelude.Maybe Prelude.UTCTime)
exportReadSetJobDetail_completionTime = Lens.lens (\ExportReadSetJobDetail' {completionTime} -> completionTime) (\s@ExportReadSetJobDetail' {} a -> s {completionTime = a} :: ExportReadSetJobDetail) Prelude.. Lens.mapping Data._Time

-- | When the job was created.
exportReadSetJobDetail_creationTime :: Lens.Lens' ExportReadSetJobDetail Prelude.UTCTime
exportReadSetJobDetail_creationTime = Lens.lens (\ExportReadSetJobDetail' {creationTime} -> creationTime) (\s@ExportReadSetJobDetail' {} a -> s {creationTime = a} :: ExportReadSetJobDetail) Prelude.. Data._Time

-- | The job\'s destination in Amazon S3.
exportReadSetJobDetail_destination :: Lens.Lens' ExportReadSetJobDetail Prelude.Text
exportReadSetJobDetail_destination = Lens.lens (\ExportReadSetJobDetail' {destination} -> destination) (\s@ExportReadSetJobDetail' {} a -> s {destination = a} :: ExportReadSetJobDetail)

-- | The job\'s ID.
exportReadSetJobDetail_id :: Lens.Lens' ExportReadSetJobDetail Prelude.Text
exportReadSetJobDetail_id = Lens.lens (\ExportReadSetJobDetail' {id} -> id) (\s@ExportReadSetJobDetail' {} a -> s {id = a} :: ExportReadSetJobDetail)

-- | The job\'s sequence store ID.
exportReadSetJobDetail_sequenceStoreId :: Lens.Lens' ExportReadSetJobDetail Prelude.Text
exportReadSetJobDetail_sequenceStoreId = Lens.lens (\ExportReadSetJobDetail' {sequenceStoreId} -> sequenceStoreId) (\s@ExportReadSetJobDetail' {} a -> s {sequenceStoreId = a} :: ExportReadSetJobDetail)

-- | The job\'s status.
exportReadSetJobDetail_status :: Lens.Lens' ExportReadSetJobDetail ReadSetExportJobStatus
exportReadSetJobDetail_status = Lens.lens (\ExportReadSetJobDetail' {status} -> status) (\s@ExportReadSetJobDetail' {} a -> s {status = a} :: ExportReadSetJobDetail)

instance Data.FromJSON ExportReadSetJobDetail where
  parseJSON =
    Data.withObject
      "ExportReadSetJobDetail"
      ( \x ->
          ExportReadSetJobDetail'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "destination")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "sequenceStoreId")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ExportReadSetJobDetail where
  hashWithSalt _salt ExportReadSetJobDetail' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ExportReadSetJobDetail where
  rnf ExportReadSetJobDetail' {..} =
    Prelude.rnf completionTime `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf destination `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf sequenceStoreId `Prelude.seq`
              Prelude.rnf status
