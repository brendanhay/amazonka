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
-- Module      : Amazonka.QuickSight.Types.Ingestion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Ingestion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ErrorInfo
import Amazonka.QuickSight.Types.IngestionRequestSource
import Amazonka.QuickSight.Types.IngestionRequestType
import Amazonka.QuickSight.Types.IngestionStatus
import Amazonka.QuickSight.Types.QueueInfo
import Amazonka.QuickSight.Types.RowInfo

-- | Information about the SPICE ingestion for a dataset.
--
-- /See:/ 'newIngestion' smart constructor.
data Ingestion = Ingestion'
  { -- | Type of this ingestion.
    requestType :: Prelude.Maybe IngestionRequestType,
    -- | The size of the data ingested, in bytes.
    ingestionSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The time that this ingestion took, measured in seconds.
    ingestionTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | Event source for this ingestion.
    requestSource :: Prelude.Maybe IngestionRequestSource,
    queueInfo :: Prelude.Maybe QueueInfo,
    rowInfo :: Prelude.Maybe RowInfo,
    -- | Ingestion ID.
    ingestionId :: Prelude.Maybe Prelude.Text,
    -- | Error information for this ingestion.
    errorInfo :: Prelude.Maybe ErrorInfo,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Text,
    -- | Ingestion status.
    ingestionStatus :: IngestionStatus,
    -- | The time that this ingestion started.
    createdTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ingestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestType', 'ingestion_requestType' - Type of this ingestion.
--
-- 'ingestionSizeInBytes', 'ingestion_ingestionSizeInBytes' - The size of the data ingested, in bytes.
--
-- 'ingestionTimeInSeconds', 'ingestion_ingestionTimeInSeconds' - The time that this ingestion took, measured in seconds.
--
-- 'requestSource', 'ingestion_requestSource' - Event source for this ingestion.
--
-- 'queueInfo', 'ingestion_queueInfo' - Undocumented member.
--
-- 'rowInfo', 'ingestion_rowInfo' - Undocumented member.
--
-- 'ingestionId', 'ingestion_ingestionId' - Ingestion ID.
--
-- 'errorInfo', 'ingestion_errorInfo' - Error information for this ingestion.
--
-- 'arn', 'ingestion_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'ingestionStatus', 'ingestion_ingestionStatus' - Ingestion status.
--
-- 'createdTime', 'ingestion_createdTime' - The time that this ingestion started.
newIngestion ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'ingestionStatus'
  IngestionStatus ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  Ingestion
newIngestion pArn_ pIngestionStatus_ pCreatedTime_ =
  Ingestion'
    { requestType = Prelude.Nothing,
      ingestionSizeInBytes = Prelude.Nothing,
      ingestionTimeInSeconds = Prelude.Nothing,
      requestSource = Prelude.Nothing,
      queueInfo = Prelude.Nothing,
      rowInfo = Prelude.Nothing,
      ingestionId = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      arn = pArn_,
      ingestionStatus = pIngestionStatus_,
      createdTime = Core._Time Lens.# pCreatedTime_
    }

-- | Type of this ingestion.
ingestion_requestType :: Lens.Lens' Ingestion (Prelude.Maybe IngestionRequestType)
ingestion_requestType = Lens.lens (\Ingestion' {requestType} -> requestType) (\s@Ingestion' {} a -> s {requestType = a} :: Ingestion)

-- | The size of the data ingested, in bytes.
ingestion_ingestionSizeInBytes :: Lens.Lens' Ingestion (Prelude.Maybe Prelude.Integer)
ingestion_ingestionSizeInBytes = Lens.lens (\Ingestion' {ingestionSizeInBytes} -> ingestionSizeInBytes) (\s@Ingestion' {} a -> s {ingestionSizeInBytes = a} :: Ingestion)

-- | The time that this ingestion took, measured in seconds.
ingestion_ingestionTimeInSeconds :: Lens.Lens' Ingestion (Prelude.Maybe Prelude.Integer)
ingestion_ingestionTimeInSeconds = Lens.lens (\Ingestion' {ingestionTimeInSeconds} -> ingestionTimeInSeconds) (\s@Ingestion' {} a -> s {ingestionTimeInSeconds = a} :: Ingestion)

-- | Event source for this ingestion.
ingestion_requestSource :: Lens.Lens' Ingestion (Prelude.Maybe IngestionRequestSource)
ingestion_requestSource = Lens.lens (\Ingestion' {requestSource} -> requestSource) (\s@Ingestion' {} a -> s {requestSource = a} :: Ingestion)

-- | Undocumented member.
ingestion_queueInfo :: Lens.Lens' Ingestion (Prelude.Maybe QueueInfo)
ingestion_queueInfo = Lens.lens (\Ingestion' {queueInfo} -> queueInfo) (\s@Ingestion' {} a -> s {queueInfo = a} :: Ingestion)

-- | Undocumented member.
ingestion_rowInfo :: Lens.Lens' Ingestion (Prelude.Maybe RowInfo)
ingestion_rowInfo = Lens.lens (\Ingestion' {rowInfo} -> rowInfo) (\s@Ingestion' {} a -> s {rowInfo = a} :: Ingestion)

-- | Ingestion ID.
ingestion_ingestionId :: Lens.Lens' Ingestion (Prelude.Maybe Prelude.Text)
ingestion_ingestionId = Lens.lens (\Ingestion' {ingestionId} -> ingestionId) (\s@Ingestion' {} a -> s {ingestionId = a} :: Ingestion)

-- | Error information for this ingestion.
ingestion_errorInfo :: Lens.Lens' Ingestion (Prelude.Maybe ErrorInfo)
ingestion_errorInfo = Lens.lens (\Ingestion' {errorInfo} -> errorInfo) (\s@Ingestion' {} a -> s {errorInfo = a} :: Ingestion)

-- | The Amazon Resource Name (ARN) of the resource.
ingestion_arn :: Lens.Lens' Ingestion Prelude.Text
ingestion_arn = Lens.lens (\Ingestion' {arn} -> arn) (\s@Ingestion' {} a -> s {arn = a} :: Ingestion)

-- | Ingestion status.
ingestion_ingestionStatus :: Lens.Lens' Ingestion IngestionStatus
ingestion_ingestionStatus = Lens.lens (\Ingestion' {ingestionStatus} -> ingestionStatus) (\s@Ingestion' {} a -> s {ingestionStatus = a} :: Ingestion)

-- | The time that this ingestion started.
ingestion_createdTime :: Lens.Lens' Ingestion Prelude.UTCTime
ingestion_createdTime = Lens.lens (\Ingestion' {createdTime} -> createdTime) (\s@Ingestion' {} a -> s {createdTime = a} :: Ingestion) Prelude.. Core._Time

instance Core.FromJSON Ingestion where
  parseJSON =
    Core.withObject
      "Ingestion"
      ( \x ->
          Ingestion'
            Prelude.<$> (x Core..:? "RequestType")
            Prelude.<*> (x Core..:? "IngestionSizeInBytes")
            Prelude.<*> (x Core..:? "IngestionTimeInSeconds")
            Prelude.<*> (x Core..:? "RequestSource")
            Prelude.<*> (x Core..:? "QueueInfo")
            Prelude.<*> (x Core..:? "RowInfo")
            Prelude.<*> (x Core..:? "IngestionId")
            Prelude.<*> (x Core..:? "ErrorInfo")
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "IngestionStatus")
            Prelude.<*> (x Core..: "CreatedTime")
      )

instance Prelude.Hashable Ingestion where
  hashWithSalt _salt Ingestion' {..} =
    _salt `Prelude.hashWithSalt` requestType
      `Prelude.hashWithSalt` ingestionSizeInBytes
      `Prelude.hashWithSalt` ingestionTimeInSeconds
      `Prelude.hashWithSalt` requestSource
      `Prelude.hashWithSalt` queueInfo
      `Prelude.hashWithSalt` rowInfo
      `Prelude.hashWithSalt` ingestionId
      `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` ingestionStatus
      `Prelude.hashWithSalt` createdTime

instance Prelude.NFData Ingestion where
  rnf Ingestion' {..} =
    Prelude.rnf requestType
      `Prelude.seq` Prelude.rnf ingestionSizeInBytes
      `Prelude.seq` Prelude.rnf ingestionTimeInSeconds
      `Prelude.seq` Prelude.rnf requestSource
      `Prelude.seq` Prelude.rnf queueInfo
      `Prelude.seq` Prelude.rnf rowInfo
      `Prelude.seq` Prelude.rnf ingestionId
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf ingestionStatus
      `Prelude.seq` Prelude.rnf createdTime
