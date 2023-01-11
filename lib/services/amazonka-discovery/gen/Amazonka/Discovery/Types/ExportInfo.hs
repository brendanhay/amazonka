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
-- Module      : Amazonka.Discovery.Types.ExportInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ExportInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types.ExportStatus
import qualified Amazonka.Prelude as Prelude

-- | Information regarding the export status of discovered data. The value is
-- an array of objects.
--
-- /See:/ 'newExportInfo' smart constructor.
data ExportInfo = ExportInfo'
  { -- | A URL for an Amazon S3 bucket where you can review the exported data.
    -- The URL is displayed only if the export succeeded.
    configurationsDownloadUrl :: Prelude.Maybe Prelude.Text,
    -- | If true, the export of agent information exceeded the size limit for a
    -- single export and the exported data is incomplete for the requested time
    -- range. To address this, select a smaller time range for the export by
    -- using @startDate@ and @endDate@.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was
    -- requested, this result does not appear in @ExportInfo@.
    requestedEndTime :: Prelude.Maybe Data.POSIX,
    -- | The value of @startTime@ parameter in the @StartExportTask@ request. If
    -- no @startTime@ was requested, this result does not appear in
    -- @ExportInfo@.
    requestedStartTime :: Prelude.Maybe Data.POSIX,
    -- | A unique identifier used to query an export.
    exportId :: Prelude.Text,
    -- | The status of the data export job.
    exportStatus :: ExportStatus,
    -- | A status message provided for API callers.
    statusMessage :: Prelude.Text,
    -- | The time that the data export was initiated.
    exportRequestTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationsDownloadUrl', 'exportInfo_configurationsDownloadUrl' - A URL for an Amazon S3 bucket where you can review the exported data.
-- The URL is displayed only if the export succeeded.
--
-- 'isTruncated', 'exportInfo_isTruncated' - If true, the export of agent information exceeded the size limit for a
-- single export and the exported data is incomplete for the requested time
-- range. To address this, select a smaller time range for the export by
-- using @startDate@ and @endDate@.
--
-- 'requestedEndTime', 'exportInfo_requestedEndTime' - The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was
-- requested, this result does not appear in @ExportInfo@.
--
-- 'requestedStartTime', 'exportInfo_requestedStartTime' - The value of @startTime@ parameter in the @StartExportTask@ request. If
-- no @startTime@ was requested, this result does not appear in
-- @ExportInfo@.
--
-- 'exportId', 'exportInfo_exportId' - A unique identifier used to query an export.
--
-- 'exportStatus', 'exportInfo_exportStatus' - The status of the data export job.
--
-- 'statusMessage', 'exportInfo_statusMessage' - A status message provided for API callers.
--
-- 'exportRequestTime', 'exportInfo_exportRequestTime' - The time that the data export was initiated.
newExportInfo ::
  -- | 'exportId'
  Prelude.Text ->
  -- | 'exportStatus'
  ExportStatus ->
  -- | 'statusMessage'
  Prelude.Text ->
  -- | 'exportRequestTime'
  Prelude.UTCTime ->
  ExportInfo
newExportInfo
  pExportId_
  pExportStatus_
  pStatusMessage_
  pExportRequestTime_ =
    ExportInfo'
      { configurationsDownloadUrl =
          Prelude.Nothing,
        isTruncated = Prelude.Nothing,
        requestedEndTime = Prelude.Nothing,
        requestedStartTime = Prelude.Nothing,
        exportId = pExportId_,
        exportStatus = pExportStatus_,
        statusMessage = pStatusMessage_,
        exportRequestTime =
          Data._Time Lens.# pExportRequestTime_
      }

-- | A URL for an Amazon S3 bucket where you can review the exported data.
-- The URL is displayed only if the export succeeded.
exportInfo_configurationsDownloadUrl :: Lens.Lens' ExportInfo (Prelude.Maybe Prelude.Text)
exportInfo_configurationsDownloadUrl = Lens.lens (\ExportInfo' {configurationsDownloadUrl} -> configurationsDownloadUrl) (\s@ExportInfo' {} a -> s {configurationsDownloadUrl = a} :: ExportInfo)

-- | If true, the export of agent information exceeded the size limit for a
-- single export and the exported data is incomplete for the requested time
-- range. To address this, select a smaller time range for the export by
-- using @startDate@ and @endDate@.
exportInfo_isTruncated :: Lens.Lens' ExportInfo (Prelude.Maybe Prelude.Bool)
exportInfo_isTruncated = Lens.lens (\ExportInfo' {isTruncated} -> isTruncated) (\s@ExportInfo' {} a -> s {isTruncated = a} :: ExportInfo)

-- | The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was
-- requested, this result does not appear in @ExportInfo@.
exportInfo_requestedEndTime :: Lens.Lens' ExportInfo (Prelude.Maybe Prelude.UTCTime)
exportInfo_requestedEndTime = Lens.lens (\ExportInfo' {requestedEndTime} -> requestedEndTime) (\s@ExportInfo' {} a -> s {requestedEndTime = a} :: ExportInfo) Prelude.. Lens.mapping Data._Time

-- | The value of @startTime@ parameter in the @StartExportTask@ request. If
-- no @startTime@ was requested, this result does not appear in
-- @ExportInfo@.
exportInfo_requestedStartTime :: Lens.Lens' ExportInfo (Prelude.Maybe Prelude.UTCTime)
exportInfo_requestedStartTime = Lens.lens (\ExportInfo' {requestedStartTime} -> requestedStartTime) (\s@ExportInfo' {} a -> s {requestedStartTime = a} :: ExportInfo) Prelude.. Lens.mapping Data._Time

-- | A unique identifier used to query an export.
exportInfo_exportId :: Lens.Lens' ExportInfo Prelude.Text
exportInfo_exportId = Lens.lens (\ExportInfo' {exportId} -> exportId) (\s@ExportInfo' {} a -> s {exportId = a} :: ExportInfo)

-- | The status of the data export job.
exportInfo_exportStatus :: Lens.Lens' ExportInfo ExportStatus
exportInfo_exportStatus = Lens.lens (\ExportInfo' {exportStatus} -> exportStatus) (\s@ExportInfo' {} a -> s {exportStatus = a} :: ExportInfo)

-- | A status message provided for API callers.
exportInfo_statusMessage :: Lens.Lens' ExportInfo Prelude.Text
exportInfo_statusMessage = Lens.lens (\ExportInfo' {statusMessage} -> statusMessage) (\s@ExportInfo' {} a -> s {statusMessage = a} :: ExportInfo)

-- | The time that the data export was initiated.
exportInfo_exportRequestTime :: Lens.Lens' ExportInfo Prelude.UTCTime
exportInfo_exportRequestTime = Lens.lens (\ExportInfo' {exportRequestTime} -> exportRequestTime) (\s@ExportInfo' {} a -> s {exportRequestTime = a} :: ExportInfo) Prelude.. Data._Time

instance Data.FromJSON ExportInfo where
  parseJSON =
    Data.withObject
      "ExportInfo"
      ( \x ->
          ExportInfo'
            Prelude.<$> (x Data..:? "configurationsDownloadUrl")
            Prelude.<*> (x Data..:? "isTruncated")
            Prelude.<*> (x Data..:? "requestedEndTime")
            Prelude.<*> (x Data..:? "requestedStartTime")
            Prelude.<*> (x Data..: "exportId")
            Prelude.<*> (x Data..: "exportStatus")
            Prelude.<*> (x Data..: "statusMessage")
            Prelude.<*> (x Data..: "exportRequestTime")
      )

instance Prelude.Hashable ExportInfo where
  hashWithSalt _salt ExportInfo' {..} =
    _salt
      `Prelude.hashWithSalt` configurationsDownloadUrl
      `Prelude.hashWithSalt` isTruncated
      `Prelude.hashWithSalt` requestedEndTime
      `Prelude.hashWithSalt` requestedStartTime
      `Prelude.hashWithSalt` exportId
      `Prelude.hashWithSalt` exportStatus
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` exportRequestTime

instance Prelude.NFData ExportInfo where
  rnf ExportInfo' {..} =
    Prelude.rnf configurationsDownloadUrl
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf requestedEndTime
      `Prelude.seq` Prelude.rnf requestedStartTime
      `Prelude.seq` Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf exportRequestTime
