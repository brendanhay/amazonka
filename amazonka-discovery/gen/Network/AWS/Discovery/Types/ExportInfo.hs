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
-- Module      : Network.AWS.Discovery.Types.ExportInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types.ExportStatus
import qualified Network.AWS.Lens as Lens

-- | Information regarding the export status of discovered data. The value is
-- an array of objects.
--
-- /See:/ 'newExportInfo' smart constructor.
data ExportInfo = ExportInfo'
  { -- | If true, the export of agent information exceeded the size limit for a
    -- single export and the exported data is incomplete for the requested time
    -- range. To address this, select a smaller time range for the export by
    -- using @startDate@ and @endDate@.
    isTruncated :: Core.Maybe Core.Bool,
    -- | A URL for an Amazon S3 bucket where you can review the exported data.
    -- The URL is displayed only if the export succeeded.
    configurationsDownloadUrl :: Core.Maybe Core.Text,
    -- | The value of @startTime@ parameter in the @StartExportTask@ request. If
    -- no @startTime@ was requested, this result does not appear in
    -- @ExportInfo@.
    requestedStartTime :: Core.Maybe Core.POSIX,
    -- | The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was
    -- requested, this result does not appear in @ExportInfo@.
    requestedEndTime :: Core.Maybe Core.POSIX,
    -- | A unique identifier used to query an export.
    exportId :: Core.Text,
    -- | The status of the data export job.
    exportStatus :: ExportStatus,
    -- | A status message provided for API callers.
    statusMessage :: Core.Text,
    -- | The time that the data export was initiated.
    exportRequestTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'exportInfo_isTruncated' - If true, the export of agent information exceeded the size limit for a
-- single export and the exported data is incomplete for the requested time
-- range. To address this, select a smaller time range for the export by
-- using @startDate@ and @endDate@.
--
-- 'configurationsDownloadUrl', 'exportInfo_configurationsDownloadUrl' - A URL for an Amazon S3 bucket where you can review the exported data.
-- The URL is displayed only if the export succeeded.
--
-- 'requestedStartTime', 'exportInfo_requestedStartTime' - The value of @startTime@ parameter in the @StartExportTask@ request. If
-- no @startTime@ was requested, this result does not appear in
-- @ExportInfo@.
--
-- 'requestedEndTime', 'exportInfo_requestedEndTime' - The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was
-- requested, this result does not appear in @ExportInfo@.
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
  Core.Text ->
  -- | 'exportStatus'
  ExportStatus ->
  -- | 'statusMessage'
  Core.Text ->
  -- | 'exportRequestTime'
  Core.UTCTime ->
  ExportInfo
newExportInfo
  pExportId_
  pExportStatus_
  pStatusMessage_
  pExportRequestTime_ =
    ExportInfo'
      { isTruncated = Core.Nothing,
        configurationsDownloadUrl = Core.Nothing,
        requestedStartTime = Core.Nothing,
        requestedEndTime = Core.Nothing,
        exportId = pExportId_,
        exportStatus = pExportStatus_,
        statusMessage = pStatusMessage_,
        exportRequestTime =
          Core._Time Lens.# pExportRequestTime_
      }

-- | If true, the export of agent information exceeded the size limit for a
-- single export and the exported data is incomplete for the requested time
-- range. To address this, select a smaller time range for the export by
-- using @startDate@ and @endDate@.
exportInfo_isTruncated :: Lens.Lens' ExportInfo (Core.Maybe Core.Bool)
exportInfo_isTruncated = Lens.lens (\ExportInfo' {isTruncated} -> isTruncated) (\s@ExportInfo' {} a -> s {isTruncated = a} :: ExportInfo)

-- | A URL for an Amazon S3 bucket where you can review the exported data.
-- The URL is displayed only if the export succeeded.
exportInfo_configurationsDownloadUrl :: Lens.Lens' ExportInfo (Core.Maybe Core.Text)
exportInfo_configurationsDownloadUrl = Lens.lens (\ExportInfo' {configurationsDownloadUrl} -> configurationsDownloadUrl) (\s@ExportInfo' {} a -> s {configurationsDownloadUrl = a} :: ExportInfo)

-- | The value of @startTime@ parameter in the @StartExportTask@ request. If
-- no @startTime@ was requested, this result does not appear in
-- @ExportInfo@.
exportInfo_requestedStartTime :: Lens.Lens' ExportInfo (Core.Maybe Core.UTCTime)
exportInfo_requestedStartTime = Lens.lens (\ExportInfo' {requestedStartTime} -> requestedStartTime) (\s@ExportInfo' {} a -> s {requestedStartTime = a} :: ExportInfo) Core.. Lens.mapping Core._Time

-- | The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was
-- requested, this result does not appear in @ExportInfo@.
exportInfo_requestedEndTime :: Lens.Lens' ExportInfo (Core.Maybe Core.UTCTime)
exportInfo_requestedEndTime = Lens.lens (\ExportInfo' {requestedEndTime} -> requestedEndTime) (\s@ExportInfo' {} a -> s {requestedEndTime = a} :: ExportInfo) Core.. Lens.mapping Core._Time

-- | A unique identifier used to query an export.
exportInfo_exportId :: Lens.Lens' ExportInfo Core.Text
exportInfo_exportId = Lens.lens (\ExportInfo' {exportId} -> exportId) (\s@ExportInfo' {} a -> s {exportId = a} :: ExportInfo)

-- | The status of the data export job.
exportInfo_exportStatus :: Lens.Lens' ExportInfo ExportStatus
exportInfo_exportStatus = Lens.lens (\ExportInfo' {exportStatus} -> exportStatus) (\s@ExportInfo' {} a -> s {exportStatus = a} :: ExportInfo)

-- | A status message provided for API callers.
exportInfo_statusMessage :: Lens.Lens' ExportInfo Core.Text
exportInfo_statusMessage = Lens.lens (\ExportInfo' {statusMessage} -> statusMessage) (\s@ExportInfo' {} a -> s {statusMessage = a} :: ExportInfo)

-- | The time that the data export was initiated.
exportInfo_exportRequestTime :: Lens.Lens' ExportInfo Core.UTCTime
exportInfo_exportRequestTime = Lens.lens (\ExportInfo' {exportRequestTime} -> exportRequestTime) (\s@ExportInfo' {} a -> s {exportRequestTime = a} :: ExportInfo) Core.. Core._Time

instance Core.FromJSON ExportInfo where
  parseJSON =
    Core.withObject
      "ExportInfo"
      ( \x ->
          ExportInfo'
            Core.<$> (x Core..:? "isTruncated")
            Core.<*> (x Core..:? "configurationsDownloadUrl")
            Core.<*> (x Core..:? "requestedStartTime")
            Core.<*> (x Core..:? "requestedEndTime")
            Core.<*> (x Core..: "exportId")
            Core.<*> (x Core..: "exportStatus")
            Core.<*> (x Core..: "statusMessage")
            Core.<*> (x Core..: "exportRequestTime")
      )

instance Core.Hashable ExportInfo

instance Core.NFData ExportInfo
