{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexV2Models.DescribeExport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific export.
module Network.AWS.LexV2Models.DescribeExport
  ( -- * Creating a Request
    DescribeExport (..),
    newDescribeExport,

    -- * Request Lenses
    describeExport_exportId,

    -- * Destructuring the Response
    DescribeExportResponse (..),
    newDescribeExportResponse,

    -- * Response Lenses
    describeExportResponse_resourceSpecification,
    describeExportResponse_fileFormat,
    describeExportResponse_exportStatus,
    describeExportResponse_lastUpdatedDateTime,
    describeExportResponse_downloadUrl,
    describeExportResponse_failureReasons,
    describeExportResponse_creationDateTime,
    describeExportResponse_exportId,
    describeExportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExport' smart constructor.
data DescribeExport = DescribeExport'
  { -- | The unique identifier of the export to describe.
    exportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportId', 'describeExport_exportId' - The unique identifier of the export to describe.
newDescribeExport ::
  -- | 'exportId'
  Prelude.Text ->
  DescribeExport
newDescribeExport pExportId_ =
  DescribeExport' {exportId = pExportId_}

-- | The unique identifier of the export to describe.
describeExport_exportId :: Lens.Lens' DescribeExport Prelude.Text
describeExport_exportId = Lens.lens (\DescribeExport' {exportId} -> exportId) (\s@DescribeExport' {} a -> s {exportId = a} :: DescribeExport)

instance Core.AWSRequest DescribeExport where
  type
    AWSResponse DescribeExport =
      DescribeExportResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExportResponse'
            Prelude.<$> (x Core..?> "resourceSpecification")
            Prelude.<*> (x Core..?> "fileFormat")
            Prelude.<*> (x Core..?> "exportStatus")
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (x Core..?> "downloadUrl")
            Prelude.<*> (x Core..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "exportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExport

instance Prelude.NFData DescribeExport

instance Core.ToHeaders DescribeExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeExport where
  toPath DescribeExport' {..} =
    Prelude.mconcat
      ["/exports/", Core.toBS exportId, "/"]

instance Core.ToQuery DescribeExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExportResponse' smart constructor.
data DescribeExportResponse = DescribeExportResponse'
  { -- | The bot, bot ID, and optional locale ID of the exported bot or bot
    -- locale.
    resourceSpecification :: Prelude.Maybe ExportResourceSpecification,
    -- | The file format used in the files that describe the bot or bot locale.
    fileFormat :: Prelude.Maybe ImportExportFileFormat,
    -- | The status of the export. When the status is @Complete@ the export
    -- archive file is available for download.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The last date and time that the export was updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | A pre-signed S3 URL that points to the bot or bot locale archive. The
    -- URL is only available for 5 minutes after calling the @DescribeExport@
    -- operation.
    downloadUrl :: Prelude.Maybe Prelude.Text,
    -- | If the @exportStatus@ is failed, contains one or more reasons why the
    -- export could not be completed.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | The date and time that the export was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of the described export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSpecification', 'describeExportResponse_resourceSpecification' - The bot, bot ID, and optional locale ID of the exported bot or bot
-- locale.
--
-- 'fileFormat', 'describeExportResponse_fileFormat' - The file format used in the files that describe the bot or bot locale.
--
-- 'exportStatus', 'describeExportResponse_exportStatus' - The status of the export. When the status is @Complete@ the export
-- archive file is available for download.
--
-- 'lastUpdatedDateTime', 'describeExportResponse_lastUpdatedDateTime' - The last date and time that the export was updated.
--
-- 'downloadUrl', 'describeExportResponse_downloadUrl' - A pre-signed S3 URL that points to the bot or bot locale archive. The
-- URL is only available for 5 minutes after calling the @DescribeExport@
-- operation.
--
-- 'failureReasons', 'describeExportResponse_failureReasons' - If the @exportStatus@ is failed, contains one or more reasons why the
-- export could not be completed.
--
-- 'creationDateTime', 'describeExportResponse_creationDateTime' - The date and time that the export was created.
--
-- 'exportId', 'describeExportResponse_exportId' - The unique identifier of the described export.
--
-- 'httpStatus', 'describeExportResponse_httpStatus' - The response's http status code.
newDescribeExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExportResponse
newDescribeExportResponse pHttpStatus_ =
  DescribeExportResponse'
    { resourceSpecification =
        Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      downloadUrl = Prelude.Nothing,
      failureReasons = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      exportId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The bot, bot ID, and optional locale ID of the exported bot or bot
-- locale.
describeExportResponse_resourceSpecification :: Lens.Lens' DescribeExportResponse (Prelude.Maybe ExportResourceSpecification)
describeExportResponse_resourceSpecification = Lens.lens (\DescribeExportResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeExportResponse' {} a -> s {resourceSpecification = a} :: DescribeExportResponse)

-- | The file format used in the files that describe the bot or bot locale.
describeExportResponse_fileFormat :: Lens.Lens' DescribeExportResponse (Prelude.Maybe ImportExportFileFormat)
describeExportResponse_fileFormat = Lens.lens (\DescribeExportResponse' {fileFormat} -> fileFormat) (\s@DescribeExportResponse' {} a -> s {fileFormat = a} :: DescribeExportResponse)

-- | The status of the export. When the status is @Complete@ the export
-- archive file is available for download.
describeExportResponse_exportStatus :: Lens.Lens' DescribeExportResponse (Prelude.Maybe ExportStatus)
describeExportResponse_exportStatus = Lens.lens (\DescribeExportResponse' {exportStatus} -> exportStatus) (\s@DescribeExportResponse' {} a -> s {exportStatus = a} :: DescribeExportResponse)

-- | The last date and time that the export was updated.
describeExportResponse_lastUpdatedDateTime :: Lens.Lens' DescribeExportResponse (Prelude.Maybe Prelude.UTCTime)
describeExportResponse_lastUpdatedDateTime = Lens.lens (\DescribeExportResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeExportResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeExportResponse) Prelude.. Lens.mapping Core._Time

-- | A pre-signed S3 URL that points to the bot or bot locale archive. The
-- URL is only available for 5 minutes after calling the @DescribeExport@
-- operation.
describeExportResponse_downloadUrl :: Lens.Lens' DescribeExportResponse (Prelude.Maybe Prelude.Text)
describeExportResponse_downloadUrl = Lens.lens (\DescribeExportResponse' {downloadUrl} -> downloadUrl) (\s@DescribeExportResponse' {} a -> s {downloadUrl = a} :: DescribeExportResponse)

-- | If the @exportStatus@ is failed, contains one or more reasons why the
-- export could not be completed.
describeExportResponse_failureReasons :: Lens.Lens' DescribeExportResponse (Prelude.Maybe [Prelude.Text])
describeExportResponse_failureReasons = Lens.lens (\DescribeExportResponse' {failureReasons} -> failureReasons) (\s@DescribeExportResponse' {} a -> s {failureReasons = a} :: DescribeExportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the export was created.
describeExportResponse_creationDateTime :: Lens.Lens' DescribeExportResponse (Prelude.Maybe Prelude.UTCTime)
describeExportResponse_creationDateTime = Lens.lens (\DescribeExportResponse' {creationDateTime} -> creationDateTime) (\s@DescribeExportResponse' {} a -> s {creationDateTime = a} :: DescribeExportResponse) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of the described export.
describeExportResponse_exportId :: Lens.Lens' DescribeExportResponse (Prelude.Maybe Prelude.Text)
describeExportResponse_exportId = Lens.lens (\DescribeExportResponse' {exportId} -> exportId) (\s@DescribeExportResponse' {} a -> s {exportId = a} :: DescribeExportResponse)

-- | The response's http status code.
describeExportResponse_httpStatus :: Lens.Lens' DescribeExportResponse Prelude.Int
describeExportResponse_httpStatus = Lens.lens (\DescribeExportResponse' {httpStatus} -> httpStatus) (\s@DescribeExportResponse' {} a -> s {httpStatus = a} :: DescribeExportResponse)

instance Prelude.NFData DescribeExportResponse
