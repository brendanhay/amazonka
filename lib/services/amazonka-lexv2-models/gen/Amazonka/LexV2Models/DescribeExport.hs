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
-- Module      : Amazonka.LexV2Models.DescribeExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific export.
module Amazonka.LexV2Models.DescribeExport
  ( -- * Creating a Request
    DescribeExport (..),
    newDescribeExport,

    -- * Request Lenses
    describeExport_exportId,

    -- * Destructuring the Response
    DescribeExportResponse (..),
    newDescribeExportResponse,

    -- * Response Lenses
    describeExportResponse_creationDateTime,
    describeExportResponse_downloadUrl,
    describeExportResponse_exportId,
    describeExportResponse_exportStatus,
    describeExportResponse_failureReasons,
    describeExportResponse_fileFormat,
    describeExportResponse_lastUpdatedDateTime,
    describeExportResponse_resourceSpecification,
    describeExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExportResponse'
            Prelude.<$> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "downloadUrl")
            Prelude.<*> (x Data..?> "exportId")
            Prelude.<*> (x Data..?> "exportStatus")
            Prelude.<*> (x Data..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "fileFormat")
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (x Data..?> "resourceSpecification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExport where
  hashWithSalt _salt DescribeExport' {..} =
    _salt `Prelude.hashWithSalt` exportId

instance Prelude.NFData DescribeExport where
  rnf DescribeExport' {..} = Prelude.rnf exportId

instance Data.ToHeaders DescribeExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeExport where
  toPath DescribeExport' {..} =
    Prelude.mconcat
      ["/exports/", Data.toBS exportId, "/"]

instance Data.ToQuery DescribeExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExportResponse' smart constructor.
data DescribeExportResponse = DescribeExportResponse'
  { -- | The date and time that the export was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | A pre-signed S3 URL that points to the bot or bot locale archive. The
    -- URL is only available for 5 minutes after calling the @DescribeExport@
    -- operation.
    downloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the described export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The status of the export. When the status is @Complete@ the export
    -- archive file is available for download.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | If the @exportStatus@ is failed, contains one or more reasons why the
    -- export could not be completed.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | The file format used in the files that describe the resource.
    fileFormat :: Prelude.Maybe ImportExportFileFormat,
    -- | The last date and time that the export was updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The bot, bot ID, and optional locale ID of the exported bot or bot
    -- locale.
    resourceSpecification :: Prelude.Maybe ExportResourceSpecification,
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
-- 'creationDateTime', 'describeExportResponse_creationDateTime' - The date and time that the export was created.
--
-- 'downloadUrl', 'describeExportResponse_downloadUrl' - A pre-signed S3 URL that points to the bot or bot locale archive. The
-- URL is only available for 5 minutes after calling the @DescribeExport@
-- operation.
--
-- 'exportId', 'describeExportResponse_exportId' - The unique identifier of the described export.
--
-- 'exportStatus', 'describeExportResponse_exportStatus' - The status of the export. When the status is @Complete@ the export
-- archive file is available for download.
--
-- 'failureReasons', 'describeExportResponse_failureReasons' - If the @exportStatus@ is failed, contains one or more reasons why the
-- export could not be completed.
--
-- 'fileFormat', 'describeExportResponse_fileFormat' - The file format used in the files that describe the resource.
--
-- 'lastUpdatedDateTime', 'describeExportResponse_lastUpdatedDateTime' - The last date and time that the export was updated.
--
-- 'resourceSpecification', 'describeExportResponse_resourceSpecification' - The bot, bot ID, and optional locale ID of the exported bot or bot
-- locale.
--
-- 'httpStatus', 'describeExportResponse_httpStatus' - The response's http status code.
newDescribeExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExportResponse
newDescribeExportResponse pHttpStatus_ =
  DescribeExportResponse'
    { creationDateTime =
        Prelude.Nothing,
      downloadUrl = Prelude.Nothing,
      exportId = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      failureReasons = Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      resourceSpecification = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that the export was created.
describeExportResponse_creationDateTime :: Lens.Lens' DescribeExportResponse (Prelude.Maybe Prelude.UTCTime)
describeExportResponse_creationDateTime = Lens.lens (\DescribeExportResponse' {creationDateTime} -> creationDateTime) (\s@DescribeExportResponse' {} a -> s {creationDateTime = a} :: DescribeExportResponse) Prelude.. Lens.mapping Data._Time

-- | A pre-signed S3 URL that points to the bot or bot locale archive. The
-- URL is only available for 5 minutes after calling the @DescribeExport@
-- operation.
describeExportResponse_downloadUrl :: Lens.Lens' DescribeExportResponse (Prelude.Maybe Prelude.Text)
describeExportResponse_downloadUrl = Lens.lens (\DescribeExportResponse' {downloadUrl} -> downloadUrl) (\s@DescribeExportResponse' {} a -> s {downloadUrl = a} :: DescribeExportResponse)

-- | The unique identifier of the described export.
describeExportResponse_exportId :: Lens.Lens' DescribeExportResponse (Prelude.Maybe Prelude.Text)
describeExportResponse_exportId = Lens.lens (\DescribeExportResponse' {exportId} -> exportId) (\s@DescribeExportResponse' {} a -> s {exportId = a} :: DescribeExportResponse)

-- | The status of the export. When the status is @Complete@ the export
-- archive file is available for download.
describeExportResponse_exportStatus :: Lens.Lens' DescribeExportResponse (Prelude.Maybe ExportStatus)
describeExportResponse_exportStatus = Lens.lens (\DescribeExportResponse' {exportStatus} -> exportStatus) (\s@DescribeExportResponse' {} a -> s {exportStatus = a} :: DescribeExportResponse)

-- | If the @exportStatus@ is failed, contains one or more reasons why the
-- export could not be completed.
describeExportResponse_failureReasons :: Lens.Lens' DescribeExportResponse (Prelude.Maybe [Prelude.Text])
describeExportResponse_failureReasons = Lens.lens (\DescribeExportResponse' {failureReasons} -> failureReasons) (\s@DescribeExportResponse' {} a -> s {failureReasons = a} :: DescribeExportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The file format used in the files that describe the resource.
describeExportResponse_fileFormat :: Lens.Lens' DescribeExportResponse (Prelude.Maybe ImportExportFileFormat)
describeExportResponse_fileFormat = Lens.lens (\DescribeExportResponse' {fileFormat} -> fileFormat) (\s@DescribeExportResponse' {} a -> s {fileFormat = a} :: DescribeExportResponse)

-- | The last date and time that the export was updated.
describeExportResponse_lastUpdatedDateTime :: Lens.Lens' DescribeExportResponse (Prelude.Maybe Prelude.UTCTime)
describeExportResponse_lastUpdatedDateTime = Lens.lens (\DescribeExportResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeExportResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeExportResponse) Prelude.. Lens.mapping Data._Time

-- | The bot, bot ID, and optional locale ID of the exported bot or bot
-- locale.
describeExportResponse_resourceSpecification :: Lens.Lens' DescribeExportResponse (Prelude.Maybe ExportResourceSpecification)
describeExportResponse_resourceSpecification = Lens.lens (\DescribeExportResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeExportResponse' {} a -> s {resourceSpecification = a} :: DescribeExportResponse)

-- | The response's http status code.
describeExportResponse_httpStatus :: Lens.Lens' DescribeExportResponse Prelude.Int
describeExportResponse_httpStatus = Lens.lens (\DescribeExportResponse' {httpStatus} -> httpStatus) (\s@DescribeExportResponse' {} a -> s {httpStatus = a} :: DescribeExportResponse)

instance Prelude.NFData DescribeExportResponse where
  rnf DescribeExportResponse' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf downloadUrl
      `Prelude.seq` Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf failureReasons
      `Prelude.seq` Prelude.rnf fileFormat
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf resourceSpecification
      `Prelude.seq` Prelude.rnf httpStatus
