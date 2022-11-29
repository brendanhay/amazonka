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
-- Module      : Amazonka.LexV2Models.UpdateExport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the password used to protect an export zip archive.
--
-- The password is not required. If you don\'t supply a password, Amazon
-- Lex generates a zip file that is not protected by a password. This is
-- the archive that is available at the pre-signed S3 URL provided by the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeExport.html DescribeExport>
-- operation.
module Amazonka.LexV2Models.UpdateExport
  ( -- * Creating a Request
    UpdateExport (..),
    newUpdateExport,

    -- * Request Lenses
    updateExport_filePassword,
    updateExport_exportId,

    -- * Destructuring the Response
    UpdateExportResponse (..),
    newUpdateExportResponse,

    -- * Response Lenses
    updateExportResponse_creationDateTime,
    updateExportResponse_resourceSpecification,
    updateExportResponse_exportStatus,
    updateExportResponse_exportId,
    updateExportResponse_lastUpdatedDateTime,
    updateExportResponse_fileFormat,
    updateExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateExport' smart constructor.
data UpdateExport = UpdateExport'
  { -- | The new password to use to encrypt the export zip archive.
    filePassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The unique identifier Amazon Lex assigned to the export.
    exportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePassword', 'updateExport_filePassword' - The new password to use to encrypt the export zip archive.
--
-- 'exportId', 'updateExport_exportId' - The unique identifier Amazon Lex assigned to the export.
newUpdateExport ::
  -- | 'exportId'
  Prelude.Text ->
  UpdateExport
newUpdateExport pExportId_ =
  UpdateExport'
    { filePassword = Prelude.Nothing,
      exportId = pExportId_
    }

-- | The new password to use to encrypt the export zip archive.
updateExport_filePassword :: Lens.Lens' UpdateExport (Prelude.Maybe Prelude.Text)
updateExport_filePassword = Lens.lens (\UpdateExport' {filePassword} -> filePassword) (\s@UpdateExport' {} a -> s {filePassword = a} :: UpdateExport) Prelude.. Lens.mapping Core._Sensitive

-- | The unique identifier Amazon Lex assigned to the export.
updateExport_exportId :: Lens.Lens' UpdateExport Prelude.Text
updateExport_exportId = Lens.lens (\UpdateExport' {exportId} -> exportId) (\s@UpdateExport' {} a -> s {exportId = a} :: UpdateExport)

instance Core.AWSRequest UpdateExport where
  type AWSResponse UpdateExport = UpdateExportResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateExportResponse'
            Prelude.<$> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "resourceSpecification")
            Prelude.<*> (x Core..?> "exportStatus")
            Prelude.<*> (x Core..?> "exportId")
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (x Core..?> "fileFormat")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateExport where
  hashWithSalt _salt UpdateExport' {..} =
    _salt `Prelude.hashWithSalt` filePassword
      `Prelude.hashWithSalt` exportId

instance Prelude.NFData UpdateExport where
  rnf UpdateExport' {..} =
    Prelude.rnf filePassword
      `Prelude.seq` Prelude.rnf exportId

instance Core.ToHeaders UpdateExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateExport where
  toJSON UpdateExport' {..} =
    Core.object
      ( Prelude.catMaybes
          [("filePassword" Core..=) Prelude.<$> filePassword]
      )

instance Core.ToPath UpdateExport where
  toPath UpdateExport' {..} =
    Prelude.mconcat
      ["/exports/", Core.toBS exportId, "/"]

instance Core.ToQuery UpdateExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateExportResponse' smart constructor.
data UpdateExportResponse = UpdateExportResponse'
  { -- | The date and time that the export was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | A description of the type of resource that was exported, either a bot or
    -- a bot locale.
    resourceSpecification :: Prelude.Maybe ExportResourceSpecification,
    -- | The status of the export. When the status is @Completed@ the export
    -- archive is available for download.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The unique identifier Amazon Lex assigned to the export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the export was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The file format used for the files that define the resource. The @TSV@
    -- format is required to export a custom vocabulary only; otherwise use
    -- @LexJson@ format.
    fileFormat :: Prelude.Maybe ImportExportFileFormat,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'updateExportResponse_creationDateTime' - The date and time that the export was created.
--
-- 'resourceSpecification', 'updateExportResponse_resourceSpecification' - A description of the type of resource that was exported, either a bot or
-- a bot locale.
--
-- 'exportStatus', 'updateExportResponse_exportStatus' - The status of the export. When the status is @Completed@ the export
-- archive is available for download.
--
-- 'exportId', 'updateExportResponse_exportId' - The unique identifier Amazon Lex assigned to the export.
--
-- 'lastUpdatedDateTime', 'updateExportResponse_lastUpdatedDateTime' - The date and time that the export was last updated.
--
-- 'fileFormat', 'updateExportResponse_fileFormat' - The file format used for the files that define the resource. The @TSV@
-- format is required to export a custom vocabulary only; otherwise use
-- @LexJson@ format.
--
-- 'httpStatus', 'updateExportResponse_httpStatus' - The response's http status code.
newUpdateExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateExportResponse
newUpdateExportResponse pHttpStatus_ =
  UpdateExportResponse'
    { creationDateTime =
        Prelude.Nothing,
      resourceSpecification = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      exportId = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that the export was created.
updateExportResponse_creationDateTime :: Lens.Lens' UpdateExportResponse (Prelude.Maybe Prelude.UTCTime)
updateExportResponse_creationDateTime = Lens.lens (\UpdateExportResponse' {creationDateTime} -> creationDateTime) (\s@UpdateExportResponse' {} a -> s {creationDateTime = a} :: UpdateExportResponse) Prelude.. Lens.mapping Core._Time

-- | A description of the type of resource that was exported, either a bot or
-- a bot locale.
updateExportResponse_resourceSpecification :: Lens.Lens' UpdateExportResponse (Prelude.Maybe ExportResourceSpecification)
updateExportResponse_resourceSpecification = Lens.lens (\UpdateExportResponse' {resourceSpecification} -> resourceSpecification) (\s@UpdateExportResponse' {} a -> s {resourceSpecification = a} :: UpdateExportResponse)

-- | The status of the export. When the status is @Completed@ the export
-- archive is available for download.
updateExportResponse_exportStatus :: Lens.Lens' UpdateExportResponse (Prelude.Maybe ExportStatus)
updateExportResponse_exportStatus = Lens.lens (\UpdateExportResponse' {exportStatus} -> exportStatus) (\s@UpdateExportResponse' {} a -> s {exportStatus = a} :: UpdateExportResponse)

-- | The unique identifier Amazon Lex assigned to the export.
updateExportResponse_exportId :: Lens.Lens' UpdateExportResponse (Prelude.Maybe Prelude.Text)
updateExportResponse_exportId = Lens.lens (\UpdateExportResponse' {exportId} -> exportId) (\s@UpdateExportResponse' {} a -> s {exportId = a} :: UpdateExportResponse)

-- | The date and time that the export was last updated.
updateExportResponse_lastUpdatedDateTime :: Lens.Lens' UpdateExportResponse (Prelude.Maybe Prelude.UTCTime)
updateExportResponse_lastUpdatedDateTime = Lens.lens (\UpdateExportResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateExportResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateExportResponse) Prelude.. Lens.mapping Core._Time

-- | The file format used for the files that define the resource. The @TSV@
-- format is required to export a custom vocabulary only; otherwise use
-- @LexJson@ format.
updateExportResponse_fileFormat :: Lens.Lens' UpdateExportResponse (Prelude.Maybe ImportExportFileFormat)
updateExportResponse_fileFormat = Lens.lens (\UpdateExportResponse' {fileFormat} -> fileFormat) (\s@UpdateExportResponse' {} a -> s {fileFormat = a} :: UpdateExportResponse)

-- | The response's http status code.
updateExportResponse_httpStatus :: Lens.Lens' UpdateExportResponse Prelude.Int
updateExportResponse_httpStatus = Lens.lens (\UpdateExportResponse' {httpStatus} -> httpStatus) (\s@UpdateExportResponse' {} a -> s {httpStatus = a} :: UpdateExportResponse)

instance Prelude.NFData UpdateExportResponse where
  rnf UpdateExportResponse' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf resourceSpecification
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf fileFormat
      `Prelude.seq` Prelude.rnf httpStatus
