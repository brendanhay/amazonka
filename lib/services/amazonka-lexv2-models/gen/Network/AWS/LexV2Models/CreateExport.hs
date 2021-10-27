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
-- Module      : Network.AWS.LexV2Models.CreateExport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a zip archive containing the contents of a bot or a bot locale.
-- The archive contains a directory structure that contains JSON files that
-- define the bot.
--
-- You can create an archive that contains the complete definition of a
-- bot, or you can specify that the archive contain only the definition of
-- a single bot locale.
--
-- For more information about exporting bots, and about the structure of
-- the export archive, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/importing-exporting.html Importing and exporting bots>
module Network.AWS.LexV2Models.CreateExport
  ( -- * Creating a Request
    CreateExport (..),
    newCreateExport,

    -- * Request Lenses
    createExport_filePassword,
    createExport_resourceSpecification,
    createExport_fileFormat,

    -- * Destructuring the Response
    CreateExportResponse (..),
    newCreateExportResponse,

    -- * Response Lenses
    createExportResponse_resourceSpecification,
    createExportResponse_fileFormat,
    createExportResponse_exportStatus,
    createExportResponse_creationDateTime,
    createExportResponse_exportId,
    createExportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateExport' smart constructor.
data CreateExport = CreateExport'
  { -- | An password to use to encrypt the exported archive. Using a password is
    -- optional, but you should encrypt the archive to protect the data in
    -- transit between Amazon Lex and your local computer.
    filePassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Specifies the type of resource to export, either a bot or a bot locale.
    -- You can only specify one type of resource to export.
    resourceSpecification :: ExportResourceSpecification,
    -- | The file format of the bot or bot locale definition files.
    fileFormat :: ImportExportFileFormat
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePassword', 'createExport_filePassword' - An password to use to encrypt the exported archive. Using a password is
-- optional, but you should encrypt the archive to protect the data in
-- transit between Amazon Lex and your local computer.
--
-- 'resourceSpecification', 'createExport_resourceSpecification' - Specifies the type of resource to export, either a bot or a bot locale.
-- You can only specify one type of resource to export.
--
-- 'fileFormat', 'createExport_fileFormat' - The file format of the bot or bot locale definition files.
newCreateExport ::
  -- | 'resourceSpecification'
  ExportResourceSpecification ->
  -- | 'fileFormat'
  ImportExportFileFormat ->
  CreateExport
newCreateExport pResourceSpecification_ pFileFormat_ =
  CreateExport'
    { filePassword = Prelude.Nothing,
      resourceSpecification = pResourceSpecification_,
      fileFormat = pFileFormat_
    }

-- | An password to use to encrypt the exported archive. Using a password is
-- optional, but you should encrypt the archive to protect the data in
-- transit between Amazon Lex and your local computer.
createExport_filePassword :: Lens.Lens' CreateExport (Prelude.Maybe Prelude.Text)
createExport_filePassword = Lens.lens (\CreateExport' {filePassword} -> filePassword) (\s@CreateExport' {} a -> s {filePassword = a} :: CreateExport) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies the type of resource to export, either a bot or a bot locale.
-- You can only specify one type of resource to export.
createExport_resourceSpecification :: Lens.Lens' CreateExport ExportResourceSpecification
createExport_resourceSpecification = Lens.lens (\CreateExport' {resourceSpecification} -> resourceSpecification) (\s@CreateExport' {} a -> s {resourceSpecification = a} :: CreateExport)

-- | The file format of the bot or bot locale definition files.
createExport_fileFormat :: Lens.Lens' CreateExport ImportExportFileFormat
createExport_fileFormat = Lens.lens (\CreateExport' {fileFormat} -> fileFormat) (\s@CreateExport' {} a -> s {fileFormat = a} :: CreateExport)

instance Core.AWSRequest CreateExport where
  type AWSResponse CreateExport = CreateExportResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExportResponse'
            Prelude.<$> (x Core..?> "resourceSpecification")
            Prelude.<*> (x Core..?> "fileFormat")
            Prelude.<*> (x Core..?> "exportStatus")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "exportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateExport

instance Prelude.NFData CreateExport

instance Core.ToHeaders CreateExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateExport where
  toJSON CreateExport' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filePassword" Core..=) Prelude.<$> filePassword,
            Prelude.Just
              ( "resourceSpecification"
                  Core..= resourceSpecification
              ),
            Prelude.Just ("fileFormat" Core..= fileFormat)
          ]
      )

instance Core.ToPath CreateExport where
  toPath = Prelude.const "/exports/"

instance Core.ToQuery CreateExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateExportResponse' smart constructor.
data CreateExportResponse = CreateExportResponse'
  { -- | A description of the type of resource that was exported, either a bot or
    -- a bot locale.
    resourceSpecification :: Prelude.Maybe ExportResourceSpecification,
    -- | The file format used for the bot or bot locale definition files.
    fileFormat :: Prelude.Maybe ImportExportFileFormat,
    -- | The status of the export. When the status is @Completed@, you can use
    -- the operation to get the pre-signed S3 URL link to your exported bot or
    -- bot locale.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The date and time that the request to export a bot was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | An identifier for a specific request to create an export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSpecification', 'createExportResponse_resourceSpecification' - A description of the type of resource that was exported, either a bot or
-- a bot locale.
--
-- 'fileFormat', 'createExportResponse_fileFormat' - The file format used for the bot or bot locale definition files.
--
-- 'exportStatus', 'createExportResponse_exportStatus' - The status of the export. When the status is @Completed@, you can use
-- the operation to get the pre-signed S3 URL link to your exported bot or
-- bot locale.
--
-- 'creationDateTime', 'createExportResponse_creationDateTime' - The date and time that the request to export a bot was created.
--
-- 'exportId', 'createExportResponse_exportId' - An identifier for a specific request to create an export.
--
-- 'httpStatus', 'createExportResponse_httpStatus' - The response's http status code.
newCreateExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateExportResponse
newCreateExportResponse pHttpStatus_ =
  CreateExportResponse'
    { resourceSpecification =
        Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      exportId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the type of resource that was exported, either a bot or
-- a bot locale.
createExportResponse_resourceSpecification :: Lens.Lens' CreateExportResponse (Prelude.Maybe ExportResourceSpecification)
createExportResponse_resourceSpecification = Lens.lens (\CreateExportResponse' {resourceSpecification} -> resourceSpecification) (\s@CreateExportResponse' {} a -> s {resourceSpecification = a} :: CreateExportResponse)

-- | The file format used for the bot or bot locale definition files.
createExportResponse_fileFormat :: Lens.Lens' CreateExportResponse (Prelude.Maybe ImportExportFileFormat)
createExportResponse_fileFormat = Lens.lens (\CreateExportResponse' {fileFormat} -> fileFormat) (\s@CreateExportResponse' {} a -> s {fileFormat = a} :: CreateExportResponse)

-- | The status of the export. When the status is @Completed@, you can use
-- the operation to get the pre-signed S3 URL link to your exported bot or
-- bot locale.
createExportResponse_exportStatus :: Lens.Lens' CreateExportResponse (Prelude.Maybe ExportStatus)
createExportResponse_exportStatus = Lens.lens (\CreateExportResponse' {exportStatus} -> exportStatus) (\s@CreateExportResponse' {} a -> s {exportStatus = a} :: CreateExportResponse)

-- | The date and time that the request to export a bot was created.
createExportResponse_creationDateTime :: Lens.Lens' CreateExportResponse (Prelude.Maybe Prelude.UTCTime)
createExportResponse_creationDateTime = Lens.lens (\CreateExportResponse' {creationDateTime} -> creationDateTime) (\s@CreateExportResponse' {} a -> s {creationDateTime = a} :: CreateExportResponse) Prelude.. Lens.mapping Core._Time

-- | An identifier for a specific request to create an export.
createExportResponse_exportId :: Lens.Lens' CreateExportResponse (Prelude.Maybe Prelude.Text)
createExportResponse_exportId = Lens.lens (\CreateExportResponse' {exportId} -> exportId) (\s@CreateExportResponse' {} a -> s {exportId = a} :: CreateExportResponse)

-- | The response's http status code.
createExportResponse_httpStatus :: Lens.Lens' CreateExportResponse Prelude.Int
createExportResponse_httpStatus = Lens.lens (\CreateExportResponse' {httpStatus} -> httpStatus) (\s@CreateExportResponse' {} a -> s {httpStatus = a} :: CreateExportResponse)

instance Prelude.NFData CreateExportResponse
