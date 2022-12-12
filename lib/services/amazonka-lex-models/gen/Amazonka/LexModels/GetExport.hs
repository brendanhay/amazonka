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
-- Module      : Amazonka.LexModels.GetExport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports the contents of a Amazon Lex resource in a specified format.
module Amazonka.LexModels.GetExport
  ( -- * Creating a Request
    GetExport (..),
    newGetExport,

    -- * Request Lenses
    getExport_name,
    getExport_version,
    getExport_resourceType,
    getExport_exportType,

    -- * Destructuring the Response
    GetExportResponse (..),
    newGetExportResponse,

    -- * Response Lenses
    getExportResponse_exportStatus,
    getExportResponse_exportType,
    getExportResponse_failureReason,
    getExportResponse_name,
    getExportResponse_resourceType,
    getExportResponse_url,
    getExportResponse_version,
    getExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExport' smart constructor.
data GetExport = GetExport'
  { -- | The name of the bot to export.
    name :: Prelude.Text,
    -- | The version of the bot to export.
    version :: Prelude.Text,
    -- | The type of resource to export.
    resourceType :: ResourceType,
    -- | The format of the exported data.
    exportType :: ExportType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getExport_name' - The name of the bot to export.
--
-- 'version', 'getExport_version' - The version of the bot to export.
--
-- 'resourceType', 'getExport_resourceType' - The type of resource to export.
--
-- 'exportType', 'getExport_exportType' - The format of the exported data.
newGetExport ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'exportType'
  ExportType ->
  GetExport
newGetExport
  pName_
  pVersion_
  pResourceType_
  pExportType_ =
    GetExport'
      { name = pName_,
        version = pVersion_,
        resourceType = pResourceType_,
        exportType = pExportType_
      }

-- | The name of the bot to export.
getExport_name :: Lens.Lens' GetExport Prelude.Text
getExport_name = Lens.lens (\GetExport' {name} -> name) (\s@GetExport' {} a -> s {name = a} :: GetExport)

-- | The version of the bot to export.
getExport_version :: Lens.Lens' GetExport Prelude.Text
getExport_version = Lens.lens (\GetExport' {version} -> version) (\s@GetExport' {} a -> s {version = a} :: GetExport)

-- | The type of resource to export.
getExport_resourceType :: Lens.Lens' GetExport ResourceType
getExport_resourceType = Lens.lens (\GetExport' {resourceType} -> resourceType) (\s@GetExport' {} a -> s {resourceType = a} :: GetExport)

-- | The format of the exported data.
getExport_exportType :: Lens.Lens' GetExport ExportType
getExport_exportType = Lens.lens (\GetExport' {exportType} -> exportType) (\s@GetExport' {} a -> s {exportType = a} :: GetExport)

instance Core.AWSRequest GetExport where
  type AWSResponse GetExport = GetExportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExportResponse'
            Prelude.<$> (x Data..?> "exportStatus")
            Prelude.<*> (x Data..?> "exportType")
            Prelude.<*> (x Data..?> "failureReason")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "resourceType")
            Prelude.<*> (x Data..?> "url")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExport where
  hashWithSalt _salt GetExport' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` exportType

instance Prelude.NFData GetExport where
  rnf GetExport' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf exportType

instance Data.ToHeaders GetExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetExport where
  toPath = Prelude.const "/exports/"

instance Data.ToQuery GetExport where
  toQuery GetExport' {..} =
    Prelude.mconcat
      [ "name" Data.=: name,
        "version" Data.=: version,
        "resourceType" Data.=: resourceType,
        "exportType" Data.=: exportType
      ]

-- | /See:/ 'newGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { -- | The status of the export.
    --
    -- -   @IN_PROGRESS@ - The export is in progress.
    --
    -- -   @READY@ - The export is complete.
    --
    -- -   @FAILED@ - The export could not be completed.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The format of the exported data.
    exportType :: Prelude.Maybe ExportType,
    -- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
    -- to export the resource.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot being exported.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the exported resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | An S3 pre-signed URL that provides the location of the exported
    -- resource. The exported resource is a ZIP archive that contains the
    -- exported resource in JSON format. The structure of the archive may
    -- change. Your code should not rely on the archive structure.
    url :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot being exported.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportStatus', 'getExportResponse_exportStatus' - The status of the export.
--
-- -   @IN_PROGRESS@ - The export is in progress.
--
-- -   @READY@ - The export is complete.
--
-- -   @FAILED@ - The export could not be completed.
--
-- 'exportType', 'getExportResponse_exportType' - The format of the exported data.
--
-- 'failureReason', 'getExportResponse_failureReason' - If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to export the resource.
--
-- 'name', 'getExportResponse_name' - The name of the bot being exported.
--
-- 'resourceType', 'getExportResponse_resourceType' - The type of the exported resource.
--
-- 'url', 'getExportResponse_url' - An S3 pre-signed URL that provides the location of the exported
-- resource. The exported resource is a ZIP archive that contains the
-- exported resource in JSON format. The structure of the archive may
-- change. Your code should not rely on the archive structure.
--
-- 'version', 'getExportResponse_version' - The version of the bot being exported.
--
-- 'httpStatus', 'getExportResponse_httpStatus' - The response's http status code.
newGetExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExportResponse
newGetExportResponse pHttpStatus_ =
  GetExportResponse'
    { exportStatus = Prelude.Nothing,
      exportType = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      url = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the export.
--
-- -   @IN_PROGRESS@ - The export is in progress.
--
-- -   @READY@ - The export is complete.
--
-- -   @FAILED@ - The export could not be completed.
getExportResponse_exportStatus :: Lens.Lens' GetExportResponse (Prelude.Maybe ExportStatus)
getExportResponse_exportStatus = Lens.lens (\GetExportResponse' {exportStatus} -> exportStatus) (\s@GetExportResponse' {} a -> s {exportStatus = a} :: GetExportResponse)

-- | The format of the exported data.
getExportResponse_exportType :: Lens.Lens' GetExportResponse (Prelude.Maybe ExportType)
getExportResponse_exportType = Lens.lens (\GetExportResponse' {exportType} -> exportType) (\s@GetExportResponse' {} a -> s {exportType = a} :: GetExportResponse)

-- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to export the resource.
getExportResponse_failureReason :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.Text)
getExportResponse_failureReason = Lens.lens (\GetExportResponse' {failureReason} -> failureReason) (\s@GetExportResponse' {} a -> s {failureReason = a} :: GetExportResponse)

-- | The name of the bot being exported.
getExportResponse_name :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.Text)
getExportResponse_name = Lens.lens (\GetExportResponse' {name} -> name) (\s@GetExportResponse' {} a -> s {name = a} :: GetExportResponse)

-- | The type of the exported resource.
getExportResponse_resourceType :: Lens.Lens' GetExportResponse (Prelude.Maybe ResourceType)
getExportResponse_resourceType = Lens.lens (\GetExportResponse' {resourceType} -> resourceType) (\s@GetExportResponse' {} a -> s {resourceType = a} :: GetExportResponse)

-- | An S3 pre-signed URL that provides the location of the exported
-- resource. The exported resource is a ZIP archive that contains the
-- exported resource in JSON format. The structure of the archive may
-- change. Your code should not rely on the archive structure.
getExportResponse_url :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.Text)
getExportResponse_url = Lens.lens (\GetExportResponse' {url} -> url) (\s@GetExportResponse' {} a -> s {url = a} :: GetExportResponse)

-- | The version of the bot being exported.
getExportResponse_version :: Lens.Lens' GetExportResponse (Prelude.Maybe Prelude.Text)
getExportResponse_version = Lens.lens (\GetExportResponse' {version} -> version) (\s@GetExportResponse' {} a -> s {version = a} :: GetExportResponse)

-- | The response's http status code.
getExportResponse_httpStatus :: Lens.Lens' GetExportResponse Prelude.Int
getExportResponse_httpStatus = Lens.lens (\GetExportResponse' {httpStatus} -> httpStatus) (\s@GetExportResponse' {} a -> s {httpStatus = a} :: GetExportResponse)

instance Prelude.NFData GetExportResponse where
  rnf GetExportResponse' {..} =
    Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf exportType
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
