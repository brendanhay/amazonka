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
-- Module      : Amazonka.Inspector2.CreateSbomExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a software bill of materials (SBOM) report.
module Amazonka.Inspector2.CreateSbomExport
  ( -- * Creating a Request
    CreateSbomExport (..),
    newCreateSbomExport,

    -- * Request Lenses
    createSbomExport_resourceFilterCriteria,
    createSbomExport_reportFormat,
    createSbomExport_s3Destination,

    -- * Destructuring the Response
    CreateSbomExportResponse (..),
    newCreateSbomExportResponse,

    -- * Response Lenses
    createSbomExportResponse_reportId,
    createSbomExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSbomExport' smart constructor.
data CreateSbomExport = CreateSbomExport'
  { -- | The resource filter criteria for the software bill of materials (SBOM)
    -- report.
    resourceFilterCriteria :: Prelude.Maybe ResourceFilterCriteria,
    -- | The output format for the software bill of materials (SBOM) report.
    reportFormat :: SbomReportFormat,
    s3Destination :: Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSbomExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceFilterCriteria', 'createSbomExport_resourceFilterCriteria' - The resource filter criteria for the software bill of materials (SBOM)
-- report.
--
-- 'reportFormat', 'createSbomExport_reportFormat' - The output format for the software bill of materials (SBOM) report.
--
-- 's3Destination', 'createSbomExport_s3Destination' - Undocumented member.
newCreateSbomExport ::
  -- | 'reportFormat'
  SbomReportFormat ->
  -- | 's3Destination'
  Destination ->
  CreateSbomExport
newCreateSbomExport pReportFormat_ pS3Destination_ =
  CreateSbomExport'
    { resourceFilterCriteria =
        Prelude.Nothing,
      reportFormat = pReportFormat_,
      s3Destination = pS3Destination_
    }

-- | The resource filter criteria for the software bill of materials (SBOM)
-- report.
createSbomExport_resourceFilterCriteria :: Lens.Lens' CreateSbomExport (Prelude.Maybe ResourceFilterCriteria)
createSbomExport_resourceFilterCriteria = Lens.lens (\CreateSbomExport' {resourceFilterCriteria} -> resourceFilterCriteria) (\s@CreateSbomExport' {} a -> s {resourceFilterCriteria = a} :: CreateSbomExport)

-- | The output format for the software bill of materials (SBOM) report.
createSbomExport_reportFormat :: Lens.Lens' CreateSbomExport SbomReportFormat
createSbomExport_reportFormat = Lens.lens (\CreateSbomExport' {reportFormat} -> reportFormat) (\s@CreateSbomExport' {} a -> s {reportFormat = a} :: CreateSbomExport)

-- | Undocumented member.
createSbomExport_s3Destination :: Lens.Lens' CreateSbomExport Destination
createSbomExport_s3Destination = Lens.lens (\CreateSbomExport' {s3Destination} -> s3Destination) (\s@CreateSbomExport' {} a -> s {s3Destination = a} :: CreateSbomExport)

instance Core.AWSRequest CreateSbomExport where
  type
    AWSResponse CreateSbomExport =
      CreateSbomExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSbomExportResponse'
            Prelude.<$> (x Data..?> "reportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSbomExport where
  hashWithSalt _salt CreateSbomExport' {..} =
    _salt
      `Prelude.hashWithSalt` resourceFilterCriteria
      `Prelude.hashWithSalt` reportFormat
      `Prelude.hashWithSalt` s3Destination

instance Prelude.NFData CreateSbomExport where
  rnf CreateSbomExport' {..} =
    Prelude.rnf resourceFilterCriteria
      `Prelude.seq` Prelude.rnf reportFormat
      `Prelude.seq` Prelude.rnf s3Destination

instance Data.ToHeaders CreateSbomExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSbomExport where
  toJSON CreateSbomExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceFilterCriteria" Data..=)
              Prelude.<$> resourceFilterCriteria,
            Prelude.Just ("reportFormat" Data..= reportFormat),
            Prelude.Just
              ("s3Destination" Data..= s3Destination)
          ]
      )

instance Data.ToPath CreateSbomExport where
  toPath = Prelude.const "/sbomexport/create"

instance Data.ToQuery CreateSbomExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSbomExportResponse' smart constructor.
data CreateSbomExportResponse = CreateSbomExportResponse'
  { -- | The report ID for the software bill of materials (SBOM) report.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSbomExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'createSbomExportResponse_reportId' - The report ID for the software bill of materials (SBOM) report.
--
-- 'httpStatus', 'createSbomExportResponse_httpStatus' - The response's http status code.
newCreateSbomExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSbomExportResponse
newCreateSbomExportResponse pHttpStatus_ =
  CreateSbomExportResponse'
    { reportId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The report ID for the software bill of materials (SBOM) report.
createSbomExportResponse_reportId :: Lens.Lens' CreateSbomExportResponse (Prelude.Maybe Prelude.Text)
createSbomExportResponse_reportId = Lens.lens (\CreateSbomExportResponse' {reportId} -> reportId) (\s@CreateSbomExportResponse' {} a -> s {reportId = a} :: CreateSbomExportResponse)

-- | The response's http status code.
createSbomExportResponse_httpStatus :: Lens.Lens' CreateSbomExportResponse Prelude.Int
createSbomExportResponse_httpStatus = Lens.lens (\CreateSbomExportResponse' {httpStatus} -> httpStatus) (\s@CreateSbomExportResponse' {} a -> s {httpStatus = a} :: CreateSbomExportResponse)

instance Prelude.NFData CreateSbomExportResponse where
  rnf CreateSbomExportResponse' {..} =
    Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf httpStatus
