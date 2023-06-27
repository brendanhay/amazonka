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
-- Module      : Amazonka.Inspector2.CancelSbomExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a software bill of materials (SBOM) report.
module Amazonka.Inspector2.CancelSbomExport
  ( -- * Creating a Request
    CancelSbomExport (..),
    newCancelSbomExport,

    -- * Request Lenses
    cancelSbomExport_reportId,

    -- * Destructuring the Response
    CancelSbomExportResponse (..),
    newCancelSbomExportResponse,

    -- * Response Lenses
    cancelSbomExportResponse_reportId,
    cancelSbomExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelSbomExport' smart constructor.
data CancelSbomExport = CancelSbomExport'
  { -- | The report ID of the SBOM export to cancel.
    reportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSbomExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'cancelSbomExport_reportId' - The report ID of the SBOM export to cancel.
newCancelSbomExport ::
  -- | 'reportId'
  Prelude.Text ->
  CancelSbomExport
newCancelSbomExport pReportId_ =
  CancelSbomExport' {reportId = pReportId_}

-- | The report ID of the SBOM export to cancel.
cancelSbomExport_reportId :: Lens.Lens' CancelSbomExport Prelude.Text
cancelSbomExport_reportId = Lens.lens (\CancelSbomExport' {reportId} -> reportId) (\s@CancelSbomExport' {} a -> s {reportId = a} :: CancelSbomExport)

instance Core.AWSRequest CancelSbomExport where
  type
    AWSResponse CancelSbomExport =
      CancelSbomExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelSbomExportResponse'
            Prelude.<$> (x Data..?> "reportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelSbomExport where
  hashWithSalt _salt CancelSbomExport' {..} =
    _salt `Prelude.hashWithSalt` reportId

instance Prelude.NFData CancelSbomExport where
  rnf CancelSbomExport' {..} = Prelude.rnf reportId

instance Data.ToHeaders CancelSbomExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelSbomExport where
  toJSON CancelSbomExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("reportId" Data..= reportId)]
      )

instance Data.ToPath CancelSbomExport where
  toPath = Prelude.const "/sbomexport/cancel"

instance Data.ToQuery CancelSbomExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelSbomExportResponse' smart constructor.
data CancelSbomExportResponse = CancelSbomExportResponse'
  { -- | The report ID of the canceled SBOM export.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSbomExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'cancelSbomExportResponse_reportId' - The report ID of the canceled SBOM export.
--
-- 'httpStatus', 'cancelSbomExportResponse_httpStatus' - The response's http status code.
newCancelSbomExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelSbomExportResponse
newCancelSbomExportResponse pHttpStatus_ =
  CancelSbomExportResponse'
    { reportId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The report ID of the canceled SBOM export.
cancelSbomExportResponse_reportId :: Lens.Lens' CancelSbomExportResponse (Prelude.Maybe Prelude.Text)
cancelSbomExportResponse_reportId = Lens.lens (\CancelSbomExportResponse' {reportId} -> reportId) (\s@CancelSbomExportResponse' {} a -> s {reportId = a} :: CancelSbomExportResponse)

-- | The response's http status code.
cancelSbomExportResponse_httpStatus :: Lens.Lens' CancelSbomExportResponse Prelude.Int
cancelSbomExportResponse_httpStatus = Lens.lens (\CancelSbomExportResponse' {httpStatus} -> httpStatus) (\s@CancelSbomExportResponse' {} a -> s {httpStatus = a} :: CancelSbomExportResponse)

instance Prelude.NFData CancelSbomExportResponse where
  rnf CancelSbomExportResponse' {..} =
    Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf httpStatus
