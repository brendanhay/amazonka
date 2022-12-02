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
-- Module      : Amazonka.Inspector2.CancelFindingsReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the given findings report.
module Amazonka.Inspector2.CancelFindingsReport
  ( -- * Creating a Request
    CancelFindingsReport (..),
    newCancelFindingsReport,

    -- * Request Lenses
    cancelFindingsReport_reportId,

    -- * Destructuring the Response
    CancelFindingsReportResponse (..),
    newCancelFindingsReportResponse,

    -- * Response Lenses
    cancelFindingsReportResponse_httpStatus,
    cancelFindingsReportResponse_reportId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelFindingsReport' smart constructor.
data CancelFindingsReport = CancelFindingsReport'
  { -- | The ID of the report to be canceled.
    reportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelFindingsReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'cancelFindingsReport_reportId' - The ID of the report to be canceled.
newCancelFindingsReport ::
  -- | 'reportId'
  Prelude.Text ->
  CancelFindingsReport
newCancelFindingsReport pReportId_ =
  CancelFindingsReport' {reportId = pReportId_}

-- | The ID of the report to be canceled.
cancelFindingsReport_reportId :: Lens.Lens' CancelFindingsReport Prelude.Text
cancelFindingsReport_reportId = Lens.lens (\CancelFindingsReport' {reportId} -> reportId) (\s@CancelFindingsReport' {} a -> s {reportId = a} :: CancelFindingsReport)

instance Core.AWSRequest CancelFindingsReport where
  type
    AWSResponse CancelFindingsReport =
      CancelFindingsReportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelFindingsReportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "reportId")
      )

instance Prelude.Hashable CancelFindingsReport where
  hashWithSalt _salt CancelFindingsReport' {..} =
    _salt `Prelude.hashWithSalt` reportId

instance Prelude.NFData CancelFindingsReport where
  rnf CancelFindingsReport' {..} = Prelude.rnf reportId

instance Data.ToHeaders CancelFindingsReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelFindingsReport where
  toJSON CancelFindingsReport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("reportId" Data..= reportId)]
      )

instance Data.ToPath CancelFindingsReport where
  toPath = Prelude.const "/reporting/cancel"

instance Data.ToQuery CancelFindingsReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelFindingsReportResponse' smart constructor.
data CancelFindingsReportResponse = CancelFindingsReportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the canceled report.
    reportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelFindingsReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelFindingsReportResponse_httpStatus' - The response's http status code.
--
-- 'reportId', 'cancelFindingsReportResponse_reportId' - The ID of the canceled report.
newCancelFindingsReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'reportId'
  Prelude.Text ->
  CancelFindingsReportResponse
newCancelFindingsReportResponse
  pHttpStatus_
  pReportId_ =
    CancelFindingsReportResponse'
      { httpStatus =
          pHttpStatus_,
        reportId = pReportId_
      }

-- | The response's http status code.
cancelFindingsReportResponse_httpStatus :: Lens.Lens' CancelFindingsReportResponse Prelude.Int
cancelFindingsReportResponse_httpStatus = Lens.lens (\CancelFindingsReportResponse' {httpStatus} -> httpStatus) (\s@CancelFindingsReportResponse' {} a -> s {httpStatus = a} :: CancelFindingsReportResponse)

-- | The ID of the canceled report.
cancelFindingsReportResponse_reportId :: Lens.Lens' CancelFindingsReportResponse Prelude.Text
cancelFindingsReportResponse_reportId = Lens.lens (\CancelFindingsReportResponse' {reportId} -> reportId) (\s@CancelFindingsReportResponse' {} a -> s {reportId = a} :: CancelFindingsReportResponse)

instance Prelude.NFData CancelFindingsReportResponse where
  rnf CancelFindingsReportResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf reportId
