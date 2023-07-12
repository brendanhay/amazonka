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
-- Module      : Amazonka.CodeBuild.DeleteReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report.
module Amazonka.CodeBuild.DeleteReport
  ( -- * Creating a Request
    DeleteReport (..),
    newDeleteReport,

    -- * Request Lenses
    deleteReport_arn,

    -- * Destructuring the Response
    DeleteReportResponse (..),
    newDeleteReportResponse,

    -- * Response Lenses
    deleteReportResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteReport' smart constructor.
data DeleteReport = DeleteReport'
  { -- | The ARN of the report to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteReport_arn' - The ARN of the report to delete.
newDeleteReport ::
  -- | 'arn'
  Prelude.Text ->
  DeleteReport
newDeleteReport pArn_ = DeleteReport' {arn = pArn_}

-- | The ARN of the report to delete.
deleteReport_arn :: Lens.Lens' DeleteReport Prelude.Text
deleteReport_arn = Lens.lens (\DeleteReport' {arn} -> arn) (\s@DeleteReport' {} a -> s {arn = a} :: DeleteReport)

instance Core.AWSRequest DeleteReport where
  type AWSResponse DeleteReport = DeleteReportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReport where
  hashWithSalt _salt DeleteReport' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteReport where
  rnf DeleteReport' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.DeleteReport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteReport where
  toJSON DeleteReport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteReport where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReportResponse' smart constructor.
data DeleteReportResponse = DeleteReportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReportResponse_httpStatus' - The response's http status code.
newDeleteReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReportResponse
newDeleteReportResponse pHttpStatus_ =
  DeleteReportResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteReportResponse_httpStatus :: Lens.Lens' DeleteReportResponse Prelude.Int
deleteReportResponse_httpStatus = Lens.lens (\DeleteReportResponse' {httpStatus} -> httpStatus) (\s@DeleteReportResponse' {} a -> s {httpStatus = a} :: DeleteReportResponse)

instance Prelude.NFData DeleteReportResponse where
  rnf DeleteReportResponse' {..} =
    Prelude.rnf httpStatus
