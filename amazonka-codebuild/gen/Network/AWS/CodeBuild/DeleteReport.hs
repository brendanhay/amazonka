{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.DeleteReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report.
module Network.AWS.CodeBuild.DeleteReport
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

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteReport' smart constructor.
data DeleteReport = DeleteReport'
  { -- | The ARN of the report to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteReport where
  type Rs DeleteReport = DeleteReportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReport

instance Prelude.NFData DeleteReport

instance Prelude.ToHeaders DeleteReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.DeleteReport" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteReport where
  toJSON DeleteReport' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath DeleteReport where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReportResponse' smart constructor.
data DeleteReportResponse = DeleteReportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteReportResponse
