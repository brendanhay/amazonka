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
-- Module      : Amazonka.ApplicationCostProfiler.DeleteReportDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified report definition in AWS Application Cost
-- Profiler. This stops the report from being generated.
module Amazonka.ApplicationCostProfiler.DeleteReportDefinition
  ( -- * Creating a Request
    DeleteReportDefinition (..),
    newDeleteReportDefinition,

    -- * Request Lenses
    deleteReportDefinition_reportId,

    -- * Destructuring the Response
    DeleteReportDefinitionResponse (..),
    newDeleteReportDefinitionResponse,

    -- * Response Lenses
    deleteReportDefinitionResponse_reportId,
    deleteReportDefinitionResponse_httpStatus,
  )
where

import Amazonka.ApplicationCostProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteReportDefinition' smart constructor.
data DeleteReportDefinition = DeleteReportDefinition'
  { -- | Required. ID of the report to delete.
    reportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReportDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'deleteReportDefinition_reportId' - Required. ID of the report to delete.
newDeleteReportDefinition ::
  -- | 'reportId'
  Prelude.Text ->
  DeleteReportDefinition
newDeleteReportDefinition pReportId_ =
  DeleteReportDefinition' {reportId = pReportId_}

-- | Required. ID of the report to delete.
deleteReportDefinition_reportId :: Lens.Lens' DeleteReportDefinition Prelude.Text
deleteReportDefinition_reportId = Lens.lens (\DeleteReportDefinition' {reportId} -> reportId) (\s@DeleteReportDefinition' {} a -> s {reportId = a} :: DeleteReportDefinition)

instance Core.AWSRequest DeleteReportDefinition where
  type
    AWSResponse DeleteReportDefinition =
      DeleteReportDefinitionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReportDefinitionResponse'
            Prelude.<$> (x Data..?> "reportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReportDefinition where
  hashWithSalt _salt DeleteReportDefinition' {..} =
    _salt `Prelude.hashWithSalt` reportId

instance Prelude.NFData DeleteReportDefinition where
  rnf DeleteReportDefinition' {..} =
    Prelude.rnf reportId

instance Data.ToHeaders DeleteReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteReportDefinition where
  toPath DeleteReportDefinition' {..} =
    Prelude.mconcat
      ["/reportDefinition/", Data.toBS reportId]

instance Data.ToQuery DeleteReportDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReportDefinitionResponse' smart constructor.
data DeleteReportDefinitionResponse = DeleteReportDefinitionResponse'
  { -- | ID of the report that was deleted.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReportDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'deleteReportDefinitionResponse_reportId' - ID of the report that was deleted.
--
-- 'httpStatus', 'deleteReportDefinitionResponse_httpStatus' - The response's http status code.
newDeleteReportDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReportDefinitionResponse
newDeleteReportDefinitionResponse pHttpStatus_ =
  DeleteReportDefinitionResponse'
    { reportId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ID of the report that was deleted.
deleteReportDefinitionResponse_reportId :: Lens.Lens' DeleteReportDefinitionResponse (Prelude.Maybe Prelude.Text)
deleteReportDefinitionResponse_reportId = Lens.lens (\DeleteReportDefinitionResponse' {reportId} -> reportId) (\s@DeleteReportDefinitionResponse' {} a -> s {reportId = a} :: DeleteReportDefinitionResponse)

-- | The response's http status code.
deleteReportDefinitionResponse_httpStatus :: Lens.Lens' DeleteReportDefinitionResponse Prelude.Int
deleteReportDefinitionResponse_httpStatus = Lens.lens (\DeleteReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteReportDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteReportDefinitionResponse)

instance
  Prelude.NFData
    DeleteReportDefinitionResponse
  where
  rnf DeleteReportDefinitionResponse' {..} =
    Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf httpStatus
