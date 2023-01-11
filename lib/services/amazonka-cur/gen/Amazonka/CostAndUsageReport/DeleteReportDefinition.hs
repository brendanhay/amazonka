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
-- Module      : Amazonka.CostAndUsageReport.DeleteReportDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified report.
module Amazonka.CostAndUsageReport.DeleteReportDefinition
  ( -- * Creating a Request
    DeleteReportDefinition (..),
    newDeleteReportDefinition,

    -- * Request Lenses
    deleteReportDefinition_reportName,

    -- * Destructuring the Response
    DeleteReportDefinitionResponse (..),
    newDeleteReportDefinitionResponse,

    -- * Response Lenses
    deleteReportDefinitionResponse_responseMessage,
    deleteReportDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostAndUsageReport.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes the specified report.
--
-- /See:/ 'newDeleteReportDefinition' smart constructor.
data DeleteReportDefinition = DeleteReportDefinition'
  { -- | The name of the report that you want to delete. The name must be unique,
    -- is case sensitive, and can\'t include spaces.
    reportName :: Prelude.Maybe Prelude.Text
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
-- 'reportName', 'deleteReportDefinition_reportName' - The name of the report that you want to delete. The name must be unique,
-- is case sensitive, and can\'t include spaces.
newDeleteReportDefinition ::
  DeleteReportDefinition
newDeleteReportDefinition =
  DeleteReportDefinition'
    { reportName =
        Prelude.Nothing
    }

-- | The name of the report that you want to delete. The name must be unique,
-- is case sensitive, and can\'t include spaces.
deleteReportDefinition_reportName :: Lens.Lens' DeleteReportDefinition (Prelude.Maybe Prelude.Text)
deleteReportDefinition_reportName = Lens.lens (\DeleteReportDefinition' {reportName} -> reportName) (\s@DeleteReportDefinition' {} a -> s {reportName = a} :: DeleteReportDefinition)

instance Core.AWSRequest DeleteReportDefinition where
  type
    AWSResponse DeleteReportDefinition =
      DeleteReportDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReportDefinitionResponse'
            Prelude.<$> (x Data..?> "ResponseMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReportDefinition where
  hashWithSalt _salt DeleteReportDefinition' {..} =
    _salt `Prelude.hashWithSalt` reportName

instance Prelude.NFData DeleteReportDefinition where
  rnf DeleteReportDefinition' {..} =
    Prelude.rnf reportName

instance Data.ToHeaders DeleteReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrigamiServiceGatewayService.DeleteReportDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteReportDefinition where
  toJSON DeleteReportDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ReportName" Data..=) Prelude.<$> reportName]
      )

instance Data.ToPath DeleteReportDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteReportDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200
-- response.
--
-- /See:/ 'newDeleteReportDefinitionResponse' smart constructor.
data DeleteReportDefinitionResponse = DeleteReportDefinitionResponse'
  { responseMessage :: Prelude.Maybe Prelude.Text,
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
-- 'responseMessage', 'deleteReportDefinitionResponse_responseMessage' - Undocumented member.
--
-- 'httpStatus', 'deleteReportDefinitionResponse_httpStatus' - The response's http status code.
newDeleteReportDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReportDefinitionResponse
newDeleteReportDefinitionResponse pHttpStatus_ =
  DeleteReportDefinitionResponse'
    { responseMessage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteReportDefinitionResponse_responseMessage :: Lens.Lens' DeleteReportDefinitionResponse (Prelude.Maybe Prelude.Text)
deleteReportDefinitionResponse_responseMessage = Lens.lens (\DeleteReportDefinitionResponse' {responseMessage} -> responseMessage) (\s@DeleteReportDefinitionResponse' {} a -> s {responseMessage = a} :: DeleteReportDefinitionResponse)

-- | The response's http status code.
deleteReportDefinitionResponse_httpStatus :: Lens.Lens' DeleteReportDefinitionResponse Prelude.Int
deleteReportDefinitionResponse_httpStatus = Lens.lens (\DeleteReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteReportDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteReportDefinitionResponse)

instance
  Prelude.NFData
    DeleteReportDefinitionResponse
  where
  rnf DeleteReportDefinitionResponse' {..} =
    Prelude.rnf responseMessage
      `Prelude.seq` Prelude.rnf httpStatus
