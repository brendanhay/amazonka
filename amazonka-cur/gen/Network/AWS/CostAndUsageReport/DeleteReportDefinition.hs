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
-- Module      : Network.AWS.CostAndUsageReport.DeleteReportDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified report.
module Network.AWS.CostAndUsageReport.DeleteReportDefinition
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

import qualified Network.AWS.Core as Core
import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes the specified report.
--
-- /See:/ 'newDeleteReportDefinition' smart constructor.
data DeleteReportDefinition = DeleteReportDefinition'
  { -- | The name of the report that you want to delete. The name must be unique,
    -- is case sensitive, and can\'t include spaces.
    reportName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  DeleteReportDefinition' {reportName = Core.Nothing}

-- | The name of the report that you want to delete. The name must be unique,
-- is case sensitive, and can\'t include spaces.
deleteReportDefinition_reportName :: Lens.Lens' DeleteReportDefinition (Core.Maybe Core.Text)
deleteReportDefinition_reportName = Lens.lens (\DeleteReportDefinition' {reportName} -> reportName) (\s@DeleteReportDefinition' {} a -> s {reportName = a} :: DeleteReportDefinition)

instance Core.AWSRequest DeleteReportDefinition where
  type
    AWSResponse DeleteReportDefinition =
      DeleteReportDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReportDefinitionResponse'
            Core.<$> (x Core..?> "ResponseMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteReportDefinition

instance Core.NFData DeleteReportDefinition

instance Core.ToHeaders DeleteReportDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrigamiServiceGatewayService.DeleteReportDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteReportDefinition where
  toJSON DeleteReportDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [("ReportName" Core..=) Core.<$> reportName]
      )

instance Core.ToPath DeleteReportDefinition where
  toPath = Core.const "/"

instance Core.ToQuery DeleteReportDefinition where
  toQuery = Core.const Core.mempty

-- | If the action is successful, the service sends back an HTTP 200
-- response.
--
-- /See:/ 'newDeleteReportDefinitionResponse' smart constructor.
data DeleteReportDefinitionResponse = DeleteReportDefinitionResponse'
  { responseMessage :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteReportDefinitionResponse
newDeleteReportDefinitionResponse pHttpStatus_ =
  DeleteReportDefinitionResponse'
    { responseMessage =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteReportDefinitionResponse_responseMessage :: Lens.Lens' DeleteReportDefinitionResponse (Core.Maybe Core.Text)
deleteReportDefinitionResponse_responseMessage = Lens.lens (\DeleteReportDefinitionResponse' {responseMessage} -> responseMessage) (\s@DeleteReportDefinitionResponse' {} a -> s {responseMessage = a} :: DeleteReportDefinitionResponse)

-- | The response's http status code.
deleteReportDefinitionResponse_httpStatus :: Lens.Lens' DeleteReportDefinitionResponse Core.Int
deleteReportDefinitionResponse_httpStatus = Lens.lens (\DeleteReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteReportDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteReportDefinitionResponse)

instance Core.NFData DeleteReportDefinitionResponse
