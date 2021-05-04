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

import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes the specified report.
--
-- /See:/ 'newDeleteReportDefinition' smart constructor.
data DeleteReportDefinition = DeleteReportDefinition'
  { -- | The name of the report that you want to delete. The name must be unique,
    -- is case sensitive, and can\'t include spaces.
    reportName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteReportDefinition where
  type
    Rs DeleteReportDefinition =
      DeleteReportDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReportDefinitionResponse'
            Prelude.<$> (x Prelude..?> "ResponseMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReportDefinition

instance Prelude.NFData DeleteReportDefinition

instance Prelude.ToHeaders DeleteReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrigamiServiceGatewayService.DeleteReportDefinition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteReportDefinition where
  toJSON DeleteReportDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("ReportName" Prelude..=) Prelude.<$> reportName]
      )

instance Prelude.ToPath DeleteReportDefinition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteReportDefinition where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
