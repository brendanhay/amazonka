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
-- Module      : Network.AWS.CostAndUsageReport.ModifyReportDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to programatically update your report preferences.
module Network.AWS.CostAndUsageReport.ModifyReportDefinition
  ( -- * Creating a Request
    ModifyReportDefinition (..),
    newModifyReportDefinition,

    -- * Request Lenses
    modifyReportDefinition_reportName,
    modifyReportDefinition_reportDefinition,

    -- * Destructuring the Response
    ModifyReportDefinitionResponse (..),
    newModifyReportDefinitionResponse,

    -- * Response Lenses
    modifyReportDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyReportDefinition' smart constructor.
data ModifyReportDefinition = ModifyReportDefinition'
  { reportName :: Core.Text,
    reportDefinition :: ReportDefinition
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyReportDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportName', 'modifyReportDefinition_reportName' - Undocumented member.
--
-- 'reportDefinition', 'modifyReportDefinition_reportDefinition' - Undocumented member.
newModifyReportDefinition ::
  -- | 'reportName'
  Core.Text ->
  -- | 'reportDefinition'
  ReportDefinition ->
  ModifyReportDefinition
newModifyReportDefinition
  pReportName_
  pReportDefinition_ =
    ModifyReportDefinition'
      { reportName = pReportName_,
        reportDefinition = pReportDefinition_
      }

-- | Undocumented member.
modifyReportDefinition_reportName :: Lens.Lens' ModifyReportDefinition Core.Text
modifyReportDefinition_reportName = Lens.lens (\ModifyReportDefinition' {reportName} -> reportName) (\s@ModifyReportDefinition' {} a -> s {reportName = a} :: ModifyReportDefinition)

-- | Undocumented member.
modifyReportDefinition_reportDefinition :: Lens.Lens' ModifyReportDefinition ReportDefinition
modifyReportDefinition_reportDefinition = Lens.lens (\ModifyReportDefinition' {reportDefinition} -> reportDefinition) (\s@ModifyReportDefinition' {} a -> s {reportDefinition = a} :: ModifyReportDefinition)

instance Core.AWSRequest ModifyReportDefinition where
  type
    AWSResponse ModifyReportDefinition =
      ModifyReportDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyReportDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyReportDefinition

instance Core.NFData ModifyReportDefinition

instance Core.ToHeaders ModifyReportDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrigamiServiceGatewayService.ModifyReportDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyReportDefinition where
  toJSON ModifyReportDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReportName" Core..= reportName),
            Core.Just
              ("ReportDefinition" Core..= reportDefinition)
          ]
      )

instance Core.ToPath ModifyReportDefinition where
  toPath = Core.const "/"

instance Core.ToQuery ModifyReportDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyReportDefinitionResponse' smart constructor.
data ModifyReportDefinitionResponse = ModifyReportDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyReportDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyReportDefinitionResponse_httpStatus' - The response's http status code.
newModifyReportDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyReportDefinitionResponse
newModifyReportDefinitionResponse pHttpStatus_ =
  ModifyReportDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifyReportDefinitionResponse_httpStatus :: Lens.Lens' ModifyReportDefinitionResponse Core.Int
modifyReportDefinitionResponse_httpStatus = Lens.lens (\ModifyReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@ModifyReportDefinitionResponse' {} a -> s {httpStatus = a} :: ModifyReportDefinitionResponse)

instance Core.NFData ModifyReportDefinitionResponse
