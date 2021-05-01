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

import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyReportDefinition' smart constructor.
data ModifyReportDefinition = ModifyReportDefinition'
  { reportName :: Prelude.Text,
    reportDefinition :: ReportDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
modifyReportDefinition_reportName :: Lens.Lens' ModifyReportDefinition Prelude.Text
modifyReportDefinition_reportName = Lens.lens (\ModifyReportDefinition' {reportName} -> reportName) (\s@ModifyReportDefinition' {} a -> s {reportName = a} :: ModifyReportDefinition)

-- | Undocumented member.
modifyReportDefinition_reportDefinition :: Lens.Lens' ModifyReportDefinition ReportDefinition
modifyReportDefinition_reportDefinition = Lens.lens (\ModifyReportDefinition' {reportDefinition} -> reportDefinition) (\s@ModifyReportDefinition' {} a -> s {reportDefinition = a} :: ModifyReportDefinition)

instance Prelude.AWSRequest ModifyReportDefinition where
  type
    Rs ModifyReportDefinition =
      ModifyReportDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyReportDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyReportDefinition

instance Prelude.NFData ModifyReportDefinition

instance Prelude.ToHeaders ModifyReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrigamiServiceGatewayService.ModifyReportDefinition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ModifyReportDefinition where
  toJSON ModifyReportDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ReportName" Prelude..= reportName),
            Prelude.Just
              ("ReportDefinition" Prelude..= reportDefinition)
          ]
      )

instance Prelude.ToPath ModifyReportDefinition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyReportDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyReportDefinitionResponse' smart constructor.
data ModifyReportDefinitionResponse = ModifyReportDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ModifyReportDefinitionResponse
newModifyReportDefinitionResponse pHttpStatus_ =
  ModifyReportDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifyReportDefinitionResponse_httpStatus :: Lens.Lens' ModifyReportDefinitionResponse Prelude.Int
modifyReportDefinitionResponse_httpStatus = Lens.lens (\ModifyReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@ModifyReportDefinitionResponse' {} a -> s {httpStatus = a} :: ModifyReportDefinitionResponse)

instance
  Prelude.NFData
    ModifyReportDefinitionResponse
