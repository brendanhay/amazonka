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
-- Module      : Amazonka.CostAndUsageReport.ModifyReportDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to programatically update your report preferences.
module Amazonka.CostAndUsageReport.ModifyReportDefinition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostAndUsageReport.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyReportDefinition' smart constructor.
data ModifyReportDefinition = ModifyReportDefinition'
  { reportName :: Prelude.Text,
    reportDefinition :: ReportDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest ModifyReportDefinition where
  type
    AWSResponse ModifyReportDefinition =
      ModifyReportDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyReportDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyReportDefinition where
  hashWithSalt _salt ModifyReportDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` reportName
      `Prelude.hashWithSalt` reportDefinition

instance Prelude.NFData ModifyReportDefinition where
  rnf ModifyReportDefinition' {..} =
    Prelude.rnf reportName
      `Prelude.seq` Prelude.rnf reportDefinition

instance Data.ToHeaders ModifyReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrigamiServiceGatewayService.ModifyReportDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyReportDefinition where
  toJSON ModifyReportDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ReportName" Data..= reportName),
            Prelude.Just
              ("ReportDefinition" Data..= reportDefinition)
          ]
      )

instance Data.ToPath ModifyReportDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyReportDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyReportDefinitionResponse' smart constructor.
data ModifyReportDefinitionResponse = ModifyReportDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf ModifyReportDefinitionResponse' {..} =
    Prelude.rnf httpStatus
