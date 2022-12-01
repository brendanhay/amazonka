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
-- Module      : Amazonka.CostAndUsageReport.PutReportDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new report using the description that you provide.
module Amazonka.CostAndUsageReport.PutReportDefinition
  ( -- * Creating a Request
    PutReportDefinition (..),
    newPutReportDefinition,

    -- * Request Lenses
    putReportDefinition_reportDefinition,

    -- * Destructuring the Response
    PutReportDefinitionResponse (..),
    newPutReportDefinitionResponse,

    -- * Response Lenses
    putReportDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostAndUsageReport.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a Cost and Usage Report.
--
-- /See:/ 'newPutReportDefinition' smart constructor.
data PutReportDefinition = PutReportDefinition'
  { -- | Represents the output of the PutReportDefinition operation. The content
    -- consists of the detailed metadata and data file information.
    reportDefinition :: ReportDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutReportDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportDefinition', 'putReportDefinition_reportDefinition' - Represents the output of the PutReportDefinition operation. The content
-- consists of the detailed metadata and data file information.
newPutReportDefinition ::
  -- | 'reportDefinition'
  ReportDefinition ->
  PutReportDefinition
newPutReportDefinition pReportDefinition_ =
  PutReportDefinition'
    { reportDefinition =
        pReportDefinition_
    }

-- | Represents the output of the PutReportDefinition operation. The content
-- consists of the detailed metadata and data file information.
putReportDefinition_reportDefinition :: Lens.Lens' PutReportDefinition ReportDefinition
putReportDefinition_reportDefinition = Lens.lens (\PutReportDefinition' {reportDefinition} -> reportDefinition) (\s@PutReportDefinition' {} a -> s {reportDefinition = a} :: PutReportDefinition)

instance Core.AWSRequest PutReportDefinition where
  type
    AWSResponse PutReportDefinition =
      PutReportDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutReportDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutReportDefinition where
  hashWithSalt _salt PutReportDefinition' {..} =
    _salt `Prelude.hashWithSalt` reportDefinition

instance Prelude.NFData PutReportDefinition where
  rnf PutReportDefinition' {..} =
    Prelude.rnf reportDefinition

instance Core.ToHeaders PutReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrigamiServiceGatewayService.PutReportDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutReportDefinition where
  toJSON PutReportDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ReportDefinition" Core..= reportDefinition)
          ]
      )

instance Core.ToPath PutReportDefinition where
  toPath = Prelude.const "/"

instance Core.ToQuery PutReportDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
--
-- /See:/ 'newPutReportDefinitionResponse' smart constructor.
data PutReportDefinitionResponse = PutReportDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutReportDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putReportDefinitionResponse_httpStatus' - The response's http status code.
newPutReportDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutReportDefinitionResponse
newPutReportDefinitionResponse pHttpStatus_ =
  PutReportDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putReportDefinitionResponse_httpStatus :: Lens.Lens' PutReportDefinitionResponse Prelude.Int
putReportDefinitionResponse_httpStatus = Lens.lens (\PutReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@PutReportDefinitionResponse' {} a -> s {httpStatus = a} :: PutReportDefinitionResponse)

instance Prelude.NFData PutReportDefinitionResponse where
  rnf PutReportDefinitionResponse' {..} =
    Prelude.rnf httpStatus
