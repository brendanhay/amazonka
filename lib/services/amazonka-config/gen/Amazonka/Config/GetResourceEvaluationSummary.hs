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
-- Module      : Amazonka.Config.GetResourceEvaluationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a summary of resource evaluation for the specified resource
-- evaluation ID from the proactive rules that were run. The results
-- indicate which evaluation context was used to evaluate the rules, which
-- resource details were evaluated, the evaluation mode that was run, and
-- whether the resource details comply with the configuration of the
-- proactive rules.
module Amazonka.Config.GetResourceEvaluationSummary
  ( -- * Creating a Request
    GetResourceEvaluationSummary (..),
    newGetResourceEvaluationSummary,

    -- * Request Lenses
    getResourceEvaluationSummary_resourceEvaluationId,

    -- * Destructuring the Response
    GetResourceEvaluationSummaryResponse (..),
    newGetResourceEvaluationSummaryResponse,

    -- * Response Lenses
    getResourceEvaluationSummaryResponse_compliance,
    getResourceEvaluationSummaryResponse_evaluationContext,
    getResourceEvaluationSummaryResponse_evaluationMode,
    getResourceEvaluationSummaryResponse_evaluationStartTimestamp,
    getResourceEvaluationSummaryResponse_evaluationStatus,
    getResourceEvaluationSummaryResponse_resourceDetails,
    getResourceEvaluationSummaryResponse_resourceEvaluationId,
    getResourceEvaluationSummaryResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceEvaluationSummary' smart constructor.
data GetResourceEvaluationSummary = GetResourceEvaluationSummary'
  { -- | The unique @ResourceEvaluationId@ of Amazon Web Services resource
    -- execution for which you want to retrieve the evaluation summary.
    resourceEvaluationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceEvaluationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceEvaluationId', 'getResourceEvaluationSummary_resourceEvaluationId' - The unique @ResourceEvaluationId@ of Amazon Web Services resource
-- execution for which you want to retrieve the evaluation summary.
newGetResourceEvaluationSummary ::
  -- | 'resourceEvaluationId'
  Prelude.Text ->
  GetResourceEvaluationSummary
newGetResourceEvaluationSummary
  pResourceEvaluationId_ =
    GetResourceEvaluationSummary'
      { resourceEvaluationId =
          pResourceEvaluationId_
      }

-- | The unique @ResourceEvaluationId@ of Amazon Web Services resource
-- execution for which you want to retrieve the evaluation summary.
getResourceEvaluationSummary_resourceEvaluationId :: Lens.Lens' GetResourceEvaluationSummary Prelude.Text
getResourceEvaluationSummary_resourceEvaluationId = Lens.lens (\GetResourceEvaluationSummary' {resourceEvaluationId} -> resourceEvaluationId) (\s@GetResourceEvaluationSummary' {} a -> s {resourceEvaluationId = a} :: GetResourceEvaluationSummary)

instance Core.AWSRequest GetResourceEvaluationSummary where
  type
    AWSResponse GetResourceEvaluationSummary =
      GetResourceEvaluationSummaryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceEvaluationSummaryResponse'
            Prelude.<$> (x Data..?> "Compliance")
            Prelude.<*> (x Data..?> "EvaluationContext")
            Prelude.<*> (x Data..?> "EvaluationMode")
            Prelude.<*> (x Data..?> "EvaluationStartTimestamp")
            Prelude.<*> (x Data..?> "EvaluationStatus")
            Prelude.<*> (x Data..?> "ResourceDetails")
            Prelude.<*> (x Data..?> "ResourceEvaluationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetResourceEvaluationSummary
  where
  hashWithSalt _salt GetResourceEvaluationSummary' {..} =
    _salt `Prelude.hashWithSalt` resourceEvaluationId

instance Prelude.NFData GetResourceEvaluationSummary where
  rnf GetResourceEvaluationSummary' {..} =
    Prelude.rnf resourceEvaluationId

instance Data.ToHeaders GetResourceEvaluationSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetResourceEvaluationSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourceEvaluationSummary where
  toJSON GetResourceEvaluationSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResourceEvaluationId"
                  Data..= resourceEvaluationId
              )
          ]
      )

instance Data.ToPath GetResourceEvaluationSummary where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourceEvaluationSummary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceEvaluationSummaryResponse' smart constructor.
data GetResourceEvaluationSummaryResponse = GetResourceEvaluationSummaryResponse'
  { -- | The compliance status of the resource evaluation summary.
    compliance :: Prelude.Maybe ComplianceType,
    -- | Returns an @EvaluationContext@ object.
    evaluationContext :: Prelude.Maybe EvaluationContext,
    -- | Lists results of the mode that you requested to retrieve the resource
    -- evaluation summary. The valid values are Detective or Proactive.
    evaluationMode :: Prelude.Maybe EvaluationMode,
    -- | The start timestamp when Config rule starts evaluating compliance for
    -- the provided resource details.
    evaluationStartTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Returns an @EvaluationStatus@ object.
    evaluationStatus :: Prelude.Maybe EvaluationStatus,
    -- | Returns a @ResourceDetails@ object.
    resourceDetails :: Prelude.Maybe ResourceDetails,
    -- | The unique @ResourceEvaluationId@ of Amazon Web Services resource
    -- execution for which you want to retrieve the evaluation summary.
    resourceEvaluationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceEvaluationSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compliance', 'getResourceEvaluationSummaryResponse_compliance' - The compliance status of the resource evaluation summary.
--
-- 'evaluationContext', 'getResourceEvaluationSummaryResponse_evaluationContext' - Returns an @EvaluationContext@ object.
--
-- 'evaluationMode', 'getResourceEvaluationSummaryResponse_evaluationMode' - Lists results of the mode that you requested to retrieve the resource
-- evaluation summary. The valid values are Detective or Proactive.
--
-- 'evaluationStartTimestamp', 'getResourceEvaluationSummaryResponse_evaluationStartTimestamp' - The start timestamp when Config rule starts evaluating compliance for
-- the provided resource details.
--
-- 'evaluationStatus', 'getResourceEvaluationSummaryResponse_evaluationStatus' - Returns an @EvaluationStatus@ object.
--
-- 'resourceDetails', 'getResourceEvaluationSummaryResponse_resourceDetails' - Returns a @ResourceDetails@ object.
--
-- 'resourceEvaluationId', 'getResourceEvaluationSummaryResponse_resourceEvaluationId' - The unique @ResourceEvaluationId@ of Amazon Web Services resource
-- execution for which you want to retrieve the evaluation summary.
--
-- 'httpStatus', 'getResourceEvaluationSummaryResponse_httpStatus' - The response's http status code.
newGetResourceEvaluationSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceEvaluationSummaryResponse
newGetResourceEvaluationSummaryResponse pHttpStatus_ =
  GetResourceEvaluationSummaryResponse'
    { compliance =
        Prelude.Nothing,
      evaluationContext = Prelude.Nothing,
      evaluationMode = Prelude.Nothing,
      evaluationStartTimestamp =
        Prelude.Nothing,
      evaluationStatus = Prelude.Nothing,
      resourceDetails = Prelude.Nothing,
      resourceEvaluationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The compliance status of the resource evaluation summary.
getResourceEvaluationSummaryResponse_compliance :: Lens.Lens' GetResourceEvaluationSummaryResponse (Prelude.Maybe ComplianceType)
getResourceEvaluationSummaryResponse_compliance = Lens.lens (\GetResourceEvaluationSummaryResponse' {compliance} -> compliance) (\s@GetResourceEvaluationSummaryResponse' {} a -> s {compliance = a} :: GetResourceEvaluationSummaryResponse)

-- | Returns an @EvaluationContext@ object.
getResourceEvaluationSummaryResponse_evaluationContext :: Lens.Lens' GetResourceEvaluationSummaryResponse (Prelude.Maybe EvaluationContext)
getResourceEvaluationSummaryResponse_evaluationContext = Lens.lens (\GetResourceEvaluationSummaryResponse' {evaluationContext} -> evaluationContext) (\s@GetResourceEvaluationSummaryResponse' {} a -> s {evaluationContext = a} :: GetResourceEvaluationSummaryResponse)

-- | Lists results of the mode that you requested to retrieve the resource
-- evaluation summary. The valid values are Detective or Proactive.
getResourceEvaluationSummaryResponse_evaluationMode :: Lens.Lens' GetResourceEvaluationSummaryResponse (Prelude.Maybe EvaluationMode)
getResourceEvaluationSummaryResponse_evaluationMode = Lens.lens (\GetResourceEvaluationSummaryResponse' {evaluationMode} -> evaluationMode) (\s@GetResourceEvaluationSummaryResponse' {} a -> s {evaluationMode = a} :: GetResourceEvaluationSummaryResponse)

-- | The start timestamp when Config rule starts evaluating compliance for
-- the provided resource details.
getResourceEvaluationSummaryResponse_evaluationStartTimestamp :: Lens.Lens' GetResourceEvaluationSummaryResponse (Prelude.Maybe Prelude.UTCTime)
getResourceEvaluationSummaryResponse_evaluationStartTimestamp = Lens.lens (\GetResourceEvaluationSummaryResponse' {evaluationStartTimestamp} -> evaluationStartTimestamp) (\s@GetResourceEvaluationSummaryResponse' {} a -> s {evaluationStartTimestamp = a} :: GetResourceEvaluationSummaryResponse) Prelude.. Lens.mapping Data._Time

-- | Returns an @EvaluationStatus@ object.
getResourceEvaluationSummaryResponse_evaluationStatus :: Lens.Lens' GetResourceEvaluationSummaryResponse (Prelude.Maybe EvaluationStatus)
getResourceEvaluationSummaryResponse_evaluationStatus = Lens.lens (\GetResourceEvaluationSummaryResponse' {evaluationStatus} -> evaluationStatus) (\s@GetResourceEvaluationSummaryResponse' {} a -> s {evaluationStatus = a} :: GetResourceEvaluationSummaryResponse)

-- | Returns a @ResourceDetails@ object.
getResourceEvaluationSummaryResponse_resourceDetails :: Lens.Lens' GetResourceEvaluationSummaryResponse (Prelude.Maybe ResourceDetails)
getResourceEvaluationSummaryResponse_resourceDetails = Lens.lens (\GetResourceEvaluationSummaryResponse' {resourceDetails} -> resourceDetails) (\s@GetResourceEvaluationSummaryResponse' {} a -> s {resourceDetails = a} :: GetResourceEvaluationSummaryResponse)

-- | The unique @ResourceEvaluationId@ of Amazon Web Services resource
-- execution for which you want to retrieve the evaluation summary.
getResourceEvaluationSummaryResponse_resourceEvaluationId :: Lens.Lens' GetResourceEvaluationSummaryResponse (Prelude.Maybe Prelude.Text)
getResourceEvaluationSummaryResponse_resourceEvaluationId = Lens.lens (\GetResourceEvaluationSummaryResponse' {resourceEvaluationId} -> resourceEvaluationId) (\s@GetResourceEvaluationSummaryResponse' {} a -> s {resourceEvaluationId = a} :: GetResourceEvaluationSummaryResponse)

-- | The response's http status code.
getResourceEvaluationSummaryResponse_httpStatus :: Lens.Lens' GetResourceEvaluationSummaryResponse Prelude.Int
getResourceEvaluationSummaryResponse_httpStatus = Lens.lens (\GetResourceEvaluationSummaryResponse' {httpStatus} -> httpStatus) (\s@GetResourceEvaluationSummaryResponse' {} a -> s {httpStatus = a} :: GetResourceEvaluationSummaryResponse)

instance
  Prelude.NFData
    GetResourceEvaluationSummaryResponse
  where
  rnf GetResourceEvaluationSummaryResponse' {..} =
    Prelude.rnf compliance
      `Prelude.seq` Prelude.rnf evaluationContext
      `Prelude.seq` Prelude.rnf evaluationMode
      `Prelude.seq` Prelude.rnf evaluationStartTimestamp
      `Prelude.seq` Prelude.rnf evaluationStatus
      `Prelude.seq` Prelude.rnf resourceDetails
      `Prelude.seq` Prelude.rnf resourceEvaluationId
      `Prelude.seq` Prelude.rnf httpStatus
