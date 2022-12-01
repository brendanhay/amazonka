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
-- Module      : Amazonka.Config.PutEvaluations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by an Lambda function to deliver evaluation results to Config. This
-- action is required in every Lambda function that is invoked by an Config
-- rule.
module Amazonka.Config.PutEvaluations
  ( -- * Creating a Request
    PutEvaluations (..),
    newPutEvaluations,

    -- * Request Lenses
    putEvaluations_testMode,
    putEvaluations_evaluations,
    putEvaluations_resultToken,

    -- * Destructuring the Response
    PutEvaluationsResponse (..),
    newPutEvaluationsResponse,

    -- * Response Lenses
    putEvaluationsResponse_failedEvaluations,
    putEvaluationsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newPutEvaluations' smart constructor.
data PutEvaluations = PutEvaluations'
  { -- | Use this parameter to specify a test run for @PutEvaluations@. You can
    -- verify whether your Lambda function will deliver evaluation results to
    -- Config. No updates occur to your existing evaluations, and evaluation
    -- results are not sent to Config.
    --
    -- When @TestMode@ is @true@, @PutEvaluations@ doesn\'t require a valid
    -- value for the @ResultToken@ parameter, but the value cannot be null.
    testMode :: Prelude.Maybe Prelude.Bool,
    -- | The assessments that the Lambda function performs. Each evaluation
    -- identifies an Amazon Web Services resource and indicates whether it
    -- complies with the Config rule that invokes the Lambda function.
    evaluations :: Prelude.Maybe [Evaluation],
    -- | An encrypted token that associates an evaluation with an Config rule.
    -- Identifies the rule and the event that triggered the evaluation.
    resultToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEvaluations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testMode', 'putEvaluations_testMode' - Use this parameter to specify a test run for @PutEvaluations@. You can
-- verify whether your Lambda function will deliver evaluation results to
-- Config. No updates occur to your existing evaluations, and evaluation
-- results are not sent to Config.
--
-- When @TestMode@ is @true@, @PutEvaluations@ doesn\'t require a valid
-- value for the @ResultToken@ parameter, but the value cannot be null.
--
-- 'evaluations', 'putEvaluations_evaluations' - The assessments that the Lambda function performs. Each evaluation
-- identifies an Amazon Web Services resource and indicates whether it
-- complies with the Config rule that invokes the Lambda function.
--
-- 'resultToken', 'putEvaluations_resultToken' - An encrypted token that associates an evaluation with an Config rule.
-- Identifies the rule and the event that triggered the evaluation.
newPutEvaluations ::
  -- | 'resultToken'
  Prelude.Text ->
  PutEvaluations
newPutEvaluations pResultToken_ =
  PutEvaluations'
    { testMode = Prelude.Nothing,
      evaluations = Prelude.Nothing,
      resultToken = pResultToken_
    }

-- | Use this parameter to specify a test run for @PutEvaluations@. You can
-- verify whether your Lambda function will deliver evaluation results to
-- Config. No updates occur to your existing evaluations, and evaluation
-- results are not sent to Config.
--
-- When @TestMode@ is @true@, @PutEvaluations@ doesn\'t require a valid
-- value for the @ResultToken@ parameter, but the value cannot be null.
putEvaluations_testMode :: Lens.Lens' PutEvaluations (Prelude.Maybe Prelude.Bool)
putEvaluations_testMode = Lens.lens (\PutEvaluations' {testMode} -> testMode) (\s@PutEvaluations' {} a -> s {testMode = a} :: PutEvaluations)

-- | The assessments that the Lambda function performs. Each evaluation
-- identifies an Amazon Web Services resource and indicates whether it
-- complies with the Config rule that invokes the Lambda function.
putEvaluations_evaluations :: Lens.Lens' PutEvaluations (Prelude.Maybe [Evaluation])
putEvaluations_evaluations = Lens.lens (\PutEvaluations' {evaluations} -> evaluations) (\s@PutEvaluations' {} a -> s {evaluations = a} :: PutEvaluations) Prelude.. Lens.mapping Lens.coerced

-- | An encrypted token that associates an evaluation with an Config rule.
-- Identifies the rule and the event that triggered the evaluation.
putEvaluations_resultToken :: Lens.Lens' PutEvaluations Prelude.Text
putEvaluations_resultToken = Lens.lens (\PutEvaluations' {resultToken} -> resultToken) (\s@PutEvaluations' {} a -> s {resultToken = a} :: PutEvaluations)

instance Core.AWSRequest PutEvaluations where
  type
    AWSResponse PutEvaluations =
      PutEvaluationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEvaluationsResponse'
            Prelude.<$> ( x Core..?> "FailedEvaluations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutEvaluations where
  hashWithSalt _salt PutEvaluations' {..} =
    _salt `Prelude.hashWithSalt` testMode
      `Prelude.hashWithSalt` evaluations
      `Prelude.hashWithSalt` resultToken

instance Prelude.NFData PutEvaluations where
  rnf PutEvaluations' {..} =
    Prelude.rnf testMode
      `Prelude.seq` Prelude.rnf evaluations
      `Prelude.seq` Prelude.rnf resultToken

instance Core.ToHeaders PutEvaluations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutEvaluations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutEvaluations where
  toJSON PutEvaluations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TestMode" Core..=) Prelude.<$> testMode,
            ("Evaluations" Core..=) Prelude.<$> evaluations,
            Prelude.Just ("ResultToken" Core..= resultToken)
          ]
      )

instance Core.ToPath PutEvaluations where
  toPath = Prelude.const "/"

instance Core.ToQuery PutEvaluations where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newPutEvaluationsResponse' smart constructor.
data PutEvaluationsResponse = PutEvaluationsResponse'
  { -- | Requests that failed because of a client or server error.
    failedEvaluations :: Prelude.Maybe [Evaluation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEvaluationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEvaluations', 'putEvaluationsResponse_failedEvaluations' - Requests that failed because of a client or server error.
--
-- 'httpStatus', 'putEvaluationsResponse_httpStatus' - The response's http status code.
newPutEvaluationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEvaluationsResponse
newPutEvaluationsResponse pHttpStatus_ =
  PutEvaluationsResponse'
    { failedEvaluations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Requests that failed because of a client or server error.
putEvaluationsResponse_failedEvaluations :: Lens.Lens' PutEvaluationsResponse (Prelude.Maybe [Evaluation])
putEvaluationsResponse_failedEvaluations = Lens.lens (\PutEvaluationsResponse' {failedEvaluations} -> failedEvaluations) (\s@PutEvaluationsResponse' {} a -> s {failedEvaluations = a} :: PutEvaluationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putEvaluationsResponse_httpStatus :: Lens.Lens' PutEvaluationsResponse Prelude.Int
putEvaluationsResponse_httpStatus = Lens.lens (\PutEvaluationsResponse' {httpStatus} -> httpStatus) (\s@PutEvaluationsResponse' {} a -> s {httpStatus = a} :: PutEvaluationsResponse)

instance Prelude.NFData PutEvaluationsResponse where
  rnf PutEvaluationsResponse' {..} =
    Prelude.rnf failedEvaluations
      `Prelude.seq` Prelude.rnf httpStatus
