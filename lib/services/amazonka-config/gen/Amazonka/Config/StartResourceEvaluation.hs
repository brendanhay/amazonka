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
-- Module      : Amazonka.Config.StartResourceEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand evaluation for the specified resource to determine
-- whether the resource details will comply with configured Config rules.
-- You can also use it for evaluation purposes. Config recommends using an
-- evaluation context. It runs an execution against the resource details
-- with all of the Config rules in your account that match with the
-- specified proactive mode and resource type.
--
-- Ensure you have the @cloudformation:DescribeType@ role setup to validate
-- the resource type schema.
module Amazonka.Config.StartResourceEvaluation
  ( -- * Creating a Request
    StartResourceEvaluation (..),
    newStartResourceEvaluation,

    -- * Request Lenses
    startResourceEvaluation_clientToken,
    startResourceEvaluation_evaluationContext,
    startResourceEvaluation_evaluationTimeout,
    startResourceEvaluation_resourceDetails,
    startResourceEvaluation_evaluationMode,

    -- * Destructuring the Response
    StartResourceEvaluationResponse (..),
    newStartResourceEvaluationResponse,

    -- * Response Lenses
    startResourceEvaluationResponse_resourceEvaluationId,
    startResourceEvaluationResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartResourceEvaluation' smart constructor.
data StartResourceEvaluation = StartResourceEvaluation'
  { -- | A client token is a unique, case-sensitive string of up to 64 ASCII
    -- characters. To make an idempotent API request using one of these
    -- actions, specify a client token in the request.
    --
    -- Avoid reusing the same client token for other API requests. If you retry
    -- a request that completed successfully using the same client token and
    -- the same parameters, the retry succeeds without performing any further
    -- actions. If you retry a successful request using the same client token,
    -- but one or more of the parameters are different, other than the Region
    -- or Availability Zone, the retry fails with an
    -- IdempotentParameterMismatch error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Returns an @EvaluationContext@ object.
    evaluationContext :: Prelude.Maybe EvaluationContext,
    -- | The timeout for an evaluation. The default is 900 seconds. You cannot
    -- specify a number greater than 3600. If you specify 0, Config uses the
    -- default.
    evaluationTimeout :: Prelude.Maybe Prelude.Natural,
    -- | Returns a @ResourceDetails@ object.
    resourceDetails :: ResourceDetails,
    -- | The mode of an evaluation. The valid value for this API is @Proactive@.
    evaluationMode :: EvaluationMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartResourceEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startResourceEvaluation_clientToken' - A client token is a unique, case-sensitive string of up to 64 ASCII
-- characters. To make an idempotent API request using one of these
-- actions, specify a client token in the request.
--
-- Avoid reusing the same client token for other API requests. If you retry
-- a request that completed successfully using the same client token and
-- the same parameters, the retry succeeds without performing any further
-- actions. If you retry a successful request using the same client token,
-- but one or more of the parameters are different, other than the Region
-- or Availability Zone, the retry fails with an
-- IdempotentParameterMismatch error.
--
-- 'evaluationContext', 'startResourceEvaluation_evaluationContext' - Returns an @EvaluationContext@ object.
--
-- 'evaluationTimeout', 'startResourceEvaluation_evaluationTimeout' - The timeout for an evaluation. The default is 900 seconds. You cannot
-- specify a number greater than 3600. If you specify 0, Config uses the
-- default.
--
-- 'resourceDetails', 'startResourceEvaluation_resourceDetails' - Returns a @ResourceDetails@ object.
--
-- 'evaluationMode', 'startResourceEvaluation_evaluationMode' - The mode of an evaluation. The valid value for this API is @Proactive@.
newStartResourceEvaluation ::
  -- | 'resourceDetails'
  ResourceDetails ->
  -- | 'evaluationMode'
  EvaluationMode ->
  StartResourceEvaluation
newStartResourceEvaluation
  pResourceDetails_
  pEvaluationMode_ =
    StartResourceEvaluation'
      { clientToken =
          Prelude.Nothing,
        evaluationContext = Prelude.Nothing,
        evaluationTimeout = Prelude.Nothing,
        resourceDetails = pResourceDetails_,
        evaluationMode = pEvaluationMode_
      }

-- | A client token is a unique, case-sensitive string of up to 64 ASCII
-- characters. To make an idempotent API request using one of these
-- actions, specify a client token in the request.
--
-- Avoid reusing the same client token for other API requests. If you retry
-- a request that completed successfully using the same client token and
-- the same parameters, the retry succeeds without performing any further
-- actions. If you retry a successful request using the same client token,
-- but one or more of the parameters are different, other than the Region
-- or Availability Zone, the retry fails with an
-- IdempotentParameterMismatch error.
startResourceEvaluation_clientToken :: Lens.Lens' StartResourceEvaluation (Prelude.Maybe Prelude.Text)
startResourceEvaluation_clientToken = Lens.lens (\StartResourceEvaluation' {clientToken} -> clientToken) (\s@StartResourceEvaluation' {} a -> s {clientToken = a} :: StartResourceEvaluation)

-- | Returns an @EvaluationContext@ object.
startResourceEvaluation_evaluationContext :: Lens.Lens' StartResourceEvaluation (Prelude.Maybe EvaluationContext)
startResourceEvaluation_evaluationContext = Lens.lens (\StartResourceEvaluation' {evaluationContext} -> evaluationContext) (\s@StartResourceEvaluation' {} a -> s {evaluationContext = a} :: StartResourceEvaluation)

-- | The timeout for an evaluation. The default is 900 seconds. You cannot
-- specify a number greater than 3600. If you specify 0, Config uses the
-- default.
startResourceEvaluation_evaluationTimeout :: Lens.Lens' StartResourceEvaluation (Prelude.Maybe Prelude.Natural)
startResourceEvaluation_evaluationTimeout = Lens.lens (\StartResourceEvaluation' {evaluationTimeout} -> evaluationTimeout) (\s@StartResourceEvaluation' {} a -> s {evaluationTimeout = a} :: StartResourceEvaluation)

-- | Returns a @ResourceDetails@ object.
startResourceEvaluation_resourceDetails :: Lens.Lens' StartResourceEvaluation ResourceDetails
startResourceEvaluation_resourceDetails = Lens.lens (\StartResourceEvaluation' {resourceDetails} -> resourceDetails) (\s@StartResourceEvaluation' {} a -> s {resourceDetails = a} :: StartResourceEvaluation)

-- | The mode of an evaluation. The valid value for this API is @Proactive@.
startResourceEvaluation_evaluationMode :: Lens.Lens' StartResourceEvaluation EvaluationMode
startResourceEvaluation_evaluationMode = Lens.lens (\StartResourceEvaluation' {evaluationMode} -> evaluationMode) (\s@StartResourceEvaluation' {} a -> s {evaluationMode = a} :: StartResourceEvaluation)

instance Core.AWSRequest StartResourceEvaluation where
  type
    AWSResponse StartResourceEvaluation =
      StartResourceEvaluationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartResourceEvaluationResponse'
            Prelude.<$> (x Data..?> "ResourceEvaluationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartResourceEvaluation where
  hashWithSalt _salt StartResourceEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` evaluationContext
      `Prelude.hashWithSalt` evaluationTimeout
      `Prelude.hashWithSalt` resourceDetails
      `Prelude.hashWithSalt` evaluationMode

instance Prelude.NFData StartResourceEvaluation where
  rnf StartResourceEvaluation' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf evaluationContext `Prelude.seq`
        Prelude.rnf evaluationTimeout `Prelude.seq`
          Prelude.rnf resourceDetails `Prelude.seq`
            Prelude.rnf evaluationMode

instance Data.ToHeaders StartResourceEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.StartResourceEvaluation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartResourceEvaluation where
  toJSON StartResourceEvaluation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("EvaluationContext" Data..=)
              Prelude.<$> evaluationContext,
            ("EvaluationTimeout" Data..=)
              Prelude.<$> evaluationTimeout,
            Prelude.Just
              ("ResourceDetails" Data..= resourceDetails),
            Prelude.Just
              ("EvaluationMode" Data..= evaluationMode)
          ]
      )

instance Data.ToPath StartResourceEvaluation where
  toPath = Prelude.const "/"

instance Data.ToQuery StartResourceEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartResourceEvaluationResponse' smart constructor.
data StartResourceEvaluationResponse = StartResourceEvaluationResponse'
  { -- | A unique ResourceEvaluationId that is associated with a single
    -- execution.
    resourceEvaluationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartResourceEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceEvaluationId', 'startResourceEvaluationResponse_resourceEvaluationId' - A unique ResourceEvaluationId that is associated with a single
-- execution.
--
-- 'httpStatus', 'startResourceEvaluationResponse_httpStatus' - The response's http status code.
newStartResourceEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartResourceEvaluationResponse
newStartResourceEvaluationResponse pHttpStatus_ =
  StartResourceEvaluationResponse'
    { resourceEvaluationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique ResourceEvaluationId that is associated with a single
-- execution.
startResourceEvaluationResponse_resourceEvaluationId :: Lens.Lens' StartResourceEvaluationResponse (Prelude.Maybe Prelude.Text)
startResourceEvaluationResponse_resourceEvaluationId = Lens.lens (\StartResourceEvaluationResponse' {resourceEvaluationId} -> resourceEvaluationId) (\s@StartResourceEvaluationResponse' {} a -> s {resourceEvaluationId = a} :: StartResourceEvaluationResponse)

-- | The response's http status code.
startResourceEvaluationResponse_httpStatus :: Lens.Lens' StartResourceEvaluationResponse Prelude.Int
startResourceEvaluationResponse_httpStatus = Lens.lens (\StartResourceEvaluationResponse' {httpStatus} -> httpStatus) (\s@StartResourceEvaluationResponse' {} a -> s {httpStatus = a} :: StartResourceEvaluationResponse)

instance
  Prelude.NFData
    StartResourceEvaluationResponse
  where
  rnf StartResourceEvaluationResponse' {..} =
    Prelude.rnf resourceEvaluationId `Prelude.seq`
      Prelude.rnf httpStatus
