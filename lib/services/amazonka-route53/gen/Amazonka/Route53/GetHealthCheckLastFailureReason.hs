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
-- Module      : Amazonka.Route53.GetHealthCheckLastFailureReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the reason that a specified health check failed most recently.
module Amazonka.Route53.GetHealthCheckLastFailureReason
  ( -- * Creating a Request
    GetHealthCheckLastFailureReason (..),
    newGetHealthCheckLastFailureReason,

    -- * Request Lenses
    getHealthCheckLastFailureReason_healthCheckId,

    -- * Destructuring the Response
    GetHealthCheckLastFailureReasonResponse (..),
    newGetHealthCheckLastFailureReasonResponse,

    -- * Response Lenses
    getHealthCheckLastFailureReasonResponse_httpStatus,
    getHealthCheckLastFailureReasonResponse_healthCheckObservations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request for the reason that a health check failed most recently.
--
-- /See:/ 'newGetHealthCheckLastFailureReason' smart constructor.
data GetHealthCheckLastFailureReason = GetHealthCheckLastFailureReason'
  { -- | The ID for the health check for which you want the last failure reason.
    -- When you created the health check, @CreateHealthCheck@ returned the ID
    -- in the response, in the @HealthCheckId@ element.
    --
    -- If you want to get the last failure reason for a calculated health
    -- check, you must use the Amazon Route 53 console or the CloudWatch
    -- console. You can\'t use @GetHealthCheckLastFailureReason@ for a
    -- calculated health check.
    healthCheckId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHealthCheckLastFailureReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckId', 'getHealthCheckLastFailureReason_healthCheckId' - The ID for the health check for which you want the last failure reason.
-- When you created the health check, @CreateHealthCheck@ returned the ID
-- in the response, in the @HealthCheckId@ element.
--
-- If you want to get the last failure reason for a calculated health
-- check, you must use the Amazon Route 53 console or the CloudWatch
-- console. You can\'t use @GetHealthCheckLastFailureReason@ for a
-- calculated health check.
newGetHealthCheckLastFailureReason ::
  -- | 'healthCheckId'
  Prelude.Text ->
  GetHealthCheckLastFailureReason
newGetHealthCheckLastFailureReason pHealthCheckId_ =
  GetHealthCheckLastFailureReason'
    { healthCheckId =
        pHealthCheckId_
    }

-- | The ID for the health check for which you want the last failure reason.
-- When you created the health check, @CreateHealthCheck@ returned the ID
-- in the response, in the @HealthCheckId@ element.
--
-- If you want to get the last failure reason for a calculated health
-- check, you must use the Amazon Route 53 console or the CloudWatch
-- console. You can\'t use @GetHealthCheckLastFailureReason@ for a
-- calculated health check.
getHealthCheckLastFailureReason_healthCheckId :: Lens.Lens' GetHealthCheckLastFailureReason Prelude.Text
getHealthCheckLastFailureReason_healthCheckId = Lens.lens (\GetHealthCheckLastFailureReason' {healthCheckId} -> healthCheckId) (\s@GetHealthCheckLastFailureReason' {} a -> s {healthCheckId = a} :: GetHealthCheckLastFailureReason)

instance
  Core.AWSRequest
    GetHealthCheckLastFailureReason
  where
  type
    AWSResponse GetHealthCheckLastFailureReason =
      GetHealthCheckLastFailureReasonResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetHealthCheckLastFailureReasonResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "HealthCheckObservations"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "HealthCheckObservation"
                        )
      )

instance
  Prelude.Hashable
    GetHealthCheckLastFailureReason
  where
  hashWithSalt
    _salt
    GetHealthCheckLastFailureReason' {..} =
      _salt `Prelude.hashWithSalt` healthCheckId

instance
  Prelude.NFData
    GetHealthCheckLastFailureReason
  where
  rnf GetHealthCheckLastFailureReason' {..} =
    Prelude.rnf healthCheckId

instance
  Data.ToHeaders
    GetHealthCheckLastFailureReason
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetHealthCheckLastFailureReason where
  toPath GetHealthCheckLastFailureReason' {..} =
    Prelude.mconcat
      [ "/2013-04-01/healthcheck/",
        Data.toBS healthCheckId,
        "/lastfailurereason"
      ]

instance Data.ToQuery GetHealthCheckLastFailureReason where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the response to a
-- @GetHealthCheckLastFailureReason@ request.
--
-- /See:/ 'newGetHealthCheckLastFailureReasonResponse' smart constructor.
data GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains one @Observation@ element for each Amazon Route 53
    -- health checker that is reporting a last failure reason.
    healthCheckObservations :: [HealthCheckObservation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHealthCheckLastFailureReasonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getHealthCheckLastFailureReasonResponse_httpStatus' - The response's http status code.
--
-- 'healthCheckObservations', 'getHealthCheckLastFailureReasonResponse_healthCheckObservations' - A list that contains one @Observation@ element for each Amazon Route 53
-- health checker that is reporting a last failure reason.
newGetHealthCheckLastFailureReasonResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHealthCheckLastFailureReasonResponse
newGetHealthCheckLastFailureReasonResponse
  pHttpStatus_ =
    GetHealthCheckLastFailureReasonResponse'
      { httpStatus =
          pHttpStatus_,
        healthCheckObservations =
          Prelude.mempty
      }

-- | The response's http status code.
getHealthCheckLastFailureReasonResponse_httpStatus :: Lens.Lens' GetHealthCheckLastFailureReasonResponse Prelude.Int
getHealthCheckLastFailureReasonResponse_httpStatus = Lens.lens (\GetHealthCheckLastFailureReasonResponse' {httpStatus} -> httpStatus) (\s@GetHealthCheckLastFailureReasonResponse' {} a -> s {httpStatus = a} :: GetHealthCheckLastFailureReasonResponse)

-- | A list that contains one @Observation@ element for each Amazon Route 53
-- health checker that is reporting a last failure reason.
getHealthCheckLastFailureReasonResponse_healthCheckObservations :: Lens.Lens' GetHealthCheckLastFailureReasonResponse [HealthCheckObservation]
getHealthCheckLastFailureReasonResponse_healthCheckObservations = Lens.lens (\GetHealthCheckLastFailureReasonResponse' {healthCheckObservations} -> healthCheckObservations) (\s@GetHealthCheckLastFailureReasonResponse' {} a -> s {healthCheckObservations = a} :: GetHealthCheckLastFailureReasonResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetHealthCheckLastFailureReasonResponse
  where
  rnf GetHealthCheckLastFailureReasonResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf healthCheckObservations
