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
-- Module      : Amazonka.Route53.GetHealthCheckStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status of a specified health check.
--
-- This API is intended for use during development to diagnose behavior. It
-- doesn’t support production use-cases with high query rates that require
-- immediate and actionable responses.
module Amazonka.Route53.GetHealthCheckStatus
  ( -- * Creating a Request
    GetHealthCheckStatus (..),
    newGetHealthCheckStatus,

    -- * Request Lenses
    getHealthCheckStatus_healthCheckId,

    -- * Destructuring the Response
    GetHealthCheckStatusResponse (..),
    newGetHealthCheckStatusResponse,

    -- * Response Lenses
    getHealthCheckStatusResponse_httpStatus,
    getHealthCheckStatusResponse_healthCheckObservations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to get the status for a health check.
--
-- /See:/ 'newGetHealthCheckStatus' smart constructor.
data GetHealthCheckStatus = GetHealthCheckStatus'
  { -- | The ID for the health check that you want the current status for. When
    -- you created the health check, @CreateHealthCheck@ returned the ID in the
    -- response, in the @HealthCheckId@ element.
    --
    -- If you want to check the status of a calculated health check, you must
    -- use the Amazon Route 53 console or the CloudWatch console. You can\'t
    -- use @GetHealthCheckStatus@ to get the status of a calculated health
    -- check.
    healthCheckId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHealthCheckStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckId', 'getHealthCheckStatus_healthCheckId' - The ID for the health check that you want the current status for. When
-- you created the health check, @CreateHealthCheck@ returned the ID in the
-- response, in the @HealthCheckId@ element.
--
-- If you want to check the status of a calculated health check, you must
-- use the Amazon Route 53 console or the CloudWatch console. You can\'t
-- use @GetHealthCheckStatus@ to get the status of a calculated health
-- check.
newGetHealthCheckStatus ::
  -- | 'healthCheckId'
  Prelude.Text ->
  GetHealthCheckStatus
newGetHealthCheckStatus pHealthCheckId_ =
  GetHealthCheckStatus'
    { healthCheckId =
        pHealthCheckId_
    }

-- | The ID for the health check that you want the current status for. When
-- you created the health check, @CreateHealthCheck@ returned the ID in the
-- response, in the @HealthCheckId@ element.
--
-- If you want to check the status of a calculated health check, you must
-- use the Amazon Route 53 console or the CloudWatch console. You can\'t
-- use @GetHealthCheckStatus@ to get the status of a calculated health
-- check.
getHealthCheckStatus_healthCheckId :: Lens.Lens' GetHealthCheckStatus Prelude.Text
getHealthCheckStatus_healthCheckId = Lens.lens (\GetHealthCheckStatus' {healthCheckId} -> healthCheckId) (\s@GetHealthCheckStatus' {} a -> s {healthCheckId = a} :: GetHealthCheckStatus)

instance Core.AWSRequest GetHealthCheckStatus where
  type
    AWSResponse GetHealthCheckStatus =
      GetHealthCheckStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetHealthCheckStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "HealthCheckObservations"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "HealthCheckObservation"
                        )
      )

instance Prelude.Hashable GetHealthCheckStatus where
  hashWithSalt _salt GetHealthCheckStatus' {..} =
    _salt `Prelude.hashWithSalt` healthCheckId

instance Prelude.NFData GetHealthCheckStatus where
  rnf GetHealthCheckStatus' {..} =
    Prelude.rnf healthCheckId

instance Data.ToHeaders GetHealthCheckStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetHealthCheckStatus where
  toPath GetHealthCheckStatus' {..} =
    Prelude.mconcat
      [ "/2013-04-01/healthcheck/",
        Data.toBS healthCheckId,
        "/status"
      ]

instance Data.ToQuery GetHealthCheckStatus where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
-- /See:/ 'newGetHealthCheckStatusResponse' smart constructor.
data GetHealthCheckStatusResponse = GetHealthCheckStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains one @HealthCheckObservation@ element for each
    -- Amazon Route 53 health checker that is reporting a status about the
    -- health check endpoint.
    healthCheckObservations :: [HealthCheckObservation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHealthCheckStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getHealthCheckStatusResponse_httpStatus' - The response's http status code.
--
-- 'healthCheckObservations', 'getHealthCheckStatusResponse_healthCheckObservations' - A list that contains one @HealthCheckObservation@ element for each
-- Amazon Route 53 health checker that is reporting a status about the
-- health check endpoint.
newGetHealthCheckStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHealthCheckStatusResponse
newGetHealthCheckStatusResponse pHttpStatus_ =
  GetHealthCheckStatusResponse'
    { httpStatus =
        pHttpStatus_,
      healthCheckObservations = Prelude.mempty
    }

-- | The response's http status code.
getHealthCheckStatusResponse_httpStatus :: Lens.Lens' GetHealthCheckStatusResponse Prelude.Int
getHealthCheckStatusResponse_httpStatus = Lens.lens (\GetHealthCheckStatusResponse' {httpStatus} -> httpStatus) (\s@GetHealthCheckStatusResponse' {} a -> s {httpStatus = a} :: GetHealthCheckStatusResponse)

-- | A list that contains one @HealthCheckObservation@ element for each
-- Amazon Route 53 health checker that is reporting a status about the
-- health check endpoint.
getHealthCheckStatusResponse_healthCheckObservations :: Lens.Lens' GetHealthCheckStatusResponse [HealthCheckObservation]
getHealthCheckStatusResponse_healthCheckObservations = Lens.lens (\GetHealthCheckStatusResponse' {healthCheckObservations} -> healthCheckObservations) (\s@GetHealthCheckStatusResponse' {} a -> s {healthCheckObservations = a} :: GetHealthCheckStatusResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetHealthCheckStatusResponse where
  rnf GetHealthCheckStatusResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf healthCheckObservations
