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
-- Module      : Network.AWS.Route53.GetHealthCheckStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status of a specified health check.
module Network.AWS.Route53.GetHealthCheckStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

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
    healthCheckId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
getHealthCheckStatus_healthCheckId :: Lens.Lens' GetHealthCheckStatus Core.Text
getHealthCheckStatus_healthCheckId = Lens.lens (\GetHealthCheckStatus' {healthCheckId} -> healthCheckId) (\s@GetHealthCheckStatus' {} a -> s {healthCheckId = a} :: GetHealthCheckStatus)

instance Core.AWSRequest GetHealthCheckStatus where
  type
    AWSResponse GetHealthCheckStatus =
      GetHealthCheckStatusResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetHealthCheckStatusResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "HealthCheckObservations"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "HealthCheckObservation"
                     )
      )

instance Core.Hashable GetHealthCheckStatus

instance Core.NFData GetHealthCheckStatus

instance Core.ToHeaders GetHealthCheckStatus where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetHealthCheckStatus where
  toPath GetHealthCheckStatus' {..} =
    Core.mconcat
      [ "/2013-04-01/healthcheck/",
        Core.toBS healthCheckId,
        "/status"
      ]

instance Core.ToQuery GetHealthCheckStatus where
  toQuery = Core.const Core.mempty

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
-- /See:/ 'newGetHealthCheckStatusResponse' smart constructor.
data GetHealthCheckStatusResponse = GetHealthCheckStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list that contains one @HealthCheckObservation@ element for each
    -- Amazon Route 53 health checker that is reporting a status about the
    -- health check endpoint.
    healthCheckObservations :: [HealthCheckObservation]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetHealthCheckStatusResponse
newGetHealthCheckStatusResponse pHttpStatus_ =
  GetHealthCheckStatusResponse'
    { httpStatus =
        pHttpStatus_,
      healthCheckObservations = Core.mempty
    }

-- | The response's http status code.
getHealthCheckStatusResponse_httpStatus :: Lens.Lens' GetHealthCheckStatusResponse Core.Int
getHealthCheckStatusResponse_httpStatus = Lens.lens (\GetHealthCheckStatusResponse' {httpStatus} -> httpStatus) (\s@GetHealthCheckStatusResponse' {} a -> s {httpStatus = a} :: GetHealthCheckStatusResponse)

-- | A list that contains one @HealthCheckObservation@ element for each
-- Amazon Route 53 health checker that is reporting a status about the
-- health check endpoint.
getHealthCheckStatusResponse_healthCheckObservations :: Lens.Lens' GetHealthCheckStatusResponse [HealthCheckObservation]
getHealthCheckStatusResponse_healthCheckObservations = Lens.lens (\GetHealthCheckStatusResponse' {healthCheckObservations} -> healthCheckObservations) (\s@GetHealthCheckStatusResponse' {} a -> s {healthCheckObservations = a} :: GetHealthCheckStatusResponse) Core.. Lens._Coerce

instance Core.NFData GetHealthCheckStatusResponse
