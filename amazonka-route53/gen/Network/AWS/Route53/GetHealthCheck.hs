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
-- Module      : Network.AWS.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified health check.
module Network.AWS.Route53.GetHealthCheck
  ( -- * Creating a Request
    GetHealthCheck (..),
    newGetHealthCheck,

    -- * Request Lenses
    getHealthCheck_healthCheckId,

    -- * Destructuring the Response
    GetHealthCheckResponse (..),
    newGetHealthCheckResponse,

    -- * Response Lenses
    getHealthCheckResponse_httpStatus,
    getHealthCheckResponse_healthCheck,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to get information about a specified health check.
--
-- /See:/ 'newGetHealthCheck' smart constructor.
data GetHealthCheck = GetHealthCheck'
  { -- | The identifier that Amazon Route 53 assigned to the health check when
    -- you created it. When you add or update a resource record set, you use
    -- this value to specify which health check to use. The value can be up to
    -- 64 characters long.
    healthCheckId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetHealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckId', 'getHealthCheck_healthCheckId' - The identifier that Amazon Route 53 assigned to the health check when
-- you created it. When you add or update a resource record set, you use
-- this value to specify which health check to use. The value can be up to
-- 64 characters long.
newGetHealthCheck ::
  -- | 'healthCheckId'
  Core.Text ->
  GetHealthCheck
newGetHealthCheck pHealthCheckId_ =
  GetHealthCheck' {healthCheckId = pHealthCheckId_}

-- | The identifier that Amazon Route 53 assigned to the health check when
-- you created it. When you add or update a resource record set, you use
-- this value to specify which health check to use. The value can be up to
-- 64 characters long.
getHealthCheck_healthCheckId :: Lens.Lens' GetHealthCheck Core.Text
getHealthCheck_healthCheckId = Lens.lens (\GetHealthCheck' {healthCheckId} -> healthCheckId) (\s@GetHealthCheck' {} a -> s {healthCheckId = a} :: GetHealthCheck)

instance Core.AWSRequest GetHealthCheck where
  type
    AWSResponse GetHealthCheck =
      GetHealthCheckResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetHealthCheckResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "HealthCheck")
      )

instance Core.Hashable GetHealthCheck

instance Core.NFData GetHealthCheck

instance Core.ToHeaders GetHealthCheck where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetHealthCheck where
  toPath GetHealthCheck' {..} =
    Core.mconcat
      ["/2013-04-01/healthcheck/", Core.toBS healthCheckId]

instance Core.ToQuery GetHealthCheck where
  toQuery = Core.const Core.mempty

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
-- /See:/ 'newGetHealthCheckResponse' smart constructor.
data GetHealthCheckResponse = GetHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains information about one health check that is
    -- associated with the current AWS account.
    healthCheck :: HealthCheck
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetHealthCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getHealthCheckResponse_httpStatus' - The response's http status code.
--
-- 'healthCheck', 'getHealthCheckResponse_healthCheck' - A complex type that contains information about one health check that is
-- associated with the current AWS account.
newGetHealthCheckResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'healthCheck'
  HealthCheck ->
  GetHealthCheckResponse
newGetHealthCheckResponse pHttpStatus_ pHealthCheck_ =
  GetHealthCheckResponse'
    { httpStatus = pHttpStatus_,
      healthCheck = pHealthCheck_
    }

-- | The response's http status code.
getHealthCheckResponse_httpStatus :: Lens.Lens' GetHealthCheckResponse Core.Int
getHealthCheckResponse_httpStatus = Lens.lens (\GetHealthCheckResponse' {httpStatus} -> httpStatus) (\s@GetHealthCheckResponse' {} a -> s {httpStatus = a} :: GetHealthCheckResponse)

-- | A complex type that contains information about one health check that is
-- associated with the current AWS account.
getHealthCheckResponse_healthCheck :: Lens.Lens' GetHealthCheckResponse HealthCheck
getHealthCheckResponse_healthCheck = Lens.lens (\GetHealthCheckResponse' {healthCheck} -> healthCheck) (\s@GetHealthCheckResponse' {} a -> s {healthCheck = a} :: GetHealthCheckResponse)

instance Core.NFData GetHealthCheckResponse
