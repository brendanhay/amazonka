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
-- Module      : Amazonka.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified health check.
module Amazonka.Route53.GetHealthCheck
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to get information about a specified health check.
--
-- /See:/ 'newGetHealthCheck' smart constructor.
data GetHealthCheck = GetHealthCheck'
  { -- | The identifier that Amazon Route 53 assigned to the health check when
    -- you created it. When you add or update a resource record set, you use
    -- this value to specify which health check to use. The value can be up to
    -- 64 characters long.
    healthCheckId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetHealthCheck
newGetHealthCheck pHealthCheckId_ =
  GetHealthCheck' {healthCheckId = pHealthCheckId_}

-- | The identifier that Amazon Route 53 assigned to the health check when
-- you created it. When you add or update a resource record set, you use
-- this value to specify which health check to use. The value can be up to
-- 64 characters long.
getHealthCheck_healthCheckId :: Lens.Lens' GetHealthCheck Prelude.Text
getHealthCheck_healthCheckId = Lens.lens (\GetHealthCheck' {healthCheckId} -> healthCheckId) (\s@GetHealthCheck' {} a -> s {healthCheckId = a} :: GetHealthCheck)

instance Core.AWSRequest GetHealthCheck where
  type
    AWSResponse GetHealthCheck =
      GetHealthCheckResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetHealthCheckResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "HealthCheck")
      )

instance Prelude.Hashable GetHealthCheck where
  hashWithSalt _salt GetHealthCheck' {..} =
    _salt `Prelude.hashWithSalt` healthCheckId

instance Prelude.NFData GetHealthCheck where
  rnf GetHealthCheck' {..} = Prelude.rnf healthCheckId

instance Data.ToHeaders GetHealthCheck where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetHealthCheck where
  toPath GetHealthCheck' {..} =
    Prelude.mconcat
      ["/2013-04-01/healthcheck/", Data.toBS healthCheckId]

instance Data.ToQuery GetHealthCheck where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
-- /See:/ 'newGetHealthCheckResponse' smart constructor.
data GetHealthCheckResponse = GetHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains information about one health check that is
    -- associated with the current Amazon Web Services account.
    healthCheck :: HealthCheck
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- associated with the current Amazon Web Services account.
newGetHealthCheckResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'healthCheck'
  HealthCheck ->
  GetHealthCheckResponse
newGetHealthCheckResponse pHttpStatus_ pHealthCheck_ =
  GetHealthCheckResponse'
    { httpStatus = pHttpStatus_,
      healthCheck = pHealthCheck_
    }

-- | The response's http status code.
getHealthCheckResponse_httpStatus :: Lens.Lens' GetHealthCheckResponse Prelude.Int
getHealthCheckResponse_httpStatus = Lens.lens (\GetHealthCheckResponse' {httpStatus} -> httpStatus) (\s@GetHealthCheckResponse' {} a -> s {httpStatus = a} :: GetHealthCheckResponse)

-- | A complex type that contains information about one health check that is
-- associated with the current Amazon Web Services account.
getHealthCheckResponse_healthCheck :: Lens.Lens' GetHealthCheckResponse HealthCheck
getHealthCheckResponse_healthCheck = Lens.lens (\GetHealthCheckResponse' {healthCheck} -> healthCheck) (\s@GetHealthCheckResponse' {} a -> s {healthCheck = a} :: GetHealthCheckResponse)

instance Prelude.NFData GetHealthCheckResponse where
  rnf GetHealthCheckResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf healthCheck
