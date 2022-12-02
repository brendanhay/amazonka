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
-- Module      : Amazonka.Route53.GetHealthCheckCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the number of health checks that are associated with the
-- current Amazon Web Services account.
module Amazonka.Route53.GetHealthCheckCount
  ( -- * Creating a Request
    GetHealthCheckCount (..),
    newGetHealthCheckCount,

    -- * Destructuring the Response
    GetHealthCheckCountResponse (..),
    newGetHealthCheckCountResponse,

    -- * Response Lenses
    getHealthCheckCountResponse_httpStatus,
    getHealthCheckCountResponse_healthCheckCount,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request for the number of health checks that are associated with the
-- current Amazon Web Services account.
--
-- /See:/ 'newGetHealthCheckCount' smart constructor.
data GetHealthCheckCount = GetHealthCheckCount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHealthCheckCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetHealthCheckCount ::
  GetHealthCheckCount
newGetHealthCheckCount = GetHealthCheckCount'

instance Core.AWSRequest GetHealthCheckCount where
  type
    AWSResponse GetHealthCheckCount =
      GetHealthCheckCountResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetHealthCheckCountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "HealthCheckCount")
      )

instance Prelude.Hashable GetHealthCheckCount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetHealthCheckCount where
  rnf _ = ()

instance Data.ToHeaders GetHealthCheckCount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetHealthCheckCount where
  toPath = Prelude.const "/2013-04-01/healthcheckcount"

instance Data.ToQuery GetHealthCheckCount where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the response to a @GetHealthCheckCount@
-- request.
--
-- /See:/ 'newGetHealthCheckCountResponse' smart constructor.
data GetHealthCheckCountResponse = GetHealthCheckCountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The number of health checks associated with the current Amazon Web
    -- Services account.
    healthCheckCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHealthCheckCountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getHealthCheckCountResponse_httpStatus' - The response's http status code.
--
-- 'healthCheckCount', 'getHealthCheckCountResponse_healthCheckCount' - The number of health checks associated with the current Amazon Web
-- Services account.
newGetHealthCheckCountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'healthCheckCount'
  Prelude.Integer ->
  GetHealthCheckCountResponse
newGetHealthCheckCountResponse
  pHttpStatus_
  pHealthCheckCount_ =
    GetHealthCheckCountResponse'
      { httpStatus =
          pHttpStatus_,
        healthCheckCount = pHealthCheckCount_
      }

-- | The response's http status code.
getHealthCheckCountResponse_httpStatus :: Lens.Lens' GetHealthCheckCountResponse Prelude.Int
getHealthCheckCountResponse_httpStatus = Lens.lens (\GetHealthCheckCountResponse' {httpStatus} -> httpStatus) (\s@GetHealthCheckCountResponse' {} a -> s {httpStatus = a} :: GetHealthCheckCountResponse)

-- | The number of health checks associated with the current Amazon Web
-- Services account.
getHealthCheckCountResponse_healthCheckCount :: Lens.Lens' GetHealthCheckCountResponse Prelude.Integer
getHealthCheckCountResponse_healthCheckCount = Lens.lens (\GetHealthCheckCountResponse' {healthCheckCount} -> healthCheckCount) (\s@GetHealthCheckCountResponse' {} a -> s {healthCheckCount = a} :: GetHealthCheckCountResponse)

instance Prelude.NFData GetHealthCheckCountResponse where
  rnf GetHealthCheckCountResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf healthCheckCount
