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
-- Module      : Amazonka.Proton.GetService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detail data for a service.
module Amazonka.Proton.GetService
  ( -- * Creating a Request
    GetService (..),
    newGetService,

    -- * Request Lenses
    getService_name,

    -- * Destructuring the Response
    GetServiceResponse (..),
    newGetServiceResponse,

    -- * Response Lenses
    getServiceResponse_service,
    getServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetService' smart constructor.
data GetService = GetService'
  { -- | The name of the service that you want to get the detail data for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getService_name' - The name of the service that you want to get the detail data for.
newGetService ::
  -- | 'name'
  Prelude.Text ->
  GetService
newGetService pName_ = GetService' {name = pName_}

-- | The name of the service that you want to get the detail data for.
getService_name :: Lens.Lens' GetService Prelude.Text
getService_name = Lens.lens (\GetService' {name} -> name) (\s@GetService' {} a -> s {name = a} :: GetService)

instance Core.AWSRequest GetService where
  type AWSResponse GetService = GetServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceResponse'
            Prelude.<$> (x Core..?> "service")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetService

instance Prelude.NFData GetService

instance Core.ToHeaders GetService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.GetService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetService where
  toJSON GetService' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath GetService where
  toPath = Prelude.const "/"

instance Core.ToQuery GetService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceResponse' smart constructor.
data GetServiceResponse = GetServiceResponse'
  { -- | The service detail data that\'s returned by AWS Proton.
    service :: Prelude.Maybe Service,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'getServiceResponse_service' - The service detail data that\'s returned by AWS Proton.
--
-- 'httpStatus', 'getServiceResponse_httpStatus' - The response's http status code.
newGetServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceResponse
newGetServiceResponse pHttpStatus_ =
  GetServiceResponse'
    { service = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The service detail data that\'s returned by AWS Proton.
getServiceResponse_service :: Lens.Lens' GetServiceResponse (Prelude.Maybe Service)
getServiceResponse_service = Lens.lens (\GetServiceResponse' {service} -> service) (\s@GetServiceResponse' {} a -> s {service = a} :: GetServiceResponse)

-- | The response's http status code.
getServiceResponse_httpStatus :: Lens.Lens' GetServiceResponse Prelude.Int
getServiceResponse_httpStatus = Lens.lens (\GetServiceResponse' {httpStatus} -> httpStatus) (\s@GetServiceResponse' {} a -> s {httpStatus = a} :: GetServiceResponse)

instance Prelude.NFData GetServiceResponse
