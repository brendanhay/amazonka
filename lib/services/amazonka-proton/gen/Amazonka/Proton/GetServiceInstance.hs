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
-- Module      : Amazonka.Proton.GetServiceInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed data for a service instance. A service instance is an
-- instantiation of service template and it runs in a specific environment.
module Amazonka.Proton.GetServiceInstance
  ( -- * Creating a Request
    GetServiceInstance (..),
    newGetServiceInstance,

    -- * Request Lenses
    getServiceInstance_name,
    getServiceInstance_serviceName,

    -- * Destructuring the Response
    GetServiceInstanceResponse (..),
    newGetServiceInstanceResponse,

    -- * Response Lenses
    getServiceInstanceResponse_httpStatus,
    getServiceInstanceResponse_serviceInstance,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceInstance' smart constructor.
data GetServiceInstance = GetServiceInstance'
  { -- | The name of a service instance that you want to get the detailed data
    -- for.
    name :: Prelude.Text,
    -- | The name of the service that the service instance belongs to.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getServiceInstance_name' - The name of a service instance that you want to get the detailed data
-- for.
--
-- 'serviceName', 'getServiceInstance_serviceName' - The name of the service that the service instance belongs to.
newGetServiceInstance ::
  -- | 'name'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  GetServiceInstance
newGetServiceInstance pName_ pServiceName_ =
  GetServiceInstance'
    { name = pName_,
      serviceName = pServiceName_
    }

-- | The name of a service instance that you want to get the detailed data
-- for.
getServiceInstance_name :: Lens.Lens' GetServiceInstance Prelude.Text
getServiceInstance_name = Lens.lens (\GetServiceInstance' {name} -> name) (\s@GetServiceInstance' {} a -> s {name = a} :: GetServiceInstance)

-- | The name of the service that the service instance belongs to.
getServiceInstance_serviceName :: Lens.Lens' GetServiceInstance Prelude.Text
getServiceInstance_serviceName = Lens.lens (\GetServiceInstance' {serviceName} -> serviceName) (\s@GetServiceInstance' {} a -> s {serviceName = a} :: GetServiceInstance)

instance Core.AWSRequest GetServiceInstance where
  type
    AWSResponse GetServiceInstance =
      GetServiceInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "serviceInstance")
      )

instance Prelude.Hashable GetServiceInstance where
  hashWithSalt _salt GetServiceInstance' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GetServiceInstance where
  rnf GetServiceInstance' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders GetServiceInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetServiceInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetServiceInstance where
  toJSON GetServiceInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath GetServiceInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery GetServiceInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceInstanceResponse' smart constructor.
data GetServiceInstanceResponse = GetServiceInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The detailed data of the requested service instance.
    serviceInstance :: ServiceInstance
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getServiceInstanceResponse_httpStatus' - The response's http status code.
--
-- 'serviceInstance', 'getServiceInstanceResponse_serviceInstance' - The detailed data of the requested service instance.
newGetServiceInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceInstance'
  ServiceInstance ->
  GetServiceInstanceResponse
newGetServiceInstanceResponse
  pHttpStatus_
  pServiceInstance_ =
    GetServiceInstanceResponse'
      { httpStatus =
          pHttpStatus_,
        serviceInstance = pServiceInstance_
      }

-- | The response's http status code.
getServiceInstanceResponse_httpStatus :: Lens.Lens' GetServiceInstanceResponse Prelude.Int
getServiceInstanceResponse_httpStatus = Lens.lens (\GetServiceInstanceResponse' {httpStatus} -> httpStatus) (\s@GetServiceInstanceResponse' {} a -> s {httpStatus = a} :: GetServiceInstanceResponse)

-- | The detailed data of the requested service instance.
getServiceInstanceResponse_serviceInstance :: Lens.Lens' GetServiceInstanceResponse ServiceInstance
getServiceInstanceResponse_serviceInstance = Lens.lens (\GetServiceInstanceResponse' {serviceInstance} -> serviceInstance) (\s@GetServiceInstanceResponse' {} a -> s {serviceInstance = a} :: GetServiceInstanceResponse)

instance Prelude.NFData GetServiceInstanceResponse where
  rnf GetServiceInstanceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceInstance
