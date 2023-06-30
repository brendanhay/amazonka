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
-- Module      : Amazonka.Route53AutoNaming.GetInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified instance.
module Amazonka.Route53AutoNaming.GetInstance
  ( -- * Creating a Request
    GetInstance (..),
    newGetInstance,

    -- * Request Lenses
    getInstance_serviceId,
    getInstance_instanceId,

    -- * Destructuring the Response
    GetInstanceResponse (..),
    newGetInstanceResponse,

    -- * Response Lenses
    getInstanceResponse_instance,
    getInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newGetInstance' smart constructor.
data GetInstance = GetInstance'
  { -- | The ID of the service that the instance is associated with.
    serviceId :: Prelude.Text,
    -- | The ID of the instance that you want to get information about.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceId', 'getInstance_serviceId' - The ID of the service that the instance is associated with.
--
-- 'instanceId', 'getInstance_instanceId' - The ID of the instance that you want to get information about.
newGetInstance ::
  -- | 'serviceId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  GetInstance
newGetInstance pServiceId_ pInstanceId_ =
  GetInstance'
    { serviceId = pServiceId_,
      instanceId = pInstanceId_
    }

-- | The ID of the service that the instance is associated with.
getInstance_serviceId :: Lens.Lens' GetInstance Prelude.Text
getInstance_serviceId = Lens.lens (\GetInstance' {serviceId} -> serviceId) (\s@GetInstance' {} a -> s {serviceId = a} :: GetInstance)

-- | The ID of the instance that you want to get information about.
getInstance_instanceId :: Lens.Lens' GetInstance Prelude.Text
getInstance_instanceId = Lens.lens (\GetInstance' {instanceId} -> instanceId) (\s@GetInstance' {} a -> s {instanceId = a} :: GetInstance)

instance Core.AWSRequest GetInstance where
  type AWSResponse GetInstance = GetInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceResponse'
            Prelude.<$> (x Data..?> "Instance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstance where
  hashWithSalt _salt GetInstance' {..} =
    _salt
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData GetInstance where
  rnf GetInstance' {..} =
    Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders GetInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.GetInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInstance where
  toJSON GetInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceId" Data..= serviceId),
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath GetInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { -- | A complex type that contains information about a specified instance.
    instance' :: Prelude.Maybe Instance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instance'', 'getInstanceResponse_instance' - A complex type that contains information about a specified instance.
--
-- 'httpStatus', 'getInstanceResponse_httpStatus' - The response's http status code.
newGetInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstanceResponse
newGetInstanceResponse pHttpStatus_ =
  GetInstanceResponse'
    { instance' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains information about a specified instance.
getInstanceResponse_instance :: Lens.Lens' GetInstanceResponse (Prelude.Maybe Instance)
getInstanceResponse_instance = Lens.lens (\GetInstanceResponse' {instance'} -> instance') (\s@GetInstanceResponse' {} a -> s {instance' = a} :: GetInstanceResponse)

-- | The response's http status code.
getInstanceResponse_httpStatus :: Lens.Lens' GetInstanceResponse Prelude.Int
getInstanceResponse_httpStatus = Lens.lens (\GetInstanceResponse' {httpStatus} -> httpStatus) (\s@GetInstanceResponse' {} a -> s {httpStatus = a} :: GetInstanceResponse)

instance Prelude.NFData GetInstanceResponse where
  rnf GetInstanceResponse' {..} =
    Prelude.rnf instance'
      `Prelude.seq` Prelude.rnf httpStatus
