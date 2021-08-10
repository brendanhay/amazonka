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
-- Module      : Network.AWS.Route53AutoNaming.GetInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified instance.
module Network.AWS.Route53AutoNaming.GetInstance
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceResponse'
            Prelude.<$> (x Core..?> "Instance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstance

instance Prelude.NFData GetInstance

instance Core.ToHeaders GetInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.GetInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetInstance where
  toJSON GetInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceId" Core..= serviceId),
            Prelude.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.ToPath GetInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery GetInstance where
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

instance Prelude.NFData GetInstanceResponse
