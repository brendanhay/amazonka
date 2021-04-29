{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53AutoNaming.DeregisterInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Route 53 DNS records and health check, if any, that
-- AWS Cloud Map created for the specified instance.
module Network.AWS.Route53AutoNaming.DeregisterInstance
  ( -- * Creating a Request
    DeregisterInstance (..),
    newDeregisterInstance,

    -- * Request Lenses
    deregisterInstance_serviceId,
    deregisterInstance_instanceId,

    -- * Destructuring the Response
    DeregisterInstanceResponse (..),
    newDeregisterInstanceResponse,

    -- * Response Lenses
    deregisterInstanceResponse_operationId,
    deregisterInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newDeregisterInstance' smart constructor.
data DeregisterInstance = DeregisterInstance'
  { -- | The ID of the service that the instance is associated with.
    serviceId :: Prelude.Text,
    -- | The value that you specified for @Id@ in the
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
    -- request.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceId', 'deregisterInstance_serviceId' - The ID of the service that the instance is associated with.
--
-- 'instanceId', 'deregisterInstance_instanceId' - The value that you specified for @Id@ in the
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
-- request.
newDeregisterInstance ::
  -- | 'serviceId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  DeregisterInstance
newDeregisterInstance pServiceId_ pInstanceId_ =
  DeregisterInstance'
    { serviceId = pServiceId_,
      instanceId = pInstanceId_
    }

-- | The ID of the service that the instance is associated with.
deregisterInstance_serviceId :: Lens.Lens' DeregisterInstance Prelude.Text
deregisterInstance_serviceId = Lens.lens (\DeregisterInstance' {serviceId} -> serviceId) (\s@DeregisterInstance' {} a -> s {serviceId = a} :: DeregisterInstance)

-- | The value that you specified for @Id@ in the
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
-- request.
deregisterInstance_instanceId :: Lens.Lens' DeregisterInstance Prelude.Text
deregisterInstance_instanceId = Lens.lens (\DeregisterInstance' {instanceId} -> instanceId) (\s@DeregisterInstance' {} a -> s {instanceId = a} :: DeregisterInstance)

instance Prelude.AWSRequest DeregisterInstance where
  type
    Rs DeregisterInstance =
      DeregisterInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterInstanceResponse'
            Prelude.<$> (x Prelude..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterInstance

instance Prelude.NFData DeregisterInstance

instance Prelude.ToHeaders DeregisterInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53AutoNaming_v20170314.DeregisterInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterInstance where
  toJSON DeregisterInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceId" Prelude..= serviceId),
            Prelude.Just ("InstanceId" Prelude..= instanceId)
          ]
      )

instance Prelude.ToPath DeregisterInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse = DeregisterInstanceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. For more information, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'deregisterInstanceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'deregisterInstanceResponse_httpStatus' - The response's http status code.
newDeregisterInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterInstanceResponse
newDeregisterInstanceResponse pHttpStatus_ =
  DeregisterInstanceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
deregisterInstanceResponse_operationId :: Lens.Lens' DeregisterInstanceResponse (Prelude.Maybe Prelude.Text)
deregisterInstanceResponse_operationId = Lens.lens (\DeregisterInstanceResponse' {operationId} -> operationId) (\s@DeregisterInstanceResponse' {} a -> s {operationId = a} :: DeregisterInstanceResponse)

-- | The response's http status code.
deregisterInstanceResponse_httpStatus :: Lens.Lens' DeregisterInstanceResponse Prelude.Int
deregisterInstanceResponse_httpStatus = Lens.lens (\DeregisterInstanceResponse' {httpStatus} -> httpStatus) (\s@DeregisterInstanceResponse' {} a -> s {httpStatus = a} :: DeregisterInstanceResponse)

instance Prelude.NFData DeregisterInstanceResponse
