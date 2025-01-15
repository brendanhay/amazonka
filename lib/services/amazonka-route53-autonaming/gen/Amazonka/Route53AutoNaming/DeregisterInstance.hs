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
-- Module      : Amazonka.Route53AutoNaming.DeregisterInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Route 53 DNS records and health check, if any, that
-- Cloud Map created for the specified instance.
module Amazonka.Route53AutoNaming.DeregisterInstance
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newDeregisterInstance' smart constructor.
data DeregisterInstance = DeregisterInstance'
  { -- | The ID of the service that the instance is associated with.
    serviceId :: Prelude.Text,
    -- | The value that you specified for @Id@ in the
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
    -- request.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeregisterInstance where
  type
    AWSResponse DeregisterInstance =
      DeregisterInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterInstanceResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterInstance where
  hashWithSalt _salt DeregisterInstance' {..} =
    _salt
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DeregisterInstance where
  rnf DeregisterInstance' {..} =
    Prelude.rnf serviceId `Prelude.seq`
      Prelude.rnf instanceId

instance Data.ToHeaders DeregisterInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.DeregisterInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterInstance where
  toJSON DeregisterInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceId" Data..= serviceId),
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath DeregisterInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse = DeregisterInstanceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'deregisterInstanceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
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
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
deregisterInstanceResponse_operationId :: Lens.Lens' DeregisterInstanceResponse (Prelude.Maybe Prelude.Text)
deregisterInstanceResponse_operationId = Lens.lens (\DeregisterInstanceResponse' {operationId} -> operationId) (\s@DeregisterInstanceResponse' {} a -> s {operationId = a} :: DeregisterInstanceResponse)

-- | The response's http status code.
deregisterInstanceResponse_httpStatus :: Lens.Lens' DeregisterInstanceResponse Prelude.Int
deregisterInstanceResponse_httpStatus = Lens.lens (\DeregisterInstanceResponse' {httpStatus} -> httpStatus) (\s@DeregisterInstanceResponse' {} a -> s {httpStatus = a} :: DeregisterInstanceResponse)

instance Prelude.NFData DeregisterInstanceResponse where
  rnf DeregisterInstanceResponse' {..} =
    Prelude.rnf operationId `Prelude.seq`
      Prelude.rnf httpStatus
