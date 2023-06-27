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
-- Module      : Amazonka.VPCLattice.DeleteService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a service. A service can\'t be deleted if it\'s associated with
-- a service network. If you delete a service, all resources related to the
-- service, such as the resource policy, auth policy, listeners, listener
-- rules, and access log subscriptions, are also deleted. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/services.html#delete-service Delete a service>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.DeleteService
  ( -- * Creating a Request
    DeleteService (..),
    newDeleteService,

    -- * Request Lenses
    deleteService_serviceIdentifier,

    -- * Destructuring the Response
    DeleteServiceResponse (..),
    newDeleteServiceResponse,

    -- * Response Lenses
    deleteServiceResponse_arn,
    deleteServiceResponse_id,
    deleteServiceResponse_name,
    deleteServiceResponse_status,
    deleteServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteService' smart constructor.
data DeleteService = DeleteService'
  { -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceIdentifier', 'deleteService_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newDeleteService ::
  -- | 'serviceIdentifier'
  Prelude.Text ->
  DeleteService
newDeleteService pServiceIdentifier_ =
  DeleteService'
    { serviceIdentifier =
        pServiceIdentifier_
    }

-- | The ID or Amazon Resource Name (ARN) of the service.
deleteService_serviceIdentifier :: Lens.Lens' DeleteService Prelude.Text
deleteService_serviceIdentifier = Lens.lens (\DeleteService' {serviceIdentifier} -> serviceIdentifier) (\s@DeleteService' {} a -> s {serviceIdentifier = a} :: DeleteService)

instance Core.AWSRequest DeleteService where
  type
    AWSResponse DeleteService =
      DeleteServiceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteService where
  hashWithSalt _salt DeleteService' {..} =
    _salt `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData DeleteService where
  rnf DeleteService' {..} =
    Prelude.rnf serviceIdentifier

instance Data.ToHeaders DeleteService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteService where
  toPath DeleteService' {..} =
    Prelude.mconcat
      ["/services/", Data.toBS serviceIdentifier]

instance Data.ToQuery DeleteService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceResponse' smart constructor.
data DeleteServiceResponse = DeleteServiceResponse'
  { -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status. You can retry the operation if the status is
    -- @DELETE_FAILED@. However, if you retry it while the status is
    -- @DELETE_IN_PROGRESS@, the status doesn\'t change.
    status :: Prelude.Maybe ServiceStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteServiceResponse_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'id', 'deleteServiceResponse_id' - The ID of the service.
--
-- 'name', 'deleteServiceResponse_name' - The name of the service.
--
-- 'status', 'deleteServiceResponse_status' - The status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it while the status is
-- @DELETE_IN_PROGRESS@, the status doesn\'t change.
--
-- 'httpStatus', 'deleteServiceResponse_httpStatus' - The response's http status code.
newDeleteServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceResponse
newDeleteServiceResponse pHttpStatus_ =
  DeleteServiceResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the service.
deleteServiceResponse_arn :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.Text)
deleteServiceResponse_arn = Lens.lens (\DeleteServiceResponse' {arn} -> arn) (\s@DeleteServiceResponse' {} a -> s {arn = a} :: DeleteServiceResponse)

-- | The ID of the service.
deleteServiceResponse_id :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.Text)
deleteServiceResponse_id = Lens.lens (\DeleteServiceResponse' {id} -> id) (\s@DeleteServiceResponse' {} a -> s {id = a} :: DeleteServiceResponse)

-- | The name of the service.
deleteServiceResponse_name :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.Text)
deleteServiceResponse_name = Lens.lens (\DeleteServiceResponse' {name} -> name) (\s@DeleteServiceResponse' {} a -> s {name = a} :: DeleteServiceResponse)

-- | The status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it while the status is
-- @DELETE_IN_PROGRESS@, the status doesn\'t change.
deleteServiceResponse_status :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe ServiceStatus)
deleteServiceResponse_status = Lens.lens (\DeleteServiceResponse' {status} -> status) (\s@DeleteServiceResponse' {} a -> s {status = a} :: DeleteServiceResponse)

-- | The response's http status code.
deleteServiceResponse_httpStatus :: Lens.Lens' DeleteServiceResponse Prelude.Int
deleteServiceResponse_httpStatus = Lens.lens (\DeleteServiceResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceResponse' {} a -> s {httpStatus = a} :: DeleteServiceResponse)

instance Prelude.NFData DeleteServiceResponse where
  rnf DeleteServiceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
