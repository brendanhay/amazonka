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
-- Module      : Amazonka.MigrationHubReFactorSpaces.DeleteService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Web Services Migration Hub Refactor Spaces service.
module Amazonka.MigrationHubReFactorSpaces.DeleteService
  ( -- * Creating a Request
    DeleteService (..),
    newDeleteService,

    -- * Request Lenses
    deleteService_applicationIdentifier,
    deleteService_environmentIdentifier,
    deleteService_serviceIdentifier,

    -- * Destructuring the Response
    DeleteServiceResponse (..),
    newDeleteServiceResponse,

    -- * Response Lenses
    deleteServiceResponse_name,
    deleteServiceResponse_arn,
    deleteServiceResponse_state,
    deleteServiceResponse_lastUpdatedTime,
    deleteServiceResponse_environmentId,
    deleteServiceResponse_applicationId,
    deleteServiceResponse_serviceId,
    deleteServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteService' smart constructor.
data DeleteService = DeleteService'
  { -- | Deletes a Refactor Spaces service.
    --
    -- The @RefactorSpacesSecurityGroup@ security group must be removed from
    -- all Amazon Web Services resources in the virtual private cloud (VPC)
    -- prior to deleting a service with a URL endpoint in a VPC.
    applicationIdentifier :: Prelude.Text,
    -- | The ID of the environment that the service is in.
    environmentIdentifier :: Prelude.Text,
    -- | The ID of the service to delete.
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
-- 'applicationIdentifier', 'deleteService_applicationIdentifier' - Deletes a Refactor Spaces service.
--
-- The @RefactorSpacesSecurityGroup@ security group must be removed from
-- all Amazon Web Services resources in the virtual private cloud (VPC)
-- prior to deleting a service with a URL endpoint in a VPC.
--
-- 'environmentIdentifier', 'deleteService_environmentIdentifier' - The ID of the environment that the service is in.
--
-- 'serviceIdentifier', 'deleteService_serviceIdentifier' - The ID of the service to delete.
newDeleteService ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  DeleteService
newDeleteService
  pApplicationIdentifier_
  pEnvironmentIdentifier_
  pServiceIdentifier_ =
    DeleteService'
      { applicationIdentifier =
          pApplicationIdentifier_,
        environmentIdentifier = pEnvironmentIdentifier_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | Deletes a Refactor Spaces service.
--
-- The @RefactorSpacesSecurityGroup@ security group must be removed from
-- all Amazon Web Services resources in the virtual private cloud (VPC)
-- prior to deleting a service with a URL endpoint in a VPC.
deleteService_applicationIdentifier :: Lens.Lens' DeleteService Prelude.Text
deleteService_applicationIdentifier = Lens.lens (\DeleteService' {applicationIdentifier} -> applicationIdentifier) (\s@DeleteService' {} a -> s {applicationIdentifier = a} :: DeleteService)

-- | The ID of the environment that the service is in.
deleteService_environmentIdentifier :: Lens.Lens' DeleteService Prelude.Text
deleteService_environmentIdentifier = Lens.lens (\DeleteService' {environmentIdentifier} -> environmentIdentifier) (\s@DeleteService' {} a -> s {environmentIdentifier = a} :: DeleteService)

-- | The ID of the service to delete.
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
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "LastUpdatedTime")
            Prelude.<*> (x Core..?> "EnvironmentId")
            Prelude.<*> (x Core..?> "ApplicationId")
            Prelude.<*> (x Core..?> "ServiceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteService where
  hashWithSalt _salt DeleteService' {..} =
    _salt `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` environmentIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData DeleteService where
  rnf DeleteService' {..} =
    Prelude.rnf applicationIdentifier
      `Prelude.seq` Prelude.rnf environmentIdentifier
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Core.ToHeaders DeleteService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteService where
  toPath DeleteService' {..} =
    Prelude.mconcat
      [ "/environments/",
        Core.toBS environmentIdentifier,
        "/applications/",
        Core.toBS applicationIdentifier,
        "/services/",
        Core.toBS serviceIdentifier
      ]

instance Core.ToQuery DeleteService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceResponse' smart constructor.
data DeleteServiceResponse = DeleteServiceResponse'
  { -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the service.
    state :: Prelude.Maybe ServiceState,
    -- | A timestamp that indicates when the service was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application that the service is in.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
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
-- 'name', 'deleteServiceResponse_name' - The name of the service.
--
-- 'arn', 'deleteServiceResponse_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'state', 'deleteServiceResponse_state' - The current state of the service.
--
-- 'lastUpdatedTime', 'deleteServiceResponse_lastUpdatedTime' - A timestamp that indicates when the service was last updated.
--
-- 'environmentId', 'deleteServiceResponse_environmentId' - The unique identifier of the environment.
--
-- 'applicationId', 'deleteServiceResponse_applicationId' - The ID of the application that the service is in.
--
-- 'serviceId', 'deleteServiceResponse_serviceId' - The unique identifier of the service.
--
-- 'httpStatus', 'deleteServiceResponse_httpStatus' - The response's http status code.
newDeleteServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceResponse
newDeleteServiceResponse pHttpStatus_ =
  DeleteServiceResponse'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the service.
deleteServiceResponse_name :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.Text)
deleteServiceResponse_name = Lens.lens (\DeleteServiceResponse' {name} -> name) (\s@DeleteServiceResponse' {} a -> s {name = a} :: DeleteServiceResponse)

-- | The Amazon Resource Name (ARN) of the service.
deleteServiceResponse_arn :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.Text)
deleteServiceResponse_arn = Lens.lens (\DeleteServiceResponse' {arn} -> arn) (\s@DeleteServiceResponse' {} a -> s {arn = a} :: DeleteServiceResponse)

-- | The current state of the service.
deleteServiceResponse_state :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe ServiceState)
deleteServiceResponse_state = Lens.lens (\DeleteServiceResponse' {state} -> state) (\s@DeleteServiceResponse' {} a -> s {state = a} :: DeleteServiceResponse)

-- | A timestamp that indicates when the service was last updated.
deleteServiceResponse_lastUpdatedTime :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.UTCTime)
deleteServiceResponse_lastUpdatedTime = Lens.lens (\DeleteServiceResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@DeleteServiceResponse' {} a -> s {lastUpdatedTime = a} :: DeleteServiceResponse) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of the environment.
deleteServiceResponse_environmentId :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.Text)
deleteServiceResponse_environmentId = Lens.lens (\DeleteServiceResponse' {environmentId} -> environmentId) (\s@DeleteServiceResponse' {} a -> s {environmentId = a} :: DeleteServiceResponse)

-- | The ID of the application that the service is in.
deleteServiceResponse_applicationId :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.Text)
deleteServiceResponse_applicationId = Lens.lens (\DeleteServiceResponse' {applicationId} -> applicationId) (\s@DeleteServiceResponse' {} a -> s {applicationId = a} :: DeleteServiceResponse)

-- | The unique identifier of the service.
deleteServiceResponse_serviceId :: Lens.Lens' DeleteServiceResponse (Prelude.Maybe Prelude.Text)
deleteServiceResponse_serviceId = Lens.lens (\DeleteServiceResponse' {serviceId} -> serviceId) (\s@DeleteServiceResponse' {} a -> s {serviceId = a} :: DeleteServiceResponse)

-- | The response's http status code.
deleteServiceResponse_httpStatus :: Lens.Lens' DeleteServiceResponse Prelude.Int
deleteServiceResponse_httpStatus = Lens.lens (\DeleteServiceResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceResponse' {} a -> s {httpStatus = a} :: DeleteServiceResponse)

instance Prelude.NFData DeleteServiceResponse where
  rnf DeleteServiceResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf httpStatus
