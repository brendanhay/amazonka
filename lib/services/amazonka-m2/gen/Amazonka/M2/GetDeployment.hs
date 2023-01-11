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
-- Module      : Amazonka.M2.GetDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of a specific deployment with a given deployment
-- identifier.
module Amazonka.M2.GetDeployment
  ( -- * Creating a Request
    GetDeployment (..),
    newGetDeployment,

    -- * Request Lenses
    getDeployment_applicationId,
    getDeployment_deploymentId,

    -- * Destructuring the Response
    GetDeploymentResponse (..),
    newGetDeploymentResponse,

    -- * Response Lenses
    getDeploymentResponse_statusReason,
    getDeploymentResponse_httpStatus,
    getDeploymentResponse_applicationId,
    getDeploymentResponse_applicationVersion,
    getDeploymentResponse_creationTime,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_environmentId,
    getDeploymentResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { -- | The unique identifier of the application.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the deployment.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getDeployment_applicationId' - The unique identifier of the application.
--
-- 'deploymentId', 'getDeployment_deploymentId' - The unique identifier for the deployment.
newGetDeployment ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'deploymentId'
  Prelude.Text ->
  GetDeployment
newGetDeployment pApplicationId_ pDeploymentId_ =
  GetDeployment'
    { applicationId = pApplicationId_,
      deploymentId = pDeploymentId_
    }

-- | The unique identifier of the application.
getDeployment_applicationId :: Lens.Lens' GetDeployment Prelude.Text
getDeployment_applicationId = Lens.lens (\GetDeployment' {applicationId} -> applicationId) (\s@GetDeployment' {} a -> s {applicationId = a} :: GetDeployment)

-- | The unique identifier for the deployment.
getDeployment_deploymentId :: Lens.Lens' GetDeployment Prelude.Text
getDeployment_deploymentId = Lens.lens (\GetDeployment' {deploymentId} -> deploymentId) (\s@GetDeployment' {} a -> s {deploymentId = a} :: GetDeployment)

instance Core.AWSRequest GetDeployment where
  type
    AWSResponse GetDeployment =
      GetDeploymentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentResponse'
            Prelude.<$> (x Data..?> "statusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "applicationId")
            Prelude.<*> (x Data..:> "applicationVersion")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "deploymentId")
            Prelude.<*> (x Data..:> "environmentId")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetDeployment where
  hashWithSalt _salt GetDeployment' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData GetDeployment where
  rnf GetDeployment' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf deploymentId

instance Data.ToHeaders GetDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDeployment where
  toPath GetDeployment' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/deployments/",
        Data.toBS deploymentId
      ]

instance Data.ToQuery GetDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { -- | The reason for the reported status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of the application.
    applicationId :: Prelude.Text,
    -- | The application version.
    applicationVersion :: Prelude.Natural,
    -- | The timestamp when the deployment was created.
    creationTime :: Data.POSIX,
    -- | The unique identifier of the deployment.
    deploymentId :: Prelude.Text,
    -- | The unique identifier of the runtime environment.
    environmentId :: Prelude.Text,
    -- | The status of the deployment.
    status :: DeploymentLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'getDeploymentResponse_statusReason' - The reason for the reported status.
--
-- 'httpStatus', 'getDeploymentResponse_httpStatus' - The response's http status code.
--
-- 'applicationId', 'getDeploymentResponse_applicationId' - The unique identifier of the application.
--
-- 'applicationVersion', 'getDeploymentResponse_applicationVersion' - The application version.
--
-- 'creationTime', 'getDeploymentResponse_creationTime' - The timestamp when the deployment was created.
--
-- 'deploymentId', 'getDeploymentResponse_deploymentId' - The unique identifier of the deployment.
--
-- 'environmentId', 'getDeploymentResponse_environmentId' - The unique identifier of the runtime environment.
--
-- 'status', 'getDeploymentResponse_status' - The status of the deployment.
newGetDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationVersion'
  Prelude.Natural ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'deploymentId'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'status'
  DeploymentLifecycle ->
  GetDeploymentResponse
newGetDeploymentResponse
  pHttpStatus_
  pApplicationId_
  pApplicationVersion_
  pCreationTime_
  pDeploymentId_
  pEnvironmentId_
  pStatus_ =
    GetDeploymentResponse'
      { statusReason =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        applicationId = pApplicationId_,
        applicationVersion = pApplicationVersion_,
        creationTime = Data._Time Lens.# pCreationTime_,
        deploymentId = pDeploymentId_,
        environmentId = pEnvironmentId_,
        status = pStatus_
      }

-- | The reason for the reported status.
getDeploymentResponse_statusReason :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_statusReason = Lens.lens (\GetDeploymentResponse' {statusReason} -> statusReason) (\s@GetDeploymentResponse' {} a -> s {statusReason = a} :: GetDeploymentResponse)

-- | The response's http status code.
getDeploymentResponse_httpStatus :: Lens.Lens' GetDeploymentResponse Prelude.Int
getDeploymentResponse_httpStatus = Lens.lens (\GetDeploymentResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentResponse' {} a -> s {httpStatus = a} :: GetDeploymentResponse)

-- | The unique identifier of the application.
getDeploymentResponse_applicationId :: Lens.Lens' GetDeploymentResponse Prelude.Text
getDeploymentResponse_applicationId = Lens.lens (\GetDeploymentResponse' {applicationId} -> applicationId) (\s@GetDeploymentResponse' {} a -> s {applicationId = a} :: GetDeploymentResponse)

-- | The application version.
getDeploymentResponse_applicationVersion :: Lens.Lens' GetDeploymentResponse Prelude.Natural
getDeploymentResponse_applicationVersion = Lens.lens (\GetDeploymentResponse' {applicationVersion} -> applicationVersion) (\s@GetDeploymentResponse' {} a -> s {applicationVersion = a} :: GetDeploymentResponse)

-- | The timestamp when the deployment was created.
getDeploymentResponse_creationTime :: Lens.Lens' GetDeploymentResponse Prelude.UTCTime
getDeploymentResponse_creationTime = Lens.lens (\GetDeploymentResponse' {creationTime} -> creationTime) (\s@GetDeploymentResponse' {} a -> s {creationTime = a} :: GetDeploymentResponse) Prelude.. Data._Time

-- | The unique identifier of the deployment.
getDeploymentResponse_deploymentId :: Lens.Lens' GetDeploymentResponse Prelude.Text
getDeploymentResponse_deploymentId = Lens.lens (\GetDeploymentResponse' {deploymentId} -> deploymentId) (\s@GetDeploymentResponse' {} a -> s {deploymentId = a} :: GetDeploymentResponse)

-- | The unique identifier of the runtime environment.
getDeploymentResponse_environmentId :: Lens.Lens' GetDeploymentResponse Prelude.Text
getDeploymentResponse_environmentId = Lens.lens (\GetDeploymentResponse' {environmentId} -> environmentId) (\s@GetDeploymentResponse' {} a -> s {environmentId = a} :: GetDeploymentResponse)

-- | The status of the deployment.
getDeploymentResponse_status :: Lens.Lens' GetDeploymentResponse DeploymentLifecycle
getDeploymentResponse_status = Lens.lens (\GetDeploymentResponse' {status} -> status) (\s@GetDeploymentResponse' {} a -> s {status = a} :: GetDeploymentResponse)

instance Prelude.NFData GetDeploymentResponse where
  rnf GetDeploymentResponse' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf status
