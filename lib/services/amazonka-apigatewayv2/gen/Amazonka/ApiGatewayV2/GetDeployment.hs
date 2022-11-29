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
-- Module      : Amazonka.ApiGatewayV2.GetDeployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Deployment.
module Amazonka.ApiGatewayV2.GetDeployment
  ( -- * Creating a Request
    GetDeployment (..),
    newGetDeployment,

    -- * Request Lenses
    getDeployment_apiId,
    getDeployment_deploymentId,

    -- * Destructuring the Response
    GetDeploymentResponse (..),
    newGetDeploymentResponse,

    -- * Response Lenses
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_autoDeployed,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_description,
    getDeploymentResponse_deploymentStatusMessage,
    getDeploymentResponse_createdDate,
    getDeploymentResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The deployment ID.
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
-- 'apiId', 'getDeployment_apiId' - The API identifier.
--
-- 'deploymentId', 'getDeployment_deploymentId' - The deployment ID.
newGetDeployment ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'deploymentId'
  Prelude.Text ->
  GetDeployment
newGetDeployment pApiId_ pDeploymentId_ =
  GetDeployment'
    { apiId = pApiId_,
      deploymentId = pDeploymentId_
    }

-- | The API identifier.
getDeployment_apiId :: Lens.Lens' GetDeployment Prelude.Text
getDeployment_apiId = Lens.lens (\GetDeployment' {apiId} -> apiId) (\s@GetDeployment' {} a -> s {apiId = a} :: GetDeployment)

-- | The deployment ID.
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
            Prelude.<$> (x Core..?> "deploymentStatus")
            Prelude.<*> (x Core..?> "autoDeployed")
            Prelude.<*> (x Core..?> "deploymentId")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "deploymentStatusMessage")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployment where
  hashWithSalt _salt GetDeployment' {..} =
    _salt `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData GetDeployment where
  rnf GetDeployment' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf deploymentId

instance Core.ToHeaders GetDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDeployment where
  toPath GetDeployment' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/deployments/",
        Core.toBS deploymentId
      ]

instance Core.ToQuery GetDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { -- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
    deploymentStatus :: Prelude.Maybe DeploymentStatus,
    -- | Specifies whether a deployment was automatically released.
    autoDeployed :: Prelude.Maybe Prelude.Bool,
    -- | The identifier for the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The description for the deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | May contain additional feedback on the status of an API deployment.
    deploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the Deployment resource was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'deploymentStatus', 'getDeploymentResponse_deploymentStatus' - The status of the deployment: PENDING, FAILED, or SUCCEEDED.
--
-- 'autoDeployed', 'getDeploymentResponse_autoDeployed' - Specifies whether a deployment was automatically released.
--
-- 'deploymentId', 'getDeploymentResponse_deploymentId' - The identifier for the deployment.
--
-- 'description', 'getDeploymentResponse_description' - The description for the deployment.
--
-- 'deploymentStatusMessage', 'getDeploymentResponse_deploymentStatusMessage' - May contain additional feedback on the status of an API deployment.
--
-- 'createdDate', 'getDeploymentResponse_createdDate' - The date and time when the Deployment resource was created.
--
-- 'httpStatus', 'getDeploymentResponse_httpStatus' - The response's http status code.
newGetDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeploymentResponse
newGetDeploymentResponse pHttpStatus_ =
  GetDeploymentResponse'
    { deploymentStatus =
        Prelude.Nothing,
      autoDeployed = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      description = Prelude.Nothing,
      deploymentStatusMessage = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
getDeploymentResponse_deploymentStatus :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe DeploymentStatus)
getDeploymentResponse_deploymentStatus = Lens.lens (\GetDeploymentResponse' {deploymentStatus} -> deploymentStatus) (\s@GetDeploymentResponse' {} a -> s {deploymentStatus = a} :: GetDeploymentResponse)

-- | Specifies whether a deployment was automatically released.
getDeploymentResponse_autoDeployed :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Bool)
getDeploymentResponse_autoDeployed = Lens.lens (\GetDeploymentResponse' {autoDeployed} -> autoDeployed) (\s@GetDeploymentResponse' {} a -> s {autoDeployed = a} :: GetDeploymentResponse)

-- | The identifier for the deployment.
getDeploymentResponse_deploymentId :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_deploymentId = Lens.lens (\GetDeploymentResponse' {deploymentId} -> deploymentId) (\s@GetDeploymentResponse' {} a -> s {deploymentId = a} :: GetDeploymentResponse)

-- | The description for the deployment.
getDeploymentResponse_description :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_description = Lens.lens (\GetDeploymentResponse' {description} -> description) (\s@GetDeploymentResponse' {} a -> s {description = a} :: GetDeploymentResponse)

-- | May contain additional feedback on the status of an API deployment.
getDeploymentResponse_deploymentStatusMessage :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.Text)
getDeploymentResponse_deploymentStatusMessage = Lens.lens (\GetDeploymentResponse' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@GetDeploymentResponse' {} a -> s {deploymentStatusMessage = a} :: GetDeploymentResponse)

-- | The date and time when the Deployment resource was created.
getDeploymentResponse_createdDate :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe Prelude.UTCTime)
getDeploymentResponse_createdDate = Lens.lens (\GetDeploymentResponse' {createdDate} -> createdDate) (\s@GetDeploymentResponse' {} a -> s {createdDate = a} :: GetDeploymentResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getDeploymentResponse_httpStatus :: Lens.Lens' GetDeploymentResponse Prelude.Int
getDeploymentResponse_httpStatus = Lens.lens (\GetDeploymentResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentResponse' {} a -> s {httpStatus = a} :: GetDeploymentResponse)

instance Prelude.NFData GetDeploymentResponse where
  rnf GetDeploymentResponse' {..} =
    Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf autoDeployed
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf httpStatus
