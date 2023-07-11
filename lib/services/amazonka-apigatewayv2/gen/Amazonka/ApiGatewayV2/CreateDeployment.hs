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
-- Module      : Amazonka.ApiGatewayV2.CreateDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Deployment for an API.
module Amazonka.ApiGatewayV2.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_description,
    createDeployment_stageName,
    createDeployment_apiId,

    -- * Destructuring the Response
    CreateDeploymentResponse (..),
    newCreateDeploymentResponse,

    -- * Response Lenses
    createDeploymentResponse_autoDeployed,
    createDeploymentResponse_createdDate,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_deploymentStatus,
    createDeploymentResponse_deploymentStatusMessage,
    createDeploymentResponse_description,
    createDeploymentResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new Deployment resource to represent a deployment.
--
-- /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | The description for the deployment resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the Stage resource for the Deployment resource to create.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createDeployment_description' - The description for the deployment resource.
--
-- 'stageName', 'createDeployment_stageName' - The name of the Stage resource for the Deployment resource to create.
--
-- 'apiId', 'createDeployment_apiId' - The API identifier.
newCreateDeployment ::
  -- | 'apiId'
  Prelude.Text ->
  CreateDeployment
newCreateDeployment pApiId_ =
  CreateDeployment'
    { description = Prelude.Nothing,
      stageName = Prelude.Nothing,
      apiId = pApiId_
    }

-- | The description for the deployment resource.
createDeployment_description :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_description = Lens.lens (\CreateDeployment' {description} -> description) (\s@CreateDeployment' {} a -> s {description = a} :: CreateDeployment)

-- | The name of the Stage resource for the Deployment resource to create.
createDeployment_stageName :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_stageName = Lens.lens (\CreateDeployment' {stageName} -> stageName) (\s@CreateDeployment' {} a -> s {stageName = a} :: CreateDeployment)

-- | The API identifier.
createDeployment_apiId :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_apiId = Lens.lens (\CreateDeployment' {apiId} -> apiId) (\s@CreateDeployment' {} a -> s {apiId = a} :: CreateDeployment)

instance Core.AWSRequest CreateDeployment where
  type
    AWSResponse CreateDeployment =
      CreateDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Prelude.<$> (x Data..?> "autoDeployed")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "deploymentId")
            Prelude.<*> (x Data..?> "deploymentStatus")
            Prelude.<*> (x Data..?> "deploymentStatusMessage")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeployment where
  hashWithSalt _salt CreateDeployment' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData CreateDeployment where
  rnf CreateDeployment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders CreateDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("stageName" Data..=) Prelude.<$> stageName
          ]
      )

instance Data.ToPath CreateDeployment where
  toPath CreateDeployment' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/deployments"]

instance Data.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | Specifies whether a deployment was automatically released.
    autoDeployed :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the Deployment resource was created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The identifier for the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
    deploymentStatus :: Prelude.Maybe DeploymentStatus,
    -- | May contain additional feedback on the status of an API deployment.
    deploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The description for the deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoDeployed', 'createDeploymentResponse_autoDeployed' - Specifies whether a deployment was automatically released.
--
-- 'createdDate', 'createDeploymentResponse_createdDate' - The date and time when the Deployment resource was created.
--
-- 'deploymentId', 'createDeploymentResponse_deploymentId' - The identifier for the deployment.
--
-- 'deploymentStatus', 'createDeploymentResponse_deploymentStatus' - The status of the deployment: PENDING, FAILED, or SUCCEEDED.
--
-- 'deploymentStatusMessage', 'createDeploymentResponse_deploymentStatusMessage' - May contain additional feedback on the status of an API deployment.
--
-- 'description', 'createDeploymentResponse_description' - The description for the deployment.
--
-- 'httpStatus', 'createDeploymentResponse_httpStatus' - The response's http status code.
newCreateDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeploymentResponse
newCreateDeploymentResponse pHttpStatus_ =
  CreateDeploymentResponse'
    { autoDeployed =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      deploymentStatusMessage = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies whether a deployment was automatically released.
createDeploymentResponse_autoDeployed :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Bool)
createDeploymentResponse_autoDeployed = Lens.lens (\CreateDeploymentResponse' {autoDeployed} -> autoDeployed) (\s@CreateDeploymentResponse' {} a -> s {autoDeployed = a} :: CreateDeploymentResponse)

-- | The date and time when the Deployment resource was created.
createDeploymentResponse_createdDate :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.UTCTime)
createDeploymentResponse_createdDate = Lens.lens (\CreateDeploymentResponse' {createdDate} -> createdDate) (\s@CreateDeploymentResponse' {} a -> s {createdDate = a} :: CreateDeploymentResponse) Prelude.. Lens.mapping Data._Time

-- | The identifier for the deployment.
createDeploymentResponse_deploymentId :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_deploymentId = Lens.lens (\CreateDeploymentResponse' {deploymentId} -> deploymentId) (\s@CreateDeploymentResponse' {} a -> s {deploymentId = a} :: CreateDeploymentResponse)

-- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
createDeploymentResponse_deploymentStatus :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe DeploymentStatus)
createDeploymentResponse_deploymentStatus = Lens.lens (\CreateDeploymentResponse' {deploymentStatus} -> deploymentStatus) (\s@CreateDeploymentResponse' {} a -> s {deploymentStatus = a} :: CreateDeploymentResponse)

-- | May contain additional feedback on the status of an API deployment.
createDeploymentResponse_deploymentStatusMessage :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_deploymentStatusMessage = Lens.lens (\CreateDeploymentResponse' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@CreateDeploymentResponse' {} a -> s {deploymentStatusMessage = a} :: CreateDeploymentResponse)

-- | The description for the deployment.
createDeploymentResponse_description :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_description = Lens.lens (\CreateDeploymentResponse' {description} -> description) (\s@CreateDeploymentResponse' {} a -> s {description = a} :: CreateDeploymentResponse)

-- | The response's http status code.
createDeploymentResponse_httpStatus :: Lens.Lens' CreateDeploymentResponse Prelude.Int
createDeploymentResponse_httpStatus = Lens.lens (\CreateDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentResponse' {} a -> s {httpStatus = a} :: CreateDeploymentResponse)

instance Prelude.NFData CreateDeploymentResponse where
  rnf CreateDeploymentResponse' {..} =
    Prelude.rnf autoDeployed
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
