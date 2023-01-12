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
-- Module      : Amazonka.M2.CreateDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and starts a deployment to deploy an application into a runtime
-- environment.
module Amazonka.M2.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_clientToken,
    createDeployment_applicationId,
    createDeployment_applicationVersion,
    createDeployment_environmentId,

    -- * Destructuring the Response
    CreateDeploymentResponse (..),
    newCreateDeploymentResponse,

    -- * Response Lenses
    createDeploymentResponse_httpStatus,
    createDeploymentResponse_deploymentId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request to create a deployment. The service generates the
    -- clientToken when the API call is triggered. The token expires after one
    -- hour, so if you retry the API within this timeframe with the same
    -- clientToken, you will get the same response. The service also handles
    -- deleting the clientToken after it expires.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The application identifier.
    applicationId :: Prelude.Text,
    -- | The version of the application to deploy.
    applicationVersion :: Prelude.Natural,
    -- | The identifier of the runtime environment where you want to deploy this
    -- application.
    environmentId :: Prelude.Text
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
-- 'clientToken', 'createDeployment_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request to create a deployment. The service generates the
-- clientToken when the API call is triggered. The token expires after one
-- hour, so if you retry the API within this timeframe with the same
-- clientToken, you will get the same response. The service also handles
-- deleting the clientToken after it expires.
--
-- 'applicationId', 'createDeployment_applicationId' - The application identifier.
--
-- 'applicationVersion', 'createDeployment_applicationVersion' - The version of the application to deploy.
--
-- 'environmentId', 'createDeployment_environmentId' - The identifier of the runtime environment where you want to deploy this
-- application.
newCreateDeployment ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationVersion'
  Prelude.Natural ->
  -- | 'environmentId'
  Prelude.Text ->
  CreateDeployment
newCreateDeployment
  pApplicationId_
  pApplicationVersion_
  pEnvironmentId_ =
    CreateDeployment'
      { clientToken = Prelude.Nothing,
        applicationId = pApplicationId_,
        applicationVersion = pApplicationVersion_,
        environmentId = pEnvironmentId_
      }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request to create a deployment. The service generates the
-- clientToken when the API call is triggered. The token expires after one
-- hour, so if you retry the API within this timeframe with the same
-- clientToken, you will get the same response. The service also handles
-- deleting the clientToken after it expires.
createDeployment_clientToken :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_clientToken = Lens.lens (\CreateDeployment' {clientToken} -> clientToken) (\s@CreateDeployment' {} a -> s {clientToken = a} :: CreateDeployment)

-- | The application identifier.
createDeployment_applicationId :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_applicationId = Lens.lens (\CreateDeployment' {applicationId} -> applicationId) (\s@CreateDeployment' {} a -> s {applicationId = a} :: CreateDeployment)

-- | The version of the application to deploy.
createDeployment_applicationVersion :: Lens.Lens' CreateDeployment Prelude.Natural
createDeployment_applicationVersion = Lens.lens (\CreateDeployment' {applicationVersion} -> applicationVersion) (\s@CreateDeployment' {} a -> s {applicationVersion = a} :: CreateDeployment)

-- | The identifier of the runtime environment where you want to deploy this
-- application.
createDeployment_environmentId :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_environmentId = Lens.lens (\CreateDeployment' {environmentId} -> environmentId) (\s@CreateDeployment' {} a -> s {environmentId = a} :: CreateDeployment)

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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "deploymentId")
      )

instance Prelude.Hashable CreateDeployment where
  hashWithSalt _salt CreateDeployment' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData CreateDeployment where
  rnf CreateDeployment' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf environmentId

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
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("applicationVersion" Data..= applicationVersion),
            Prelude.Just
              ("environmentId" Data..= environmentId)
          ]
      )

instance Data.ToPath CreateDeployment where
  toPath CreateDeployment' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/deployments"
      ]

instance Data.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of the deployment.
    deploymentId :: Prelude.Text
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
-- 'httpStatus', 'createDeploymentResponse_httpStatus' - The response's http status code.
--
-- 'deploymentId', 'createDeploymentResponse_deploymentId' - The unique identifier of the deployment.
newCreateDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deploymentId'
  Prelude.Text ->
  CreateDeploymentResponse
newCreateDeploymentResponse
  pHttpStatus_
  pDeploymentId_ =
    CreateDeploymentResponse'
      { httpStatus =
          pHttpStatus_,
        deploymentId = pDeploymentId_
      }

-- | The response's http status code.
createDeploymentResponse_httpStatus :: Lens.Lens' CreateDeploymentResponse Prelude.Int
createDeploymentResponse_httpStatus = Lens.lens (\CreateDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentResponse' {} a -> s {httpStatus = a} :: CreateDeploymentResponse)

-- | The unique identifier of the deployment.
createDeploymentResponse_deploymentId :: Lens.Lens' CreateDeploymentResponse Prelude.Text
createDeploymentResponse_deploymentId = Lens.lens (\CreateDeploymentResponse' {deploymentId} -> deploymentId) (\s@CreateDeploymentResponse' {} a -> s {deploymentId = a} :: CreateDeploymentResponse)

instance Prelude.NFData CreateDeploymentResponse where
  rnf CreateDeploymentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deploymentId
