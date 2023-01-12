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
-- Module      : Amazonka.Amplify.CreateBackendEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new backend environment for an Amplify app.
module Amazonka.Amplify.CreateBackendEnvironment
  ( -- * Creating a Request
    CreateBackendEnvironment (..),
    newCreateBackendEnvironment,

    -- * Request Lenses
    createBackendEnvironment_deploymentArtifacts,
    createBackendEnvironment_stackName,
    createBackendEnvironment_appId,
    createBackendEnvironment_environmentName,

    -- * Destructuring the Response
    CreateBackendEnvironmentResponse (..),
    newCreateBackendEnvironmentResponse,

    -- * Response Lenses
    createBackendEnvironmentResponse_httpStatus,
    createBackendEnvironmentResponse_backendEnvironment,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the backend environment create request.
--
-- /See:/ 'newCreateBackendEnvironment' smart constructor.
data CreateBackendEnvironment = CreateBackendEnvironment'
  { -- | The name of deployment artifacts.
    deploymentArtifacts :: Prelude.Maybe Prelude.Text,
    -- | The AWS CloudFormation stack name of a backend environment.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the backend environment.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentArtifacts', 'createBackendEnvironment_deploymentArtifacts' - The name of deployment artifacts.
--
-- 'stackName', 'createBackendEnvironment_stackName' - The AWS CloudFormation stack name of a backend environment.
--
-- 'appId', 'createBackendEnvironment_appId' - The unique ID for an Amplify app.
--
-- 'environmentName', 'createBackendEnvironment_environmentName' - The name for the backend environment.
newCreateBackendEnvironment ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  CreateBackendEnvironment
newCreateBackendEnvironment pAppId_ pEnvironmentName_ =
  CreateBackendEnvironment'
    { deploymentArtifacts =
        Prelude.Nothing,
      stackName = Prelude.Nothing,
      appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The name of deployment artifacts.
createBackendEnvironment_deploymentArtifacts :: Lens.Lens' CreateBackendEnvironment (Prelude.Maybe Prelude.Text)
createBackendEnvironment_deploymentArtifacts = Lens.lens (\CreateBackendEnvironment' {deploymentArtifacts} -> deploymentArtifacts) (\s@CreateBackendEnvironment' {} a -> s {deploymentArtifacts = a} :: CreateBackendEnvironment)

-- | The AWS CloudFormation stack name of a backend environment.
createBackendEnvironment_stackName :: Lens.Lens' CreateBackendEnvironment (Prelude.Maybe Prelude.Text)
createBackendEnvironment_stackName = Lens.lens (\CreateBackendEnvironment' {stackName} -> stackName) (\s@CreateBackendEnvironment' {} a -> s {stackName = a} :: CreateBackendEnvironment)

-- | The unique ID for an Amplify app.
createBackendEnvironment_appId :: Lens.Lens' CreateBackendEnvironment Prelude.Text
createBackendEnvironment_appId = Lens.lens (\CreateBackendEnvironment' {appId} -> appId) (\s@CreateBackendEnvironment' {} a -> s {appId = a} :: CreateBackendEnvironment)

-- | The name for the backend environment.
createBackendEnvironment_environmentName :: Lens.Lens' CreateBackendEnvironment Prelude.Text
createBackendEnvironment_environmentName = Lens.lens (\CreateBackendEnvironment' {environmentName} -> environmentName) (\s@CreateBackendEnvironment' {} a -> s {environmentName = a} :: CreateBackendEnvironment)

instance Core.AWSRequest CreateBackendEnvironment where
  type
    AWSResponse CreateBackendEnvironment =
      CreateBackendEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackendEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "backendEnvironment")
      )

instance Prelude.Hashable CreateBackendEnvironment where
  hashWithSalt _salt CreateBackendEnvironment' {..} =
    _salt `Prelude.hashWithSalt` deploymentArtifacts
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData CreateBackendEnvironment where
  rnf CreateBackendEnvironment' {..} =
    Prelude.rnf deploymentArtifacts
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders CreateBackendEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackendEnvironment where
  toJSON CreateBackendEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deploymentArtifacts" Data..=)
              Prelude.<$> deploymentArtifacts,
            ("stackName" Data..=) Prelude.<$> stackName,
            Prelude.Just
              ("environmentName" Data..= environmentName)
          ]
      )

instance Data.ToPath CreateBackendEnvironment where
  toPath CreateBackendEnvironment' {..} =
    Prelude.mconcat
      ["/apps/", Data.toBS appId, "/backendenvironments"]

instance Data.ToQuery CreateBackendEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the create backend environment request.
--
-- /See:/ 'newCreateBackendEnvironmentResponse' smart constructor.
data CreateBackendEnvironmentResponse = CreateBackendEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes the backend environment for an Amplify app.
    backendEnvironment :: BackendEnvironment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createBackendEnvironmentResponse_httpStatus' - The response's http status code.
--
-- 'backendEnvironment', 'createBackendEnvironmentResponse_backendEnvironment' - Describes the backend environment for an Amplify app.
newCreateBackendEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'backendEnvironment'
  BackendEnvironment ->
  CreateBackendEnvironmentResponse
newCreateBackendEnvironmentResponse
  pHttpStatus_
  pBackendEnvironment_ =
    CreateBackendEnvironmentResponse'
      { httpStatus =
          pHttpStatus_,
        backendEnvironment = pBackendEnvironment_
      }

-- | The response's http status code.
createBackendEnvironmentResponse_httpStatus :: Lens.Lens' CreateBackendEnvironmentResponse Prelude.Int
createBackendEnvironmentResponse_httpStatus = Lens.lens (\CreateBackendEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateBackendEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateBackendEnvironmentResponse)

-- | Describes the backend environment for an Amplify app.
createBackendEnvironmentResponse_backendEnvironment :: Lens.Lens' CreateBackendEnvironmentResponse BackendEnvironment
createBackendEnvironmentResponse_backendEnvironment = Lens.lens (\CreateBackendEnvironmentResponse' {backendEnvironment} -> backendEnvironment) (\s@CreateBackendEnvironmentResponse' {} a -> s {backendEnvironment = a} :: CreateBackendEnvironmentResponse)

instance
  Prelude.NFData
    CreateBackendEnvironmentResponse
  where
  rnf CreateBackendEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf backendEnvironment
