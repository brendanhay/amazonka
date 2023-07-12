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
-- Module      : Amazonka.APIGateway.CreateDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Deployment resource, which makes a specified RestApi callable
-- over the internet.
module Amazonka.APIGateway.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_cacheClusterEnabled,
    createDeployment_cacheClusterSize,
    createDeployment_canarySettings,
    createDeployment_description,
    createDeployment_stageDescription,
    createDeployment_stageName,
    createDeployment_tracingEnabled,
    createDeployment_variables,
    createDeployment_restApiId,

    -- * Destructuring the Response
    Deployment (..),
    newDeployment,

    -- * Response Lenses
    deployment_apiSummary,
    deployment_createdDate,
    deployment_description,
    deployment_id,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to create a Deployment resource.
--
-- /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | Enables a cache cluster for the Stage resource specified in the input.
    cacheClusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The stage\'s cache capacity in GB. For more information about choosing a
    -- cache size, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-caching.html Enabling API caching to enhance responsiveness>.
    cacheClusterSize :: Prelude.Maybe CacheClusterSize,
    -- | The input configuration for the canary deployment when the deployment is
    -- a canary release deployment.
    canarySettings :: Prelude.Maybe DeploymentCanarySettings,
    -- | The description for the Deployment resource to create.
    description :: Prelude.Maybe Prelude.Text,
    -- | The description of the Stage resource for the Deployment resource to
    -- create.
    stageDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the Stage resource for the Deployment resource to create.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether active tracing with X-ray is enabled for the Stage.
    tracingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A map that defines the stage variables for the Stage resource that is
    -- associated with the new deployment. Variable names can have alphanumeric
    -- and underscore characters, and the values must match
    -- @[A-Za-z0-9-._~:\/?#&=,]+@.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
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
-- 'cacheClusterEnabled', 'createDeployment_cacheClusterEnabled' - Enables a cache cluster for the Stage resource specified in the input.
--
-- 'cacheClusterSize', 'createDeployment_cacheClusterSize' - The stage\'s cache capacity in GB. For more information about choosing a
-- cache size, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-caching.html Enabling API caching to enhance responsiveness>.
--
-- 'canarySettings', 'createDeployment_canarySettings' - The input configuration for the canary deployment when the deployment is
-- a canary release deployment.
--
-- 'description', 'createDeployment_description' - The description for the Deployment resource to create.
--
-- 'stageDescription', 'createDeployment_stageDescription' - The description of the Stage resource for the Deployment resource to
-- create.
--
-- 'stageName', 'createDeployment_stageName' - The name of the Stage resource for the Deployment resource to create.
--
-- 'tracingEnabled', 'createDeployment_tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the Stage.
--
-- 'variables', 'createDeployment_variables' - A map that defines the stage variables for the Stage resource that is
-- associated with the new deployment. Variable names can have alphanumeric
-- and underscore characters, and the values must match
-- @[A-Za-z0-9-._~:\/?#&=,]+@.
--
-- 'restApiId', 'createDeployment_restApiId' - The string identifier of the associated RestApi.
newCreateDeployment ::
  -- | 'restApiId'
  Prelude.Text ->
  CreateDeployment
newCreateDeployment pRestApiId_ =
  CreateDeployment'
    { cacheClusterEnabled =
        Prelude.Nothing,
      cacheClusterSize = Prelude.Nothing,
      canarySettings = Prelude.Nothing,
      description = Prelude.Nothing,
      stageDescription = Prelude.Nothing,
      stageName = Prelude.Nothing,
      tracingEnabled = Prelude.Nothing,
      variables = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | Enables a cache cluster for the Stage resource specified in the input.
createDeployment_cacheClusterEnabled :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Bool)
createDeployment_cacheClusterEnabled = Lens.lens (\CreateDeployment' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@CreateDeployment' {} a -> s {cacheClusterEnabled = a} :: CreateDeployment)

-- | The stage\'s cache capacity in GB. For more information about choosing a
-- cache size, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-caching.html Enabling API caching to enhance responsiveness>.
createDeployment_cacheClusterSize :: Lens.Lens' CreateDeployment (Prelude.Maybe CacheClusterSize)
createDeployment_cacheClusterSize = Lens.lens (\CreateDeployment' {cacheClusterSize} -> cacheClusterSize) (\s@CreateDeployment' {} a -> s {cacheClusterSize = a} :: CreateDeployment)

-- | The input configuration for the canary deployment when the deployment is
-- a canary release deployment.
createDeployment_canarySettings :: Lens.Lens' CreateDeployment (Prelude.Maybe DeploymentCanarySettings)
createDeployment_canarySettings = Lens.lens (\CreateDeployment' {canarySettings} -> canarySettings) (\s@CreateDeployment' {} a -> s {canarySettings = a} :: CreateDeployment)

-- | The description for the Deployment resource to create.
createDeployment_description :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_description = Lens.lens (\CreateDeployment' {description} -> description) (\s@CreateDeployment' {} a -> s {description = a} :: CreateDeployment)

-- | The description of the Stage resource for the Deployment resource to
-- create.
createDeployment_stageDescription :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_stageDescription = Lens.lens (\CreateDeployment' {stageDescription} -> stageDescription) (\s@CreateDeployment' {} a -> s {stageDescription = a} :: CreateDeployment)

-- | The name of the Stage resource for the Deployment resource to create.
createDeployment_stageName :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_stageName = Lens.lens (\CreateDeployment' {stageName} -> stageName) (\s@CreateDeployment' {} a -> s {stageName = a} :: CreateDeployment)

-- | Specifies whether active tracing with X-ray is enabled for the Stage.
createDeployment_tracingEnabled :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Bool)
createDeployment_tracingEnabled = Lens.lens (\CreateDeployment' {tracingEnabled} -> tracingEnabled) (\s@CreateDeployment' {} a -> s {tracingEnabled = a} :: CreateDeployment)

-- | A map that defines the stage variables for the Stage resource that is
-- associated with the new deployment. Variable names can have alphanumeric
-- and underscore characters, and the values must match
-- @[A-Za-z0-9-._~:\/?#&=,]+@.
createDeployment_variables :: Lens.Lens' CreateDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeployment_variables = Lens.lens (\CreateDeployment' {variables} -> variables) (\s@CreateDeployment' {} a -> s {variables = a} :: CreateDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
createDeployment_restApiId :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_restApiId = Lens.lens (\CreateDeployment' {restApiId} -> restApiId) (\s@CreateDeployment' {} a -> s {restApiId = a} :: CreateDeployment)

instance Core.AWSRequest CreateDeployment where
  type AWSResponse CreateDeployment = Deployment
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateDeployment where
  hashWithSalt _salt CreateDeployment' {..} =
    _salt
      `Prelude.hashWithSalt` cacheClusterEnabled
      `Prelude.hashWithSalt` cacheClusterSize
      `Prelude.hashWithSalt` canarySettings
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stageDescription
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` tracingEnabled
      `Prelude.hashWithSalt` variables
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData CreateDeployment where
  rnf CreateDeployment' {..} =
    Prelude.rnf cacheClusterEnabled
      `Prelude.seq` Prelude.rnf cacheClusterSize
      `Prelude.seq` Prelude.rnf canarySettings
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf stageDescription
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf tracingEnabled
      `Prelude.seq` Prelude.rnf variables
      `Prelude.seq` Prelude.rnf restApiId

instance Data.ToHeaders CreateDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cacheClusterEnabled" Data..=)
              Prelude.<$> cacheClusterEnabled,
            ("cacheClusterSize" Data..=)
              Prelude.<$> cacheClusterSize,
            ("canarySettings" Data..=)
              Prelude.<$> canarySettings,
            ("description" Data..=) Prelude.<$> description,
            ("stageDescription" Data..=)
              Prelude.<$> stageDescription,
            ("stageName" Data..=) Prelude.<$> stageName,
            ("tracingEnabled" Data..=)
              Prelude.<$> tracingEnabled,
            ("variables" Data..=) Prelude.<$> variables
          ]
      )

instance Data.ToPath CreateDeployment where
  toPath CreateDeployment' {..} =
    Prelude.mconcat
      ["/restapis/", Data.toBS restApiId, "/deployments"]

instance Data.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty
