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
-- Module      : Network.AWS.APIGateway.CreateDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Deployment resource, which makes a specified RestApi callable
-- over the internet.
module Network.AWS.APIGateway.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_tracingEnabled,
    createDeployment_cacheClusterEnabled,
    createDeployment_stageName,
    createDeployment_variables,
    createDeployment_stageDescription,
    createDeployment_description,
    createDeployment_canarySettings,
    createDeployment_cacheClusterSize,
    createDeployment_restApiId,

    -- * Destructuring the Response
    Deployment (..),
    newDeployment,

    -- * Response Lenses
    deployment_createdDate,
    deployment_id,
    deployment_apiSummary,
    deployment_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to create a Deployment resource.
--
-- /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | Specifies whether active tracing with X-ray is enabled for the Stage.
    tracingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Enables a cache cluster for the Stage resource specified in the input.
    cacheClusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Stage resource for the Deployment resource to create.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | A map that defines the stage variables for the Stage resource that is
    -- associated with the new deployment. Variable names can have alphanumeric
    -- and underscore characters, and the values must match
    -- @[A-Za-z0-9-._~:\/?#&=,]+@.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the Stage resource for the Deployment resource to
    -- create.
    stageDescription :: Prelude.Maybe Prelude.Text,
    -- | The description for the Deployment resource to create.
    description :: Prelude.Maybe Prelude.Text,
    -- | The input configuration for the canary deployment when the deployment is
    -- a canary release deployment.
    canarySettings :: Prelude.Maybe DeploymentCanarySettings,
    -- | Specifies the cache cluster size for the Stage resource specified in the
    -- input, if a cache cluster is enabled.
    cacheClusterSize :: Prelude.Maybe CacheClusterSize,
    -- | [Required] The string identifier of the associated RestApi.
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
-- 'tracingEnabled', 'createDeployment_tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the Stage.
--
-- 'cacheClusterEnabled', 'createDeployment_cacheClusterEnabled' - Enables a cache cluster for the Stage resource specified in the input.
--
-- 'stageName', 'createDeployment_stageName' - The name of the Stage resource for the Deployment resource to create.
--
-- 'variables', 'createDeployment_variables' - A map that defines the stage variables for the Stage resource that is
-- associated with the new deployment. Variable names can have alphanumeric
-- and underscore characters, and the values must match
-- @[A-Za-z0-9-._~:\/?#&=,]+@.
--
-- 'stageDescription', 'createDeployment_stageDescription' - The description of the Stage resource for the Deployment resource to
-- create.
--
-- 'description', 'createDeployment_description' - The description for the Deployment resource to create.
--
-- 'canarySettings', 'createDeployment_canarySettings' - The input configuration for the canary deployment when the deployment is
-- a canary release deployment.
--
-- 'cacheClusterSize', 'createDeployment_cacheClusterSize' - Specifies the cache cluster size for the Stage resource specified in the
-- input, if a cache cluster is enabled.
--
-- 'restApiId', 'createDeployment_restApiId' - [Required] The string identifier of the associated RestApi.
newCreateDeployment ::
  -- | 'restApiId'
  Prelude.Text ->
  CreateDeployment
newCreateDeployment pRestApiId_ =
  CreateDeployment'
    { tracingEnabled = Prelude.Nothing,
      cacheClusterEnabled = Prelude.Nothing,
      stageName = Prelude.Nothing,
      variables = Prelude.Nothing,
      stageDescription = Prelude.Nothing,
      description = Prelude.Nothing,
      canarySettings = Prelude.Nothing,
      cacheClusterSize = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | Specifies whether active tracing with X-ray is enabled for the Stage.
createDeployment_tracingEnabled :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Bool)
createDeployment_tracingEnabled = Lens.lens (\CreateDeployment' {tracingEnabled} -> tracingEnabled) (\s@CreateDeployment' {} a -> s {tracingEnabled = a} :: CreateDeployment)

-- | Enables a cache cluster for the Stage resource specified in the input.
createDeployment_cacheClusterEnabled :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Bool)
createDeployment_cacheClusterEnabled = Lens.lens (\CreateDeployment' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@CreateDeployment' {} a -> s {cacheClusterEnabled = a} :: CreateDeployment)

-- | The name of the Stage resource for the Deployment resource to create.
createDeployment_stageName :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_stageName = Lens.lens (\CreateDeployment' {stageName} -> stageName) (\s@CreateDeployment' {} a -> s {stageName = a} :: CreateDeployment)

-- | A map that defines the stage variables for the Stage resource that is
-- associated with the new deployment. Variable names can have alphanumeric
-- and underscore characters, and the values must match
-- @[A-Za-z0-9-._~:\/?#&=,]+@.
createDeployment_variables :: Lens.Lens' CreateDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeployment_variables = Lens.lens (\CreateDeployment' {variables} -> variables) (\s@CreateDeployment' {} a -> s {variables = a} :: CreateDeployment) Prelude.. Lens.mapping Lens._Coerce

-- | The description of the Stage resource for the Deployment resource to
-- create.
createDeployment_stageDescription :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_stageDescription = Lens.lens (\CreateDeployment' {stageDescription} -> stageDescription) (\s@CreateDeployment' {} a -> s {stageDescription = a} :: CreateDeployment)

-- | The description for the Deployment resource to create.
createDeployment_description :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_description = Lens.lens (\CreateDeployment' {description} -> description) (\s@CreateDeployment' {} a -> s {description = a} :: CreateDeployment)

-- | The input configuration for the canary deployment when the deployment is
-- a canary release deployment.
createDeployment_canarySettings :: Lens.Lens' CreateDeployment (Prelude.Maybe DeploymentCanarySettings)
createDeployment_canarySettings = Lens.lens (\CreateDeployment' {canarySettings} -> canarySettings) (\s@CreateDeployment' {} a -> s {canarySettings = a} :: CreateDeployment)

-- | Specifies the cache cluster size for the Stage resource specified in the
-- input, if a cache cluster is enabled.
createDeployment_cacheClusterSize :: Lens.Lens' CreateDeployment (Prelude.Maybe CacheClusterSize)
createDeployment_cacheClusterSize = Lens.lens (\CreateDeployment' {cacheClusterSize} -> cacheClusterSize) (\s@CreateDeployment' {} a -> s {cacheClusterSize = a} :: CreateDeployment)

-- | [Required] The string identifier of the associated RestApi.
createDeployment_restApiId :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_restApiId = Lens.lens (\CreateDeployment' {restApiId} -> restApiId) (\s@CreateDeployment' {} a -> s {restApiId = a} :: CreateDeployment)

instance Core.AWSRequest CreateDeployment where
  type AWSResponse CreateDeployment = Deployment
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateDeployment

instance Prelude.NFData CreateDeployment

instance Core.ToHeaders CreateDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tracingEnabled" Core..=)
              Prelude.<$> tracingEnabled,
            ("cacheClusterEnabled" Core..=)
              Prelude.<$> cacheClusterEnabled,
            ("stageName" Core..=) Prelude.<$> stageName,
            ("variables" Core..=) Prelude.<$> variables,
            ("stageDescription" Core..=)
              Prelude.<$> stageDescription,
            ("description" Core..=) Prelude.<$> description,
            ("canarySettings" Core..=)
              Prelude.<$> canarySettings,
            ("cacheClusterSize" Core..=)
              Prelude.<$> cacheClusterSize
          ]
      )

instance Core.ToPath CreateDeployment where
  toPath CreateDeployment' {..} =
    Prelude.mconcat
      ["/restapis/", Core.toBS restApiId, "/deployments"]

instance Core.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty
