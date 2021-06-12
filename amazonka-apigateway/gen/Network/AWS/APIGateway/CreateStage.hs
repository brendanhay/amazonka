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
-- Module      : Network.AWS.APIGateway.CreateStage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Stage resource that references a pre-existing Deployment
-- for the API.
module Network.AWS.APIGateway.CreateStage
  ( -- * Creating a Request
    CreateStage (..),
    newCreateStage,

    -- * Request Lenses
    createStage_tracingEnabled,
    createStage_cacheClusterEnabled,
    createStage_documentationVersion,
    createStage_variables,
    createStage_tags,
    createStage_description,
    createStage_canarySettings,
    createStage_cacheClusterSize,
    createStage_restApiId,
    createStage_stageName,
    createStage_deploymentId,

    -- * Destructuring the Response
    Stage (..),
    newStage,

    -- * Response Lenses
    stage_deploymentId,
    stage_createdDate,
    stage_tracingEnabled,
    stage_webAclArn,
    stage_lastUpdatedDate,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_documentationVersion,
    stage_variables,
    stage_accessLogSettings,
    stage_tags,
    stage_clientCertificateId,
    stage_description,
    stage_canarySettings,
    stage_cacheClusterSize,
    stage_methodSettings,
    stage_cacheClusterStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to create a Stage resource.
--
-- /See:/ 'newCreateStage' smart constructor.
data CreateStage = CreateStage'
  { -- | Specifies whether active tracing with X-ray is enabled for the Stage.
    tracingEnabled :: Core.Maybe Core.Bool,
    -- | Whether cache clustering is enabled for the stage.
    cacheClusterEnabled :: Core.Maybe Core.Bool,
    -- | The version of the associated API documentation.
    documentationVersion :: Core.Maybe Core.Text,
    -- | A map that defines the stage variables for the new Stage resource.
    -- Variable names can have alphanumeric and underscore characters, and the
    -- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
    variables :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the Stage resource.
    description :: Core.Maybe Core.Text,
    -- | The canary deployment settings of this stage.
    canarySettings :: Core.Maybe CanarySettings,
    -- | The stage\'s cache cluster size.
    cacheClusterSize :: Core.Maybe CacheClusterSize,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The name for the Stage resource. Stage names can only contain
    -- alphanumeric characters, hyphens, and underscores. Maximum length is 128
    -- characters.
    stageName :: Core.Text,
    -- | [Required] The identifier of the Deployment resource for the Stage
    -- resource.
    deploymentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tracingEnabled', 'createStage_tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the Stage.
--
-- 'cacheClusterEnabled', 'createStage_cacheClusterEnabled' - Whether cache clustering is enabled for the stage.
--
-- 'documentationVersion', 'createStage_documentationVersion' - The version of the associated API documentation.
--
-- 'variables', 'createStage_variables' - A map that defines the stage variables for the new Stage resource.
-- Variable names can have alphanumeric and underscore characters, and the
-- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
--
-- 'tags', 'createStage_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'description', 'createStage_description' - The description of the Stage resource.
--
-- 'canarySettings', 'createStage_canarySettings' - The canary deployment settings of this stage.
--
-- 'cacheClusterSize', 'createStage_cacheClusterSize' - The stage\'s cache cluster size.
--
-- 'restApiId', 'createStage_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'stageName', 'createStage_stageName' - [Required] The name for the Stage resource. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
--
-- 'deploymentId', 'createStage_deploymentId' - [Required] The identifier of the Deployment resource for the Stage
-- resource.
newCreateStage ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'stageName'
  Core.Text ->
  -- | 'deploymentId'
  Core.Text ->
  CreateStage
newCreateStage pRestApiId_ pStageName_ pDeploymentId_ =
  CreateStage'
    { tracingEnabled = Core.Nothing,
      cacheClusterEnabled = Core.Nothing,
      documentationVersion = Core.Nothing,
      variables = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      canarySettings = Core.Nothing,
      cacheClusterSize = Core.Nothing,
      restApiId = pRestApiId_,
      stageName = pStageName_,
      deploymentId = pDeploymentId_
    }

-- | Specifies whether active tracing with X-ray is enabled for the Stage.
createStage_tracingEnabled :: Lens.Lens' CreateStage (Core.Maybe Core.Bool)
createStage_tracingEnabled = Lens.lens (\CreateStage' {tracingEnabled} -> tracingEnabled) (\s@CreateStage' {} a -> s {tracingEnabled = a} :: CreateStage)

-- | Whether cache clustering is enabled for the stage.
createStage_cacheClusterEnabled :: Lens.Lens' CreateStage (Core.Maybe Core.Bool)
createStage_cacheClusterEnabled = Lens.lens (\CreateStage' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@CreateStage' {} a -> s {cacheClusterEnabled = a} :: CreateStage)

-- | The version of the associated API documentation.
createStage_documentationVersion :: Lens.Lens' CreateStage (Core.Maybe Core.Text)
createStage_documentationVersion = Lens.lens (\CreateStage' {documentationVersion} -> documentationVersion) (\s@CreateStage' {} a -> s {documentationVersion = a} :: CreateStage)

-- | A map that defines the stage variables for the new Stage resource.
-- Variable names can have alphanumeric and underscore characters, and the
-- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
createStage_variables :: Lens.Lens' CreateStage (Core.Maybe (Core.HashMap Core.Text Core.Text))
createStage_variables = Lens.lens (\CreateStage' {variables} -> variables) (\s@CreateStage' {} a -> s {variables = a} :: CreateStage) Core.. Lens.mapping Lens._Coerce

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createStage_tags :: Lens.Lens' CreateStage (Core.Maybe (Core.HashMap Core.Text Core.Text))
createStage_tags = Lens.lens (\CreateStage' {tags} -> tags) (\s@CreateStage' {} a -> s {tags = a} :: CreateStage) Core.. Lens.mapping Lens._Coerce

-- | The description of the Stage resource.
createStage_description :: Lens.Lens' CreateStage (Core.Maybe Core.Text)
createStage_description = Lens.lens (\CreateStage' {description} -> description) (\s@CreateStage' {} a -> s {description = a} :: CreateStage)

-- | The canary deployment settings of this stage.
createStage_canarySettings :: Lens.Lens' CreateStage (Core.Maybe CanarySettings)
createStage_canarySettings = Lens.lens (\CreateStage' {canarySettings} -> canarySettings) (\s@CreateStage' {} a -> s {canarySettings = a} :: CreateStage)

-- | The stage\'s cache cluster size.
createStage_cacheClusterSize :: Lens.Lens' CreateStage (Core.Maybe CacheClusterSize)
createStage_cacheClusterSize = Lens.lens (\CreateStage' {cacheClusterSize} -> cacheClusterSize) (\s@CreateStage' {} a -> s {cacheClusterSize = a} :: CreateStage)

-- | [Required] The string identifier of the associated RestApi.
createStage_restApiId :: Lens.Lens' CreateStage Core.Text
createStage_restApiId = Lens.lens (\CreateStage' {restApiId} -> restApiId) (\s@CreateStage' {} a -> s {restApiId = a} :: CreateStage)

-- | [Required] The name for the Stage resource. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
createStage_stageName :: Lens.Lens' CreateStage Core.Text
createStage_stageName = Lens.lens (\CreateStage' {stageName} -> stageName) (\s@CreateStage' {} a -> s {stageName = a} :: CreateStage)

-- | [Required] The identifier of the Deployment resource for the Stage
-- resource.
createStage_deploymentId :: Lens.Lens' CreateStage Core.Text
createStage_deploymentId = Lens.lens (\CreateStage' {deploymentId} -> deploymentId) (\s@CreateStage' {} a -> s {deploymentId = a} :: CreateStage)

instance Core.AWSRequest CreateStage where
  type AWSResponse CreateStage = Stage
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateStage

instance Core.NFData CreateStage

instance Core.ToHeaders CreateStage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateStage where
  toJSON CreateStage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tracingEnabled" Core..=) Core.<$> tracingEnabled,
            ("cacheClusterEnabled" Core..=)
              Core.<$> cacheClusterEnabled,
            ("documentationVersion" Core..=)
              Core.<$> documentationVersion,
            ("variables" Core..=) Core.<$> variables,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            ("canarySettings" Core..=) Core.<$> canarySettings,
            ("cacheClusterSize" Core..=)
              Core.<$> cacheClusterSize,
            Core.Just ("stageName" Core..= stageName),
            Core.Just ("deploymentId" Core..= deploymentId)
          ]
      )

instance Core.ToPath CreateStage where
  toPath CreateStage' {..} =
    Core.mconcat
      ["/restapis/", Core.toBS restApiId, "/stages"]

instance Core.ToQuery CreateStage where
  toQuery = Core.const Core.mempty
