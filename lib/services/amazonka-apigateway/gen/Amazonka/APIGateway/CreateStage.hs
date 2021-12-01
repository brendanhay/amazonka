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
-- Module      : Amazonka.APIGateway.CreateStage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Stage resource that references a pre-existing Deployment
-- for the API.
module Amazonka.APIGateway.CreateStage
  ( -- * Creating a Request
    CreateStage (..),
    newCreateStage,

    -- * Request Lenses
    createStage_variables,
    createStage_documentationVersion,
    createStage_tracingEnabled,
    createStage_cacheClusterSize,
    createStage_canarySettings,
    createStage_cacheClusterEnabled,
    createStage_description,
    createStage_tags,
    createStage_restApiId,
    createStage_stageName,
    createStage_deploymentId,

    -- * Destructuring the Response
    Stage (..),
    newStage,

    -- * Response Lenses
    stage_deploymentId,
    stage_variables,
    stage_accessLogSettings,
    stage_documentationVersion,
    stage_clientCertificateId,
    stage_tracingEnabled,
    stage_createdDate,
    stage_cacheClusterStatus,
    stage_methodSettings,
    stage_lastUpdatedDate,
    stage_cacheClusterSize,
    stage_webAclArn,
    stage_canarySettings,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_description,
    stage_tags,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to create a Stage resource.
--
-- /See:/ 'newCreateStage' smart constructor.
data CreateStage = CreateStage'
  { -- | A map that defines the stage variables for the new Stage resource.
    -- Variable names can have alphanumeric and underscore characters, and the
    -- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the associated API documentation.
    documentationVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether active tracing with X-ray is enabled for the Stage.
    tracingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The stage\'s cache cluster size.
    cacheClusterSize :: Prelude.Maybe CacheClusterSize,
    -- | The canary deployment settings of this stage.
    canarySettings :: Prelude.Maybe CanarySettings,
    -- | Whether cache clustering is enabled for the stage.
    cacheClusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The description of the Stage resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The name for the Stage resource. Stage names can only contain
    -- alphanumeric characters, hyphens, and underscores. Maximum length is 128
    -- characters.
    stageName :: Prelude.Text,
    -- | [Required] The identifier of the Deployment resource for the Stage
    -- resource.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variables', 'createStage_variables' - A map that defines the stage variables for the new Stage resource.
-- Variable names can have alphanumeric and underscore characters, and the
-- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
--
-- 'documentationVersion', 'createStage_documentationVersion' - The version of the associated API documentation.
--
-- 'tracingEnabled', 'createStage_tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the Stage.
--
-- 'cacheClusterSize', 'createStage_cacheClusterSize' - The stage\'s cache cluster size.
--
-- 'canarySettings', 'createStage_canarySettings' - The canary deployment settings of this stage.
--
-- 'cacheClusterEnabled', 'createStage_cacheClusterEnabled' - Whether cache clustering is enabled for the stage.
--
-- 'description', 'createStage_description' - The description of the Stage resource.
--
-- 'tags', 'createStage_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
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
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  -- | 'deploymentId'
  Prelude.Text ->
  CreateStage
newCreateStage pRestApiId_ pStageName_ pDeploymentId_ =
  CreateStage'
    { variables = Prelude.Nothing,
      documentationVersion = Prelude.Nothing,
      tracingEnabled = Prelude.Nothing,
      cacheClusterSize = Prelude.Nothing,
      canarySettings = Prelude.Nothing,
      cacheClusterEnabled = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      restApiId = pRestApiId_,
      stageName = pStageName_,
      deploymentId = pDeploymentId_
    }

-- | A map that defines the stage variables for the new Stage resource.
-- Variable names can have alphanumeric and underscore characters, and the
-- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
createStage_variables :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStage_variables = Lens.lens (\CreateStage' {variables} -> variables) (\s@CreateStage' {} a -> s {variables = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | The version of the associated API documentation.
createStage_documentationVersion :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_documentationVersion = Lens.lens (\CreateStage' {documentationVersion} -> documentationVersion) (\s@CreateStage' {} a -> s {documentationVersion = a} :: CreateStage)

-- | Specifies whether active tracing with X-ray is enabled for the Stage.
createStage_tracingEnabled :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Bool)
createStage_tracingEnabled = Lens.lens (\CreateStage' {tracingEnabled} -> tracingEnabled) (\s@CreateStage' {} a -> s {tracingEnabled = a} :: CreateStage)

-- | The stage\'s cache cluster size.
createStage_cacheClusterSize :: Lens.Lens' CreateStage (Prelude.Maybe CacheClusterSize)
createStage_cacheClusterSize = Lens.lens (\CreateStage' {cacheClusterSize} -> cacheClusterSize) (\s@CreateStage' {} a -> s {cacheClusterSize = a} :: CreateStage)

-- | The canary deployment settings of this stage.
createStage_canarySettings :: Lens.Lens' CreateStage (Prelude.Maybe CanarySettings)
createStage_canarySettings = Lens.lens (\CreateStage' {canarySettings} -> canarySettings) (\s@CreateStage' {} a -> s {canarySettings = a} :: CreateStage)

-- | Whether cache clustering is enabled for the stage.
createStage_cacheClusterEnabled :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Bool)
createStage_cacheClusterEnabled = Lens.lens (\CreateStage' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@CreateStage' {} a -> s {cacheClusterEnabled = a} :: CreateStage)

-- | The description of the Stage resource.
createStage_description :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_description = Lens.lens (\CreateStage' {description} -> description) (\s@CreateStage' {} a -> s {description = a} :: CreateStage)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createStage_tags :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStage_tags = Lens.lens (\CreateStage' {tags} -> tags) (\s@CreateStage' {} a -> s {tags = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | [Required] The string identifier of the associated RestApi.
createStage_restApiId :: Lens.Lens' CreateStage Prelude.Text
createStage_restApiId = Lens.lens (\CreateStage' {restApiId} -> restApiId) (\s@CreateStage' {} a -> s {restApiId = a} :: CreateStage)

-- | [Required] The name for the Stage resource. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
createStage_stageName :: Lens.Lens' CreateStage Prelude.Text
createStage_stageName = Lens.lens (\CreateStage' {stageName} -> stageName) (\s@CreateStage' {} a -> s {stageName = a} :: CreateStage)

-- | [Required] The identifier of the Deployment resource for the Stage
-- resource.
createStage_deploymentId :: Lens.Lens' CreateStage Prelude.Text
createStage_deploymentId = Lens.lens (\CreateStage' {deploymentId} -> deploymentId) (\s@CreateStage' {} a -> s {deploymentId = a} :: CreateStage)

instance Core.AWSRequest CreateStage where
  type AWSResponse CreateStage = Stage
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateStage where
  hashWithSalt salt' CreateStage' {..} =
    salt' `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` cacheClusterEnabled
      `Prelude.hashWithSalt` canarySettings
      `Prelude.hashWithSalt` cacheClusterSize
      `Prelude.hashWithSalt` tracingEnabled
      `Prelude.hashWithSalt` documentationVersion
      `Prelude.hashWithSalt` variables

instance Prelude.NFData CreateStage where
  rnf CreateStage' {..} =
    Prelude.rnf variables
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf cacheClusterEnabled
      `Prelude.seq` Prelude.rnf canarySettings
      `Prelude.seq` Prelude.rnf cacheClusterSize
      `Prelude.seq` Prelude.rnf tracingEnabled
      `Prelude.seq` Prelude.rnf documentationVersion

instance Core.ToHeaders CreateStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON CreateStage where
  toJSON CreateStage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("variables" Core..=) Prelude.<$> variables,
            ("documentationVersion" Core..=)
              Prelude.<$> documentationVersion,
            ("tracingEnabled" Core..=)
              Prelude.<$> tracingEnabled,
            ("cacheClusterSize" Core..=)
              Prelude.<$> cacheClusterSize,
            ("canarySettings" Core..=)
              Prelude.<$> canarySettings,
            ("cacheClusterEnabled" Core..=)
              Prelude.<$> cacheClusterEnabled,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("stageName" Core..= stageName),
            Prelude.Just ("deploymentId" Core..= deploymentId)
          ]
      )

instance Core.ToPath CreateStage where
  toPath CreateStage' {..} =
    Prelude.mconcat
      ["/restapis/", Core.toBS restApiId, "/stages"]

instance Core.ToQuery CreateStage where
  toQuery = Prelude.const Prelude.mempty
