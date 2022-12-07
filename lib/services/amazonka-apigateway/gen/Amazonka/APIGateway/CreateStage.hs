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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createStage_tags,
    createStage_cacheClusterEnabled,
    createStage_description,
    createStage_tracingEnabled,
    createStage_cacheClusterSize,
    createStage_canarySettings,
    createStage_documentationVersion,
    createStage_variables,
    createStage_restApiId,
    createStage_stageName,
    createStage_deploymentId,

    -- * Destructuring the Response
    Stage (..),
    newStage,

    -- * Response Lenses
    stage_tags,
    stage_webAclArn,
    stage_stageName,
    stage_cacheClusterEnabled,
    stage_accessLogSettings,
    stage_cacheClusterStatus,
    stage_deploymentId,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_description,
    stage_tracingEnabled,
    stage_clientCertificateId,
    stage_cacheClusterSize,
    stage_canarySettings,
    stage_createdDate,
    stage_documentationVersion,
    stage_variables,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to create a Stage resource.
--
-- /See:/ 'newCreateStage' smart constructor.
data CreateStage = CreateStage'
  { -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Whether cache clustering is enabled for the stage.
    cacheClusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The description of the Stage resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether active tracing with X-ray is enabled for the Stage.
    tracingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The stage\'s cache cluster size.
    cacheClusterSize :: Prelude.Maybe CacheClusterSize,
    -- | The canary deployment settings of this stage.
    canarySettings :: Prelude.Maybe CanarySettings,
    -- | The version of the associated API documentation.
    documentationVersion :: Prelude.Maybe Prelude.Text,
    -- | A map that defines the stage variables for the new Stage resource.
    -- Variable names can have alphanumeric and underscore characters, and the
    -- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name for the Stage resource. Stage names can only contain
    -- alphanumeric characters, hyphens, and underscores. Maximum length is 128
    -- characters.
    stageName :: Prelude.Text,
    -- | The identifier of the Deployment resource for the Stage resource.
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
-- 'tags', 'createStage_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'cacheClusterEnabled', 'createStage_cacheClusterEnabled' - Whether cache clustering is enabled for the stage.
--
-- 'description', 'createStage_description' - The description of the Stage resource.
--
-- 'tracingEnabled', 'createStage_tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the Stage.
--
-- 'cacheClusterSize', 'createStage_cacheClusterSize' - The stage\'s cache cluster size.
--
-- 'canarySettings', 'createStage_canarySettings' - The canary deployment settings of this stage.
--
-- 'documentationVersion', 'createStage_documentationVersion' - The version of the associated API documentation.
--
-- 'variables', 'createStage_variables' - A map that defines the stage variables for the new Stage resource.
-- Variable names can have alphanumeric and underscore characters, and the
-- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
--
-- 'restApiId', 'createStage_restApiId' - The string identifier of the associated RestApi.
--
-- 'stageName', 'createStage_stageName' - The name for the Stage resource. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
--
-- 'deploymentId', 'createStage_deploymentId' - The identifier of the Deployment resource for the Stage resource.
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
    { tags = Prelude.Nothing,
      cacheClusterEnabled = Prelude.Nothing,
      description = Prelude.Nothing,
      tracingEnabled = Prelude.Nothing,
      cacheClusterSize = Prelude.Nothing,
      canarySettings = Prelude.Nothing,
      documentationVersion = Prelude.Nothing,
      variables = Prelude.Nothing,
      restApiId = pRestApiId_,
      stageName = pStageName_,
      deploymentId = pDeploymentId_
    }

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createStage_tags :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStage_tags = Lens.lens (\CreateStage' {tags} -> tags) (\s@CreateStage' {} a -> s {tags = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | Whether cache clustering is enabled for the stage.
createStage_cacheClusterEnabled :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Bool)
createStage_cacheClusterEnabled = Lens.lens (\CreateStage' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@CreateStage' {} a -> s {cacheClusterEnabled = a} :: CreateStage)

-- | The description of the Stage resource.
createStage_description :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_description = Lens.lens (\CreateStage' {description} -> description) (\s@CreateStage' {} a -> s {description = a} :: CreateStage)

-- | Specifies whether active tracing with X-ray is enabled for the Stage.
createStage_tracingEnabled :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Bool)
createStage_tracingEnabled = Lens.lens (\CreateStage' {tracingEnabled} -> tracingEnabled) (\s@CreateStage' {} a -> s {tracingEnabled = a} :: CreateStage)

-- | The stage\'s cache cluster size.
createStage_cacheClusterSize :: Lens.Lens' CreateStage (Prelude.Maybe CacheClusterSize)
createStage_cacheClusterSize = Lens.lens (\CreateStage' {cacheClusterSize} -> cacheClusterSize) (\s@CreateStage' {} a -> s {cacheClusterSize = a} :: CreateStage)

-- | The canary deployment settings of this stage.
createStage_canarySettings :: Lens.Lens' CreateStage (Prelude.Maybe CanarySettings)
createStage_canarySettings = Lens.lens (\CreateStage' {canarySettings} -> canarySettings) (\s@CreateStage' {} a -> s {canarySettings = a} :: CreateStage)

-- | The version of the associated API documentation.
createStage_documentationVersion :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_documentationVersion = Lens.lens (\CreateStage' {documentationVersion} -> documentationVersion) (\s@CreateStage' {} a -> s {documentationVersion = a} :: CreateStage)

-- | A map that defines the stage variables for the new Stage resource.
-- Variable names can have alphanumeric and underscore characters, and the
-- values must match @[A-Za-z0-9-._~:\/?#&=,]+@.
createStage_variables :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStage_variables = Lens.lens (\CreateStage' {variables} -> variables) (\s@CreateStage' {} a -> s {variables = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
createStage_restApiId :: Lens.Lens' CreateStage Prelude.Text
createStage_restApiId = Lens.lens (\CreateStage' {restApiId} -> restApiId) (\s@CreateStage' {} a -> s {restApiId = a} :: CreateStage)

-- | The name for the Stage resource. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
createStage_stageName :: Lens.Lens' CreateStage Prelude.Text
createStage_stageName = Lens.lens (\CreateStage' {stageName} -> stageName) (\s@CreateStage' {} a -> s {stageName = a} :: CreateStage)

-- | The identifier of the Deployment resource for the Stage resource.
createStage_deploymentId :: Lens.Lens' CreateStage Prelude.Text
createStage_deploymentId = Lens.lens (\CreateStage' {deploymentId} -> deploymentId) (\s@CreateStage' {} a -> s {deploymentId = a} :: CreateStage)

instance Core.AWSRequest CreateStage where
  type AWSResponse CreateStage = Stage
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateStage where
  hashWithSalt _salt CreateStage' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` cacheClusterEnabled
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tracingEnabled
      `Prelude.hashWithSalt` cacheClusterSize
      `Prelude.hashWithSalt` canarySettings
      `Prelude.hashWithSalt` documentationVersion
      `Prelude.hashWithSalt` variables
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData CreateStage where
  rnf CreateStage' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cacheClusterEnabled
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tracingEnabled
      `Prelude.seq` Prelude.rnf cacheClusterSize
      `Prelude.seq` Prelude.rnf canarySettings
      `Prelude.seq` Prelude.rnf documentationVersion
      `Prelude.seq` Prelude.rnf variables
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf deploymentId

instance Data.ToHeaders CreateStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateStage where
  toJSON CreateStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("cacheClusterEnabled" Data..=)
              Prelude.<$> cacheClusterEnabled,
            ("description" Data..=) Prelude.<$> description,
            ("tracingEnabled" Data..=)
              Prelude.<$> tracingEnabled,
            ("cacheClusterSize" Data..=)
              Prelude.<$> cacheClusterSize,
            ("canarySettings" Data..=)
              Prelude.<$> canarySettings,
            ("documentationVersion" Data..=)
              Prelude.<$> documentationVersion,
            ("variables" Data..=) Prelude.<$> variables,
            Prelude.Just ("stageName" Data..= stageName),
            Prelude.Just ("deploymentId" Data..= deploymentId)
          ]
      )

instance Data.ToPath CreateStage where
  toPath CreateStage' {..} =
    Prelude.mconcat
      ["/restapis/", Data.toBS restApiId, "/stages"]

instance Data.ToQuery CreateStage where
  toQuery = Prelude.const Prelude.mempty
