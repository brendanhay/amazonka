{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Stage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Stage where

import Network.AWS.APIGateway.Types.AccessLogSettings
import Network.AWS.APIGateway.Types.CacheClusterSize
import Network.AWS.APIGateway.Types.CacheClusterStatus
import Network.AWS.APIGateway.Types.CanarySettings
import Network.AWS.APIGateway.Types.MethodSetting
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a unique identifier for a version of a deployed RestApi that
-- is callable by users.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploy an API>
--
-- /See:/ 'newStage' smart constructor.
data Stage = Stage'
  { -- | The identifier of the Deployment that the stage points to.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the stage was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether active tracing with X-ray is enabled for the Stage.
    tracingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the WebAcl associated with the Stage.
    webAclArn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the stage last updated.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether a cache cluster is enabled for the stage.
    cacheClusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the stage is the first path segment in the Uniform Resource
    -- Identifier (URI) of a call to API Gateway. Stage names can only contain
    -- alphanumeric characters, hyphens, and underscores. Maximum length is 128
    -- characters.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The version of the associated API documentation.
    documentationVersion :: Prelude.Maybe Prelude.Text,
    -- | A map that defines the stage variables for a Stage resource. Variable
    -- names can have alphanumeric and underscore characters, and the values
    -- must match @[A-Za-z0-9-._~:\/?#&=,]+@.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Settings for logging access in this stage.
    accessLogSettings :: Prelude.Maybe AccessLogSettings,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of a client certificate for an API stage.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The stage\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Settings for the canary deployment in this stage.
    canarySettings :: Prelude.Maybe CanarySettings,
    -- | The size of the cache cluster for the stage, if enabled.
    cacheClusterSize :: Prelude.Maybe CacheClusterSize,
    -- | A map that defines the method settings for a Stage resource. Keys
    -- (designated as @\/{method_setting_key@ below) are method paths defined
    -- as @{resource_path}\/{http_method}@ for an individual method override,
    -- or @\/\\*\/\\*@ for overriding all methods in the stage.
    methodSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text MethodSetting),
    -- | The status of the cache cluster for the stage, if enabled.
    cacheClusterStatus :: Prelude.Maybe CacheClusterStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Stage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'stage_deploymentId' - The identifier of the Deployment that the stage points to.
--
-- 'createdDate', 'stage_createdDate' - The timestamp when the stage was created.
--
-- 'tracingEnabled', 'stage_tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the Stage.
--
-- 'webAclArn', 'stage_webAclArn' - The ARN of the WebAcl associated with the Stage.
--
-- 'lastUpdatedDate', 'stage_lastUpdatedDate' - The timestamp when the stage last updated.
--
-- 'cacheClusterEnabled', 'stage_cacheClusterEnabled' - Specifies whether a cache cluster is enabled for the stage.
--
-- 'stageName', 'stage_stageName' - The name of the stage is the first path segment in the Uniform Resource
-- Identifier (URI) of a call to API Gateway. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
--
-- 'documentationVersion', 'stage_documentationVersion' - The version of the associated API documentation.
--
-- 'variables', 'stage_variables' - A map that defines the stage variables for a Stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match @[A-Za-z0-9-._~:\/?#&=,]+@.
--
-- 'accessLogSettings', 'stage_accessLogSettings' - Settings for logging access in this stage.
--
-- 'tags', 'stage_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'clientCertificateId', 'stage_clientCertificateId' - The identifier of a client certificate for an API stage.
--
-- 'description', 'stage_description' - The stage\'s description.
--
-- 'canarySettings', 'stage_canarySettings' - Settings for the canary deployment in this stage.
--
-- 'cacheClusterSize', 'stage_cacheClusterSize' - The size of the cache cluster for the stage, if enabled.
--
-- 'methodSettings', 'stage_methodSettings' - A map that defines the method settings for a Stage resource. Keys
-- (designated as @\/{method_setting_key@ below) are method paths defined
-- as @{resource_path}\/{http_method}@ for an individual method override,
-- or @\/\\*\/\\*@ for overriding all methods in the stage.
--
-- 'cacheClusterStatus', 'stage_cacheClusterStatus' - The status of the cache cluster for the stage, if enabled.
newStage ::
  Stage
newStage =
  Stage'
    { deploymentId = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      tracingEnabled = Prelude.Nothing,
      webAclArn = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      cacheClusterEnabled = Prelude.Nothing,
      stageName = Prelude.Nothing,
      documentationVersion = Prelude.Nothing,
      variables = Prelude.Nothing,
      accessLogSettings = Prelude.Nothing,
      tags = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      description = Prelude.Nothing,
      canarySettings = Prelude.Nothing,
      cacheClusterSize = Prelude.Nothing,
      methodSettings = Prelude.Nothing,
      cacheClusterStatus = Prelude.Nothing
    }

-- | The identifier of the Deployment that the stage points to.
stage_deploymentId :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_deploymentId = Lens.lens (\Stage' {deploymentId} -> deploymentId) (\s@Stage' {} a -> s {deploymentId = a} :: Stage)

-- | The timestamp when the stage was created.
stage_createdDate :: Lens.Lens' Stage (Prelude.Maybe Prelude.UTCTime)
stage_createdDate = Lens.lens (\Stage' {createdDate} -> createdDate) (\s@Stage' {} a -> s {createdDate = a} :: Stage) Prelude.. Lens.mapping Core._Time

-- | Specifies whether active tracing with X-ray is enabled for the Stage.
stage_tracingEnabled :: Lens.Lens' Stage (Prelude.Maybe Prelude.Bool)
stage_tracingEnabled = Lens.lens (\Stage' {tracingEnabled} -> tracingEnabled) (\s@Stage' {} a -> s {tracingEnabled = a} :: Stage)

-- | The ARN of the WebAcl associated with the Stage.
stage_webAclArn :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_webAclArn = Lens.lens (\Stage' {webAclArn} -> webAclArn) (\s@Stage' {} a -> s {webAclArn = a} :: Stage)

-- | The timestamp when the stage last updated.
stage_lastUpdatedDate :: Lens.Lens' Stage (Prelude.Maybe Prelude.UTCTime)
stage_lastUpdatedDate = Lens.lens (\Stage' {lastUpdatedDate} -> lastUpdatedDate) (\s@Stage' {} a -> s {lastUpdatedDate = a} :: Stage) Prelude.. Lens.mapping Core._Time

-- | Specifies whether a cache cluster is enabled for the stage.
stage_cacheClusterEnabled :: Lens.Lens' Stage (Prelude.Maybe Prelude.Bool)
stage_cacheClusterEnabled = Lens.lens (\Stage' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@Stage' {} a -> s {cacheClusterEnabled = a} :: Stage)

-- | The name of the stage is the first path segment in the Uniform Resource
-- Identifier (URI) of a call to API Gateway. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
stage_stageName :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_stageName = Lens.lens (\Stage' {stageName} -> stageName) (\s@Stage' {} a -> s {stageName = a} :: Stage)

-- | The version of the associated API documentation.
stage_documentationVersion :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_documentationVersion = Lens.lens (\Stage' {documentationVersion} -> documentationVersion) (\s@Stage' {} a -> s {documentationVersion = a} :: Stage)

-- | A map that defines the stage variables for a Stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match @[A-Za-z0-9-._~:\/?#&=,]+@.
stage_variables :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stage_variables = Lens.lens (\Stage' {variables} -> variables) (\s@Stage' {} a -> s {variables = a} :: Stage) Prelude.. Lens.mapping Lens._Coerce

-- | Settings for logging access in this stage.
stage_accessLogSettings :: Lens.Lens' Stage (Prelude.Maybe AccessLogSettings)
stage_accessLogSettings = Lens.lens (\Stage' {accessLogSettings} -> accessLogSettings) (\s@Stage' {} a -> s {accessLogSettings = a} :: Stage)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
stage_tags :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stage_tags = Lens.lens (\Stage' {tags} -> tags) (\s@Stage' {} a -> s {tags = a} :: Stage) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier of a client certificate for an API stage.
stage_clientCertificateId :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_clientCertificateId = Lens.lens (\Stage' {clientCertificateId} -> clientCertificateId) (\s@Stage' {} a -> s {clientCertificateId = a} :: Stage)

-- | The stage\'s description.
stage_description :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_description = Lens.lens (\Stage' {description} -> description) (\s@Stage' {} a -> s {description = a} :: Stage)

-- | Settings for the canary deployment in this stage.
stage_canarySettings :: Lens.Lens' Stage (Prelude.Maybe CanarySettings)
stage_canarySettings = Lens.lens (\Stage' {canarySettings} -> canarySettings) (\s@Stage' {} a -> s {canarySettings = a} :: Stage)

-- | The size of the cache cluster for the stage, if enabled.
stage_cacheClusterSize :: Lens.Lens' Stage (Prelude.Maybe CacheClusterSize)
stage_cacheClusterSize = Lens.lens (\Stage' {cacheClusterSize} -> cacheClusterSize) (\s@Stage' {} a -> s {cacheClusterSize = a} :: Stage)

-- | A map that defines the method settings for a Stage resource. Keys
-- (designated as @\/{method_setting_key@ below) are method paths defined
-- as @{resource_path}\/{http_method}@ for an individual method override,
-- or @\/\\*\/\\*@ for overriding all methods in the stage.
stage_methodSettings :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text MethodSetting))
stage_methodSettings = Lens.lens (\Stage' {methodSettings} -> methodSettings) (\s@Stage' {} a -> s {methodSettings = a} :: Stage) Prelude.. Lens.mapping Lens._Coerce

-- | The status of the cache cluster for the stage, if enabled.
stage_cacheClusterStatus :: Lens.Lens' Stage (Prelude.Maybe CacheClusterStatus)
stage_cacheClusterStatus = Lens.lens (\Stage' {cacheClusterStatus} -> cacheClusterStatus) (\s@Stage' {} a -> s {cacheClusterStatus = a} :: Stage)

instance Core.FromJSON Stage where
  parseJSON =
    Core.withObject
      "Stage"
      ( \x ->
          Stage'
            Prelude.<$> (x Core..:? "deploymentId")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "tracingEnabled")
            Prelude.<*> (x Core..:? "webAclArn")
            Prelude.<*> (x Core..:? "lastUpdatedDate")
            Prelude.<*> (x Core..:? "cacheClusterEnabled")
            Prelude.<*> (x Core..:? "stageName")
            Prelude.<*> (x Core..:? "documentationVersion")
            Prelude.<*> (x Core..:? "variables" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "accessLogSettings")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "clientCertificateId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "canarySettings")
            Prelude.<*> (x Core..:? "cacheClusterSize")
            Prelude.<*> (x Core..:? "methodSettings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "cacheClusterStatus")
      )

instance Prelude.Hashable Stage

instance Prelude.NFData Stage
