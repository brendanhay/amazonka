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
-- Module      : Amazonka.APIGateway.Types.Stage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Stage where

import Amazonka.APIGateway.Types.AccessLogSettings
import Amazonka.APIGateway.Types.CacheClusterSize
import Amazonka.APIGateway.Types.CacheClusterStatus
import Amazonka.APIGateway.Types.CanarySettings
import Amazonka.APIGateway.Types.MethodSetting
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a unique identifier for a version of a deployed RestApi that
-- is callable by users.
--
-- /See:/ 'newStage' smart constructor.
data Stage = Stage'
  { -- | Settings for logging access in this stage.
    accessLogSettings :: Prelude.Maybe AccessLogSettings,
    -- | Specifies whether a cache cluster is enabled for the stage.
    cacheClusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The stage\'s cache capacity in GB. For more information about choosing a
    -- cache size, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-caching.html Enabling API caching to enhance responsiveness>.
    cacheClusterSize :: Prelude.Maybe CacheClusterSize,
    -- | The status of the cache cluster for the stage, if enabled.
    cacheClusterStatus :: Prelude.Maybe CacheClusterStatus,
    -- | Settings for the canary deployment in this stage.
    canarySettings :: Prelude.Maybe CanarySettings,
    -- | The identifier of a client certificate for an API stage.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the stage was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the Deployment that the stage points to.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The stage\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The version of the associated API documentation.
    documentationVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the stage last updated.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | A map that defines the method settings for a Stage resource. Keys
    -- (designated as @\/{method_setting_key@ below) are method paths defined
    -- as @{resource_path}\/{http_method}@ for an individual method override,
    -- or @\/\\*\/\\*@ for overriding all methods in the stage.
    methodSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text MethodSetting),
    -- | The name of the stage is the first path segment in the Uniform Resource
    -- Identifier (URI) of a call to API Gateway. Stage names can only contain
    -- alphanumeric characters, hyphens, and underscores. Maximum length is 128
    -- characters.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether active tracing with X-ray is enabled for the Stage.
    tracingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A map that defines the stage variables for a Stage resource. Variable
    -- names can have alphanumeric and underscore characters, and the values
    -- must match @[A-Za-z0-9-._~:\/?#&=,]+@.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the WebAcl associated with the Stage.
    webAclArn :: Prelude.Maybe Prelude.Text
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
-- 'accessLogSettings', 'stage_accessLogSettings' - Settings for logging access in this stage.
--
-- 'cacheClusterEnabled', 'stage_cacheClusterEnabled' - Specifies whether a cache cluster is enabled for the stage.
--
-- 'cacheClusterSize', 'stage_cacheClusterSize' - The stage\'s cache capacity in GB. For more information about choosing a
-- cache size, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-caching.html Enabling API caching to enhance responsiveness>.
--
-- 'cacheClusterStatus', 'stage_cacheClusterStatus' - The status of the cache cluster for the stage, if enabled.
--
-- 'canarySettings', 'stage_canarySettings' - Settings for the canary deployment in this stage.
--
-- 'clientCertificateId', 'stage_clientCertificateId' - The identifier of a client certificate for an API stage.
--
-- 'createdDate', 'stage_createdDate' - The timestamp when the stage was created.
--
-- 'deploymentId', 'stage_deploymentId' - The identifier of the Deployment that the stage points to.
--
-- 'description', 'stage_description' - The stage\'s description.
--
-- 'documentationVersion', 'stage_documentationVersion' - The version of the associated API documentation.
--
-- 'lastUpdatedDate', 'stage_lastUpdatedDate' - The timestamp when the stage last updated.
--
-- 'methodSettings', 'stage_methodSettings' - A map that defines the method settings for a Stage resource. Keys
-- (designated as @\/{method_setting_key@ below) are method paths defined
-- as @{resource_path}\/{http_method}@ for an individual method override,
-- or @\/\\*\/\\*@ for overriding all methods in the stage.
--
-- 'stageName', 'stage_stageName' - The name of the stage is the first path segment in the Uniform Resource
-- Identifier (URI) of a call to API Gateway. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
--
-- 'tags', 'stage_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'tracingEnabled', 'stage_tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the Stage.
--
-- 'variables', 'stage_variables' - A map that defines the stage variables for a Stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match @[A-Za-z0-9-._~:\/?#&=,]+@.
--
-- 'webAclArn', 'stage_webAclArn' - The ARN of the WebAcl associated with the Stage.
newStage ::
  Stage
newStage =
  Stage'
    { accessLogSettings = Prelude.Nothing,
      cacheClusterEnabled = Prelude.Nothing,
      cacheClusterSize = Prelude.Nothing,
      cacheClusterStatus = Prelude.Nothing,
      canarySettings = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      description = Prelude.Nothing,
      documentationVersion = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      methodSettings = Prelude.Nothing,
      stageName = Prelude.Nothing,
      tags = Prelude.Nothing,
      tracingEnabled = Prelude.Nothing,
      variables = Prelude.Nothing,
      webAclArn = Prelude.Nothing
    }

-- | Settings for logging access in this stage.
stage_accessLogSettings :: Lens.Lens' Stage (Prelude.Maybe AccessLogSettings)
stage_accessLogSettings = Lens.lens (\Stage' {accessLogSettings} -> accessLogSettings) (\s@Stage' {} a -> s {accessLogSettings = a} :: Stage)

-- | Specifies whether a cache cluster is enabled for the stage.
stage_cacheClusterEnabled :: Lens.Lens' Stage (Prelude.Maybe Prelude.Bool)
stage_cacheClusterEnabled = Lens.lens (\Stage' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@Stage' {} a -> s {cacheClusterEnabled = a} :: Stage)

-- | The stage\'s cache capacity in GB. For more information about choosing a
-- cache size, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-caching.html Enabling API caching to enhance responsiveness>.
stage_cacheClusterSize :: Lens.Lens' Stage (Prelude.Maybe CacheClusterSize)
stage_cacheClusterSize = Lens.lens (\Stage' {cacheClusterSize} -> cacheClusterSize) (\s@Stage' {} a -> s {cacheClusterSize = a} :: Stage)

-- | The status of the cache cluster for the stage, if enabled.
stage_cacheClusterStatus :: Lens.Lens' Stage (Prelude.Maybe CacheClusterStatus)
stage_cacheClusterStatus = Lens.lens (\Stage' {cacheClusterStatus} -> cacheClusterStatus) (\s@Stage' {} a -> s {cacheClusterStatus = a} :: Stage)

-- | Settings for the canary deployment in this stage.
stage_canarySettings :: Lens.Lens' Stage (Prelude.Maybe CanarySettings)
stage_canarySettings = Lens.lens (\Stage' {canarySettings} -> canarySettings) (\s@Stage' {} a -> s {canarySettings = a} :: Stage)

-- | The identifier of a client certificate for an API stage.
stage_clientCertificateId :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_clientCertificateId = Lens.lens (\Stage' {clientCertificateId} -> clientCertificateId) (\s@Stage' {} a -> s {clientCertificateId = a} :: Stage)

-- | The timestamp when the stage was created.
stage_createdDate :: Lens.Lens' Stage (Prelude.Maybe Prelude.UTCTime)
stage_createdDate = Lens.lens (\Stage' {createdDate} -> createdDate) (\s@Stage' {} a -> s {createdDate = a} :: Stage) Prelude.. Lens.mapping Data._Time

-- | The identifier of the Deployment that the stage points to.
stage_deploymentId :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_deploymentId = Lens.lens (\Stage' {deploymentId} -> deploymentId) (\s@Stage' {} a -> s {deploymentId = a} :: Stage)

-- | The stage\'s description.
stage_description :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_description = Lens.lens (\Stage' {description} -> description) (\s@Stage' {} a -> s {description = a} :: Stage)

-- | The version of the associated API documentation.
stage_documentationVersion :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_documentationVersion = Lens.lens (\Stage' {documentationVersion} -> documentationVersion) (\s@Stage' {} a -> s {documentationVersion = a} :: Stage)

-- | The timestamp when the stage last updated.
stage_lastUpdatedDate :: Lens.Lens' Stage (Prelude.Maybe Prelude.UTCTime)
stage_lastUpdatedDate = Lens.lens (\Stage' {lastUpdatedDate} -> lastUpdatedDate) (\s@Stage' {} a -> s {lastUpdatedDate = a} :: Stage) Prelude.. Lens.mapping Data._Time

-- | A map that defines the method settings for a Stage resource. Keys
-- (designated as @\/{method_setting_key@ below) are method paths defined
-- as @{resource_path}\/{http_method}@ for an individual method override,
-- or @\/\\*\/\\*@ for overriding all methods in the stage.
stage_methodSettings :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text MethodSetting))
stage_methodSettings = Lens.lens (\Stage' {methodSettings} -> methodSettings) (\s@Stage' {} a -> s {methodSettings = a} :: Stage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stage is the first path segment in the Uniform Resource
-- Identifier (URI) of a call to API Gateway. Stage names can only contain
-- alphanumeric characters, hyphens, and underscores. Maximum length is 128
-- characters.
stage_stageName :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_stageName = Lens.lens (\Stage' {stageName} -> stageName) (\s@Stage' {} a -> s {stageName = a} :: Stage)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
stage_tags :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stage_tags = Lens.lens (\Stage' {tags} -> tags) (\s@Stage' {} a -> s {tags = a} :: Stage) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether active tracing with X-ray is enabled for the Stage.
stage_tracingEnabled :: Lens.Lens' Stage (Prelude.Maybe Prelude.Bool)
stage_tracingEnabled = Lens.lens (\Stage' {tracingEnabled} -> tracingEnabled) (\s@Stage' {} a -> s {tracingEnabled = a} :: Stage)

-- | A map that defines the stage variables for a Stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match @[A-Za-z0-9-._~:\/?#&=,]+@.
stage_variables :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stage_variables = Lens.lens (\Stage' {variables} -> variables) (\s@Stage' {} a -> s {variables = a} :: Stage) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the WebAcl associated with the Stage.
stage_webAclArn :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_webAclArn = Lens.lens (\Stage' {webAclArn} -> webAclArn) (\s@Stage' {} a -> s {webAclArn = a} :: Stage)

instance Data.FromJSON Stage where
  parseJSON =
    Data.withObject
      "Stage"
      ( \x ->
          Stage'
            Prelude.<$> (x Data..:? "accessLogSettings")
            Prelude.<*> (x Data..:? "cacheClusterEnabled")
            Prelude.<*> (x Data..:? "cacheClusterSize")
            Prelude.<*> (x Data..:? "cacheClusterStatus")
            Prelude.<*> (x Data..:? "canarySettings")
            Prelude.<*> (x Data..:? "clientCertificateId")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "deploymentId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "documentationVersion")
            Prelude.<*> (x Data..:? "lastUpdatedDate")
            Prelude.<*> (x Data..:? "methodSettings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "stageName")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tracingEnabled")
            Prelude.<*> (x Data..:? "variables" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "webAclArn")
      )

instance Prelude.Hashable Stage where
  hashWithSalt _salt Stage' {..} =
    _salt
      `Prelude.hashWithSalt` accessLogSettings
      `Prelude.hashWithSalt` cacheClusterEnabled
      `Prelude.hashWithSalt` cacheClusterSize
      `Prelude.hashWithSalt` cacheClusterStatus
      `Prelude.hashWithSalt` canarySettings
      `Prelude.hashWithSalt` clientCertificateId
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` documentationVersion
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` methodSettings
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tracingEnabled
      `Prelude.hashWithSalt` variables
      `Prelude.hashWithSalt` webAclArn

instance Prelude.NFData Stage where
  rnf Stage' {..} =
    Prelude.rnf accessLogSettings
      `Prelude.seq` Prelude.rnf cacheClusterEnabled
      `Prelude.seq` Prelude.rnf cacheClusterSize
      `Prelude.seq` Prelude.rnf cacheClusterStatus
      `Prelude.seq` Prelude.rnf canarySettings
      `Prelude.seq` Prelude.rnf clientCertificateId
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf documentationVersion
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf methodSettings
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tracingEnabled
      `Prelude.seq` Prelude.rnf variables
      `Prelude.seq` Prelude.rnf webAclArn
