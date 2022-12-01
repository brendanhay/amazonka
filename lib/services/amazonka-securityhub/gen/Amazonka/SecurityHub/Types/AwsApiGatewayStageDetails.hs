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
-- Module      : Amazonka.SecurityHub.Types.AwsApiGatewayStageDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayStageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsApiGatewayAccessLogSettings
import Amazonka.SecurityHub.Types.AwsApiGatewayCanarySettings
import Amazonka.SecurityHub.Types.AwsApiGatewayMethodSettings

-- | Provides information about a version 1 Amazon API Gateway stage.
--
-- /See:/ 'newAwsApiGatewayStageDetails' smart constructor.
data AwsApiGatewayStageDetails = AwsApiGatewayStageDetails'
  { -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the web ACL associated with the stage.
    webAclArn :: Prelude.Maybe Prelude.Text,
    -- | Settings for logging access for the stage.
    accessLogSettings :: Prelude.Maybe AwsApiGatewayAccessLogSettings,
    -- | Indicates whether a cache cluster is enabled for the stage.
    cacheClusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If a cache cluster is enabled, the status of the cache cluster.
    cacheClusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the deployment that the stage points to.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the stage was most recently updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastUpdatedDate :: Prelude.Maybe Prelude.Text,
    -- | Defines the method settings for the stage.
    methodSettings :: Prelude.Maybe [AwsApiGatewayMethodSettings],
    -- | Indicates whether active tracing with X-Ray is enabled for the stage.
    tracingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the client certificate for the stage.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | If a cache cluster is enabled, the size of the cache cluster.
    cacheClusterSize :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the stage was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | Information about settings for canary deployment in the stage.
    canarySettings :: Prelude.Maybe AwsApiGatewayCanarySettings,
    -- | The version of the API documentation that is associated with the stage.
    documentationVersion :: Prelude.Maybe Prelude.Text,
    -- | A map that defines the stage variables for the stage.
    --
    -- Variable names can have alphanumeric and underscore characters.
    --
    -- Variable values can contain the following characters:
    --
    -- -   Uppercase and lowercase letters
    --
    -- -   Numbers
    --
    -- -   Special characters -._~:\/?#&=,
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiGatewayStageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'awsApiGatewayStageDetails_stageName' - The name of the stage.
--
-- 'webAclArn', 'awsApiGatewayStageDetails_webAclArn' - The ARN of the web ACL associated with the stage.
--
-- 'accessLogSettings', 'awsApiGatewayStageDetails_accessLogSettings' - Settings for logging access for the stage.
--
-- 'cacheClusterEnabled', 'awsApiGatewayStageDetails_cacheClusterEnabled' - Indicates whether a cache cluster is enabled for the stage.
--
-- 'cacheClusterStatus', 'awsApiGatewayStageDetails_cacheClusterStatus' - If a cache cluster is enabled, the status of the cache cluster.
--
-- 'deploymentId', 'awsApiGatewayStageDetails_deploymentId' - The identifier of the deployment that the stage points to.
--
-- 'lastUpdatedDate', 'awsApiGatewayStageDetails_lastUpdatedDate' - Indicates when the stage was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'methodSettings', 'awsApiGatewayStageDetails_methodSettings' - Defines the method settings for the stage.
--
-- 'tracingEnabled', 'awsApiGatewayStageDetails_tracingEnabled' - Indicates whether active tracing with X-Ray is enabled for the stage.
--
-- 'description', 'awsApiGatewayStageDetails_description' - A description of the stage.
--
-- 'clientCertificateId', 'awsApiGatewayStageDetails_clientCertificateId' - The identifier of the client certificate for the stage.
--
-- 'cacheClusterSize', 'awsApiGatewayStageDetails_cacheClusterSize' - If a cache cluster is enabled, the size of the cache cluster.
--
-- 'createdDate', 'awsApiGatewayStageDetails_createdDate' - Indicates when the stage was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'canarySettings', 'awsApiGatewayStageDetails_canarySettings' - Information about settings for canary deployment in the stage.
--
-- 'documentationVersion', 'awsApiGatewayStageDetails_documentationVersion' - The version of the API documentation that is associated with the stage.
--
-- 'variables', 'awsApiGatewayStageDetails_variables' - A map that defines the stage variables for the stage.
--
-- Variable names can have alphanumeric and underscore characters.
--
-- Variable values can contain the following characters:
--
-- -   Uppercase and lowercase letters
--
-- -   Numbers
--
-- -   Special characters -._~:\/?#&=,
newAwsApiGatewayStageDetails ::
  AwsApiGatewayStageDetails
newAwsApiGatewayStageDetails =
  AwsApiGatewayStageDetails'
    { stageName =
        Prelude.Nothing,
      webAclArn = Prelude.Nothing,
      accessLogSettings = Prelude.Nothing,
      cacheClusterEnabled = Prelude.Nothing,
      cacheClusterStatus = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      methodSettings = Prelude.Nothing,
      tracingEnabled = Prelude.Nothing,
      description = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      cacheClusterSize = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      canarySettings = Prelude.Nothing,
      documentationVersion = Prelude.Nothing,
      variables = Prelude.Nothing
    }

-- | The name of the stage.
awsApiGatewayStageDetails_stageName :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_stageName = Lens.lens (\AwsApiGatewayStageDetails' {stageName} -> stageName) (\s@AwsApiGatewayStageDetails' {} a -> s {stageName = a} :: AwsApiGatewayStageDetails)

-- | The ARN of the web ACL associated with the stage.
awsApiGatewayStageDetails_webAclArn :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_webAclArn = Lens.lens (\AwsApiGatewayStageDetails' {webAclArn} -> webAclArn) (\s@AwsApiGatewayStageDetails' {} a -> s {webAclArn = a} :: AwsApiGatewayStageDetails)

-- | Settings for logging access for the stage.
awsApiGatewayStageDetails_accessLogSettings :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe AwsApiGatewayAccessLogSettings)
awsApiGatewayStageDetails_accessLogSettings = Lens.lens (\AwsApiGatewayStageDetails' {accessLogSettings} -> accessLogSettings) (\s@AwsApiGatewayStageDetails' {} a -> s {accessLogSettings = a} :: AwsApiGatewayStageDetails)

-- | Indicates whether a cache cluster is enabled for the stage.
awsApiGatewayStageDetails_cacheClusterEnabled :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Bool)
awsApiGatewayStageDetails_cacheClusterEnabled = Lens.lens (\AwsApiGatewayStageDetails' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@AwsApiGatewayStageDetails' {} a -> s {cacheClusterEnabled = a} :: AwsApiGatewayStageDetails)

-- | If a cache cluster is enabled, the status of the cache cluster.
awsApiGatewayStageDetails_cacheClusterStatus :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_cacheClusterStatus = Lens.lens (\AwsApiGatewayStageDetails' {cacheClusterStatus} -> cacheClusterStatus) (\s@AwsApiGatewayStageDetails' {} a -> s {cacheClusterStatus = a} :: AwsApiGatewayStageDetails)

-- | The identifier of the deployment that the stage points to.
awsApiGatewayStageDetails_deploymentId :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_deploymentId = Lens.lens (\AwsApiGatewayStageDetails' {deploymentId} -> deploymentId) (\s@AwsApiGatewayStageDetails' {} a -> s {deploymentId = a} :: AwsApiGatewayStageDetails)

-- | Indicates when the stage was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayStageDetails_lastUpdatedDate :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_lastUpdatedDate = Lens.lens (\AwsApiGatewayStageDetails' {lastUpdatedDate} -> lastUpdatedDate) (\s@AwsApiGatewayStageDetails' {} a -> s {lastUpdatedDate = a} :: AwsApiGatewayStageDetails)

-- | Defines the method settings for the stage.
awsApiGatewayStageDetails_methodSettings :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe [AwsApiGatewayMethodSettings])
awsApiGatewayStageDetails_methodSettings = Lens.lens (\AwsApiGatewayStageDetails' {methodSettings} -> methodSettings) (\s@AwsApiGatewayStageDetails' {} a -> s {methodSettings = a} :: AwsApiGatewayStageDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether active tracing with X-Ray is enabled for the stage.
awsApiGatewayStageDetails_tracingEnabled :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Bool)
awsApiGatewayStageDetails_tracingEnabled = Lens.lens (\AwsApiGatewayStageDetails' {tracingEnabled} -> tracingEnabled) (\s@AwsApiGatewayStageDetails' {} a -> s {tracingEnabled = a} :: AwsApiGatewayStageDetails)

-- | A description of the stage.
awsApiGatewayStageDetails_description :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_description = Lens.lens (\AwsApiGatewayStageDetails' {description} -> description) (\s@AwsApiGatewayStageDetails' {} a -> s {description = a} :: AwsApiGatewayStageDetails)

-- | The identifier of the client certificate for the stage.
awsApiGatewayStageDetails_clientCertificateId :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_clientCertificateId = Lens.lens (\AwsApiGatewayStageDetails' {clientCertificateId} -> clientCertificateId) (\s@AwsApiGatewayStageDetails' {} a -> s {clientCertificateId = a} :: AwsApiGatewayStageDetails)

-- | If a cache cluster is enabled, the size of the cache cluster.
awsApiGatewayStageDetails_cacheClusterSize :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_cacheClusterSize = Lens.lens (\AwsApiGatewayStageDetails' {cacheClusterSize} -> cacheClusterSize) (\s@AwsApiGatewayStageDetails' {} a -> s {cacheClusterSize = a} :: AwsApiGatewayStageDetails)

-- | Indicates when the stage was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayStageDetails_createdDate :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_createdDate = Lens.lens (\AwsApiGatewayStageDetails' {createdDate} -> createdDate) (\s@AwsApiGatewayStageDetails' {} a -> s {createdDate = a} :: AwsApiGatewayStageDetails)

-- | Information about settings for canary deployment in the stage.
awsApiGatewayStageDetails_canarySettings :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe AwsApiGatewayCanarySettings)
awsApiGatewayStageDetails_canarySettings = Lens.lens (\AwsApiGatewayStageDetails' {canarySettings} -> canarySettings) (\s@AwsApiGatewayStageDetails' {} a -> s {canarySettings = a} :: AwsApiGatewayStageDetails)

-- | The version of the API documentation that is associated with the stage.
awsApiGatewayStageDetails_documentationVersion :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_documentationVersion = Lens.lens (\AwsApiGatewayStageDetails' {documentationVersion} -> documentationVersion) (\s@AwsApiGatewayStageDetails' {} a -> s {documentationVersion = a} :: AwsApiGatewayStageDetails)

-- | A map that defines the stage variables for the stage.
--
-- Variable names can have alphanumeric and underscore characters.
--
-- Variable values can contain the following characters:
--
-- -   Uppercase and lowercase letters
--
-- -   Numbers
--
-- -   Special characters -._~:\/?#&=,
awsApiGatewayStageDetails_variables :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsApiGatewayStageDetails_variables = Lens.lens (\AwsApiGatewayStageDetails' {variables} -> variables) (\s@AwsApiGatewayStageDetails' {} a -> s {variables = a} :: AwsApiGatewayStageDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsApiGatewayStageDetails where
  parseJSON =
    Core.withObject
      "AwsApiGatewayStageDetails"
      ( \x ->
          AwsApiGatewayStageDetails'
            Prelude.<$> (x Core..:? "StageName")
            Prelude.<*> (x Core..:? "WebAclArn")
            Prelude.<*> (x Core..:? "AccessLogSettings")
            Prelude.<*> (x Core..:? "CacheClusterEnabled")
            Prelude.<*> (x Core..:? "CacheClusterStatus")
            Prelude.<*> (x Core..:? "DeploymentId")
            Prelude.<*> (x Core..:? "LastUpdatedDate")
            Prelude.<*> (x Core..:? "MethodSettings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TracingEnabled")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ClientCertificateId")
            Prelude.<*> (x Core..:? "CacheClusterSize")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "CanarySettings")
            Prelude.<*> (x Core..:? "DocumentationVersion")
            Prelude.<*> (x Core..:? "Variables" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsApiGatewayStageDetails where
  hashWithSalt _salt AwsApiGatewayStageDetails' {..} =
    _salt `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` webAclArn
      `Prelude.hashWithSalt` accessLogSettings
      `Prelude.hashWithSalt` cacheClusterEnabled
      `Prelude.hashWithSalt` cacheClusterStatus
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` methodSettings
      `Prelude.hashWithSalt` tracingEnabled
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` clientCertificateId
      `Prelude.hashWithSalt` cacheClusterSize
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` canarySettings
      `Prelude.hashWithSalt` documentationVersion
      `Prelude.hashWithSalt` variables

instance Prelude.NFData AwsApiGatewayStageDetails where
  rnf AwsApiGatewayStageDetails' {..} =
    Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf webAclArn
      `Prelude.seq` Prelude.rnf accessLogSettings
      `Prelude.seq` Prelude.rnf cacheClusterEnabled
      `Prelude.seq` Prelude.rnf cacheClusterStatus
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf methodSettings
      `Prelude.seq` Prelude.rnf tracingEnabled
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf clientCertificateId
      `Prelude.seq` Prelude.rnf cacheClusterSize
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf canarySettings
      `Prelude.seq` Prelude.rnf documentationVersion
      `Prelude.seq` Prelude.rnf variables

instance Core.ToJSON AwsApiGatewayStageDetails where
  toJSON AwsApiGatewayStageDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StageName" Core..=) Prelude.<$> stageName,
            ("WebAclArn" Core..=) Prelude.<$> webAclArn,
            ("AccessLogSettings" Core..=)
              Prelude.<$> accessLogSettings,
            ("CacheClusterEnabled" Core..=)
              Prelude.<$> cacheClusterEnabled,
            ("CacheClusterStatus" Core..=)
              Prelude.<$> cacheClusterStatus,
            ("DeploymentId" Core..=) Prelude.<$> deploymentId,
            ("LastUpdatedDate" Core..=)
              Prelude.<$> lastUpdatedDate,
            ("MethodSettings" Core..=)
              Prelude.<$> methodSettings,
            ("TracingEnabled" Core..=)
              Prelude.<$> tracingEnabled,
            ("Description" Core..=) Prelude.<$> description,
            ("ClientCertificateId" Core..=)
              Prelude.<$> clientCertificateId,
            ("CacheClusterSize" Core..=)
              Prelude.<$> cacheClusterSize,
            ("CreatedDate" Core..=) Prelude.<$> createdDate,
            ("CanarySettings" Core..=)
              Prelude.<$> canarySettings,
            ("DocumentationVersion" Core..=)
              Prelude.<$> documentationVersion,
            ("Variables" Core..=) Prelude.<$> variables
          ]
      )
