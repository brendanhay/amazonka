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
-- Module      : Network.AWS.SecurityHub.Types.AwsApiGatewayStageDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsApiGatewayStageDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsApiGatewayAccessLogSettings
import Network.AWS.SecurityHub.Types.AwsApiGatewayCanarySettings
import Network.AWS.SecurityHub.Types.AwsApiGatewayMethodSettings

-- | Provides information about a version 1 Amazon API Gateway stage.
--
-- /See:/ 'newAwsApiGatewayStageDetails' smart constructor.
data AwsApiGatewayStageDetails = AwsApiGatewayStageDetails'
  { -- | The identifier of the deployment that the stage points to.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | Settings for logging access for the stage.
    accessLogSettings :: Prelude.Maybe AwsApiGatewayAccessLogSettings,
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
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the API documentation that is associated with the stage.
    documentationVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the client certificate for the stage.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether active tracing with X-Ray is enabled for the stage.
    tracingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the stage was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | Defines the method settings for the stage.
    methodSettings :: Prelude.Maybe [AwsApiGatewayMethodSettings],
    -- | If a cache cluster is enabled, the status of the cache cluster.
    cacheClusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the web ACL associated with the stage.
    webAclArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a cache cluster is enabled for the stage.
    cacheClusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If a cache cluster is enabled, the size of the cache cluster.
    cacheClusterSize :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the stage was most recently updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastUpdatedDate :: Prelude.Maybe Prelude.Text,
    -- | Information about settings for canary deployment in the stage.
    canarySettings :: Prelude.Maybe AwsApiGatewayCanarySettings,
    -- | A description of the stage.
    description :: Prelude.Maybe Prelude.Text
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
-- 'deploymentId', 'awsApiGatewayStageDetails_deploymentId' - The identifier of the deployment that the stage points to.
--
-- 'accessLogSettings', 'awsApiGatewayStageDetails_accessLogSettings' - Settings for logging access for the stage.
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
--
-- 'documentationVersion', 'awsApiGatewayStageDetails_documentationVersion' - The version of the API documentation that is associated with the stage.
--
-- 'clientCertificateId', 'awsApiGatewayStageDetails_clientCertificateId' - The identifier of the client certificate for the stage.
--
-- 'tracingEnabled', 'awsApiGatewayStageDetails_tracingEnabled' - Indicates whether active tracing with X-Ray is enabled for the stage.
--
-- 'createdDate', 'awsApiGatewayStageDetails_createdDate' - Indicates when the stage was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'methodSettings', 'awsApiGatewayStageDetails_methodSettings' - Defines the method settings for the stage.
--
-- 'cacheClusterStatus', 'awsApiGatewayStageDetails_cacheClusterStatus' - If a cache cluster is enabled, the status of the cache cluster.
--
-- 'webAclArn', 'awsApiGatewayStageDetails_webAclArn' - The ARN of the web ACL associated with the stage.
--
-- 'stageName', 'awsApiGatewayStageDetails_stageName' - The name of the stage.
--
-- 'cacheClusterEnabled', 'awsApiGatewayStageDetails_cacheClusterEnabled' - Indicates whether a cache cluster is enabled for the stage.
--
-- 'cacheClusterSize', 'awsApiGatewayStageDetails_cacheClusterSize' - If a cache cluster is enabled, the size of the cache cluster.
--
-- 'lastUpdatedDate', 'awsApiGatewayStageDetails_lastUpdatedDate' - Indicates when the stage was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'canarySettings', 'awsApiGatewayStageDetails_canarySettings' - Information about settings for canary deployment in the stage.
--
-- 'description', 'awsApiGatewayStageDetails_description' - A description of the stage.
newAwsApiGatewayStageDetails ::
  AwsApiGatewayStageDetails
newAwsApiGatewayStageDetails =
  AwsApiGatewayStageDetails'
    { deploymentId =
        Prelude.Nothing,
      accessLogSettings = Prelude.Nothing,
      variables = Prelude.Nothing,
      documentationVersion = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      tracingEnabled = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      methodSettings = Prelude.Nothing,
      cacheClusterStatus = Prelude.Nothing,
      webAclArn = Prelude.Nothing,
      stageName = Prelude.Nothing,
      cacheClusterEnabled = Prelude.Nothing,
      cacheClusterSize = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      canarySettings = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The identifier of the deployment that the stage points to.
awsApiGatewayStageDetails_deploymentId :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_deploymentId = Lens.lens (\AwsApiGatewayStageDetails' {deploymentId} -> deploymentId) (\s@AwsApiGatewayStageDetails' {} a -> s {deploymentId = a} :: AwsApiGatewayStageDetails)

-- | Settings for logging access for the stage.
awsApiGatewayStageDetails_accessLogSettings :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe AwsApiGatewayAccessLogSettings)
awsApiGatewayStageDetails_accessLogSettings = Lens.lens (\AwsApiGatewayStageDetails' {accessLogSettings} -> accessLogSettings) (\s@AwsApiGatewayStageDetails' {} a -> s {accessLogSettings = a} :: AwsApiGatewayStageDetails)

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

-- | The version of the API documentation that is associated with the stage.
awsApiGatewayStageDetails_documentationVersion :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_documentationVersion = Lens.lens (\AwsApiGatewayStageDetails' {documentationVersion} -> documentationVersion) (\s@AwsApiGatewayStageDetails' {} a -> s {documentationVersion = a} :: AwsApiGatewayStageDetails)

-- | The identifier of the client certificate for the stage.
awsApiGatewayStageDetails_clientCertificateId :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_clientCertificateId = Lens.lens (\AwsApiGatewayStageDetails' {clientCertificateId} -> clientCertificateId) (\s@AwsApiGatewayStageDetails' {} a -> s {clientCertificateId = a} :: AwsApiGatewayStageDetails)

-- | Indicates whether active tracing with X-Ray is enabled for the stage.
awsApiGatewayStageDetails_tracingEnabled :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Bool)
awsApiGatewayStageDetails_tracingEnabled = Lens.lens (\AwsApiGatewayStageDetails' {tracingEnabled} -> tracingEnabled) (\s@AwsApiGatewayStageDetails' {} a -> s {tracingEnabled = a} :: AwsApiGatewayStageDetails)

-- | Indicates when the stage was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayStageDetails_createdDate :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_createdDate = Lens.lens (\AwsApiGatewayStageDetails' {createdDate} -> createdDate) (\s@AwsApiGatewayStageDetails' {} a -> s {createdDate = a} :: AwsApiGatewayStageDetails)

-- | Defines the method settings for the stage.
awsApiGatewayStageDetails_methodSettings :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe [AwsApiGatewayMethodSettings])
awsApiGatewayStageDetails_methodSettings = Lens.lens (\AwsApiGatewayStageDetails' {methodSettings} -> methodSettings) (\s@AwsApiGatewayStageDetails' {} a -> s {methodSettings = a} :: AwsApiGatewayStageDetails) Prelude.. Lens.mapping Lens.coerced

-- | If a cache cluster is enabled, the status of the cache cluster.
awsApiGatewayStageDetails_cacheClusterStatus :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_cacheClusterStatus = Lens.lens (\AwsApiGatewayStageDetails' {cacheClusterStatus} -> cacheClusterStatus) (\s@AwsApiGatewayStageDetails' {} a -> s {cacheClusterStatus = a} :: AwsApiGatewayStageDetails)

-- | The ARN of the web ACL associated with the stage.
awsApiGatewayStageDetails_webAclArn :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_webAclArn = Lens.lens (\AwsApiGatewayStageDetails' {webAclArn} -> webAclArn) (\s@AwsApiGatewayStageDetails' {} a -> s {webAclArn = a} :: AwsApiGatewayStageDetails)

-- | The name of the stage.
awsApiGatewayStageDetails_stageName :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_stageName = Lens.lens (\AwsApiGatewayStageDetails' {stageName} -> stageName) (\s@AwsApiGatewayStageDetails' {} a -> s {stageName = a} :: AwsApiGatewayStageDetails)

-- | Indicates whether a cache cluster is enabled for the stage.
awsApiGatewayStageDetails_cacheClusterEnabled :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Bool)
awsApiGatewayStageDetails_cacheClusterEnabled = Lens.lens (\AwsApiGatewayStageDetails' {cacheClusterEnabled} -> cacheClusterEnabled) (\s@AwsApiGatewayStageDetails' {} a -> s {cacheClusterEnabled = a} :: AwsApiGatewayStageDetails)

-- | If a cache cluster is enabled, the size of the cache cluster.
awsApiGatewayStageDetails_cacheClusterSize :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_cacheClusterSize = Lens.lens (\AwsApiGatewayStageDetails' {cacheClusterSize} -> cacheClusterSize) (\s@AwsApiGatewayStageDetails' {} a -> s {cacheClusterSize = a} :: AwsApiGatewayStageDetails)

-- | Indicates when the stage was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayStageDetails_lastUpdatedDate :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_lastUpdatedDate = Lens.lens (\AwsApiGatewayStageDetails' {lastUpdatedDate} -> lastUpdatedDate) (\s@AwsApiGatewayStageDetails' {} a -> s {lastUpdatedDate = a} :: AwsApiGatewayStageDetails)

-- | Information about settings for canary deployment in the stage.
awsApiGatewayStageDetails_canarySettings :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe AwsApiGatewayCanarySettings)
awsApiGatewayStageDetails_canarySettings = Lens.lens (\AwsApiGatewayStageDetails' {canarySettings} -> canarySettings) (\s@AwsApiGatewayStageDetails' {} a -> s {canarySettings = a} :: AwsApiGatewayStageDetails)

-- | A description of the stage.
awsApiGatewayStageDetails_description :: Lens.Lens' AwsApiGatewayStageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayStageDetails_description = Lens.lens (\AwsApiGatewayStageDetails' {description} -> description) (\s@AwsApiGatewayStageDetails' {} a -> s {description = a} :: AwsApiGatewayStageDetails)

instance Core.FromJSON AwsApiGatewayStageDetails where
  parseJSON =
    Core.withObject
      "AwsApiGatewayStageDetails"
      ( \x ->
          AwsApiGatewayStageDetails'
            Prelude.<$> (x Core..:? "DeploymentId")
            Prelude.<*> (x Core..:? "AccessLogSettings")
            Prelude.<*> (x Core..:? "Variables" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DocumentationVersion")
            Prelude.<*> (x Core..:? "ClientCertificateId")
            Prelude.<*> (x Core..:? "TracingEnabled")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "MethodSettings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CacheClusterStatus")
            Prelude.<*> (x Core..:? "WebAclArn")
            Prelude.<*> (x Core..:? "StageName")
            Prelude.<*> (x Core..:? "CacheClusterEnabled")
            Prelude.<*> (x Core..:? "CacheClusterSize")
            Prelude.<*> (x Core..:? "LastUpdatedDate")
            Prelude.<*> (x Core..:? "CanarySettings")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable AwsApiGatewayStageDetails

instance Prelude.NFData AwsApiGatewayStageDetails

instance Core.ToJSON AwsApiGatewayStageDetails where
  toJSON AwsApiGatewayStageDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeploymentId" Core..=) Prelude.<$> deploymentId,
            ("AccessLogSettings" Core..=)
              Prelude.<$> accessLogSettings,
            ("Variables" Core..=) Prelude.<$> variables,
            ("DocumentationVersion" Core..=)
              Prelude.<$> documentationVersion,
            ("ClientCertificateId" Core..=)
              Prelude.<$> clientCertificateId,
            ("TracingEnabled" Core..=)
              Prelude.<$> tracingEnabled,
            ("CreatedDate" Core..=) Prelude.<$> createdDate,
            ("MethodSettings" Core..=)
              Prelude.<$> methodSettings,
            ("CacheClusterStatus" Core..=)
              Prelude.<$> cacheClusterStatus,
            ("WebAclArn" Core..=) Prelude.<$> webAclArn,
            ("StageName" Core..=) Prelude.<$> stageName,
            ("CacheClusterEnabled" Core..=)
              Prelude.<$> cacheClusterEnabled,
            ("CacheClusterSize" Core..=)
              Prelude.<$> cacheClusterSize,
            ("LastUpdatedDate" Core..=)
              Prelude.<$> lastUpdatedDate,
            ("CanarySettings" Core..=)
              Prelude.<$> canarySettings,
            ("Description" Core..=) Prelude.<$> description
          ]
      )
