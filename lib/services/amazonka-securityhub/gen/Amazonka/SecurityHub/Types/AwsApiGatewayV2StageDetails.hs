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
-- Module      : Amazonka.SecurityHub.Types.AwsApiGatewayV2StageDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayV2StageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsApiGatewayAccessLogSettings
import Amazonka.SecurityHub.Types.AwsApiGatewayV2RouteSettings

-- | Contains information about a version 2 stage for Amazon API Gateway.
--
-- /See:/ 'newAwsApiGatewayV2StageDetails' smart constructor.
data AwsApiGatewayV2StageDetails = AwsApiGatewayV2StageDetails'
  { -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | Information about settings for logging access for the stage.
    accessLogSettings :: Prelude.Maybe AwsApiGatewayAccessLogSettings,
    -- | The identifier of the deployment that the stage is associated with.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether updates to an API automatically trigger a new
    -- deployment.
    autoDeploy :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the stage was most recently updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastUpdatedDate :: Prelude.Maybe Prelude.Text,
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
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a client certificate for a stage. Supported only for
    -- WebSocket API calls.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The status of the last deployment of a stage. Supported only if the
    -- stage has automatic deployment enabled.
    lastDeploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | Default route settings for the stage.
    defaultRouteSettings :: Prelude.Maybe AwsApiGatewayV2RouteSettings,
    -- | Indicates when the stage was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the stage is managed by API Gateway.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | The route settings for the stage.
    routeSettings :: Prelude.Maybe AwsApiGatewayV2RouteSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiGatewayV2StageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'awsApiGatewayV2StageDetails_stageName' - The name of the stage.
--
-- 'accessLogSettings', 'awsApiGatewayV2StageDetails_accessLogSettings' - Information about settings for logging access for the stage.
--
-- 'deploymentId', 'awsApiGatewayV2StageDetails_deploymentId' - The identifier of the deployment that the stage is associated with.
--
-- 'autoDeploy', 'awsApiGatewayV2StageDetails_autoDeploy' - Indicates whether updates to an API automatically trigger a new
-- deployment.
--
-- 'lastUpdatedDate', 'awsApiGatewayV2StageDetails_lastUpdatedDate' - Indicates when the stage was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'stageVariables', 'awsApiGatewayV2StageDetails_stageVariables' - A map that defines the stage variables for the stage.
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
-- 'description', 'awsApiGatewayV2StageDetails_description' - The description of the stage.
--
-- 'clientCertificateId', 'awsApiGatewayV2StageDetails_clientCertificateId' - The identifier of a client certificate for a stage. Supported only for
-- WebSocket API calls.
--
-- 'lastDeploymentStatusMessage', 'awsApiGatewayV2StageDetails_lastDeploymentStatusMessage' - The status of the last deployment of a stage. Supported only if the
-- stage has automatic deployment enabled.
--
-- 'defaultRouteSettings', 'awsApiGatewayV2StageDetails_defaultRouteSettings' - Default route settings for the stage.
--
-- 'createdDate', 'awsApiGatewayV2StageDetails_createdDate' - Indicates when the stage was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'apiGatewayManaged', 'awsApiGatewayV2StageDetails_apiGatewayManaged' - Indicates whether the stage is managed by API Gateway.
--
-- 'routeSettings', 'awsApiGatewayV2StageDetails_routeSettings' - The route settings for the stage.
newAwsApiGatewayV2StageDetails ::
  AwsApiGatewayV2StageDetails
newAwsApiGatewayV2StageDetails =
  AwsApiGatewayV2StageDetails'
    { stageName =
        Prelude.Nothing,
      accessLogSettings = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      autoDeploy = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      stageVariables = Prelude.Nothing,
      description = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      lastDeploymentStatusMessage = Prelude.Nothing,
      defaultRouteSettings = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      apiGatewayManaged = Prelude.Nothing,
      routeSettings = Prelude.Nothing
    }

-- | The name of the stage.
awsApiGatewayV2StageDetails_stageName :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_stageName = Lens.lens (\AwsApiGatewayV2StageDetails' {stageName} -> stageName) (\s@AwsApiGatewayV2StageDetails' {} a -> s {stageName = a} :: AwsApiGatewayV2StageDetails)

-- | Information about settings for logging access for the stage.
awsApiGatewayV2StageDetails_accessLogSettings :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe AwsApiGatewayAccessLogSettings)
awsApiGatewayV2StageDetails_accessLogSettings = Lens.lens (\AwsApiGatewayV2StageDetails' {accessLogSettings} -> accessLogSettings) (\s@AwsApiGatewayV2StageDetails' {} a -> s {accessLogSettings = a} :: AwsApiGatewayV2StageDetails)

-- | The identifier of the deployment that the stage is associated with.
awsApiGatewayV2StageDetails_deploymentId :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_deploymentId = Lens.lens (\AwsApiGatewayV2StageDetails' {deploymentId} -> deploymentId) (\s@AwsApiGatewayV2StageDetails' {} a -> s {deploymentId = a} :: AwsApiGatewayV2StageDetails)

-- | Indicates whether updates to an API automatically trigger a new
-- deployment.
awsApiGatewayV2StageDetails_autoDeploy :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Bool)
awsApiGatewayV2StageDetails_autoDeploy = Lens.lens (\AwsApiGatewayV2StageDetails' {autoDeploy} -> autoDeploy) (\s@AwsApiGatewayV2StageDetails' {} a -> s {autoDeploy = a} :: AwsApiGatewayV2StageDetails)

-- | Indicates when the stage was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayV2StageDetails_lastUpdatedDate :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_lastUpdatedDate = Lens.lens (\AwsApiGatewayV2StageDetails' {lastUpdatedDate} -> lastUpdatedDate) (\s@AwsApiGatewayV2StageDetails' {} a -> s {lastUpdatedDate = a} :: AwsApiGatewayV2StageDetails)

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
awsApiGatewayV2StageDetails_stageVariables :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsApiGatewayV2StageDetails_stageVariables = Lens.lens (\AwsApiGatewayV2StageDetails' {stageVariables} -> stageVariables) (\s@AwsApiGatewayV2StageDetails' {} a -> s {stageVariables = a} :: AwsApiGatewayV2StageDetails) Prelude.. Lens.mapping Lens.coerced

-- | The description of the stage.
awsApiGatewayV2StageDetails_description :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_description = Lens.lens (\AwsApiGatewayV2StageDetails' {description} -> description) (\s@AwsApiGatewayV2StageDetails' {} a -> s {description = a} :: AwsApiGatewayV2StageDetails)

-- | The identifier of a client certificate for a stage. Supported only for
-- WebSocket API calls.
awsApiGatewayV2StageDetails_clientCertificateId :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_clientCertificateId = Lens.lens (\AwsApiGatewayV2StageDetails' {clientCertificateId} -> clientCertificateId) (\s@AwsApiGatewayV2StageDetails' {} a -> s {clientCertificateId = a} :: AwsApiGatewayV2StageDetails)

-- | The status of the last deployment of a stage. Supported only if the
-- stage has automatic deployment enabled.
awsApiGatewayV2StageDetails_lastDeploymentStatusMessage :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_lastDeploymentStatusMessage = Lens.lens (\AwsApiGatewayV2StageDetails' {lastDeploymentStatusMessage} -> lastDeploymentStatusMessage) (\s@AwsApiGatewayV2StageDetails' {} a -> s {lastDeploymentStatusMessage = a} :: AwsApiGatewayV2StageDetails)

-- | Default route settings for the stage.
awsApiGatewayV2StageDetails_defaultRouteSettings :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe AwsApiGatewayV2RouteSettings)
awsApiGatewayV2StageDetails_defaultRouteSettings = Lens.lens (\AwsApiGatewayV2StageDetails' {defaultRouteSettings} -> defaultRouteSettings) (\s@AwsApiGatewayV2StageDetails' {} a -> s {defaultRouteSettings = a} :: AwsApiGatewayV2StageDetails)

-- | Indicates when the stage was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayV2StageDetails_createdDate :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_createdDate = Lens.lens (\AwsApiGatewayV2StageDetails' {createdDate} -> createdDate) (\s@AwsApiGatewayV2StageDetails' {} a -> s {createdDate = a} :: AwsApiGatewayV2StageDetails)

-- | Indicates whether the stage is managed by API Gateway.
awsApiGatewayV2StageDetails_apiGatewayManaged :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Bool)
awsApiGatewayV2StageDetails_apiGatewayManaged = Lens.lens (\AwsApiGatewayV2StageDetails' {apiGatewayManaged} -> apiGatewayManaged) (\s@AwsApiGatewayV2StageDetails' {} a -> s {apiGatewayManaged = a} :: AwsApiGatewayV2StageDetails)

-- | The route settings for the stage.
awsApiGatewayV2StageDetails_routeSettings :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe AwsApiGatewayV2RouteSettings)
awsApiGatewayV2StageDetails_routeSettings = Lens.lens (\AwsApiGatewayV2StageDetails' {routeSettings} -> routeSettings) (\s@AwsApiGatewayV2StageDetails' {} a -> s {routeSettings = a} :: AwsApiGatewayV2StageDetails)

instance Data.FromJSON AwsApiGatewayV2StageDetails where
  parseJSON =
    Data.withObject
      "AwsApiGatewayV2StageDetails"
      ( \x ->
          AwsApiGatewayV2StageDetails'
            Prelude.<$> (x Data..:? "StageName")
            Prelude.<*> (x Data..:? "AccessLogSettings")
            Prelude.<*> (x Data..:? "DeploymentId")
            Prelude.<*> (x Data..:? "AutoDeploy")
            Prelude.<*> (x Data..:? "LastUpdatedDate")
            Prelude.<*> (x Data..:? "StageVariables" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ClientCertificateId")
            Prelude.<*> (x Data..:? "LastDeploymentStatusMessage")
            Prelude.<*> (x Data..:? "DefaultRouteSettings")
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "ApiGatewayManaged")
            Prelude.<*> (x Data..:? "RouteSettings")
      )

instance Prelude.Hashable AwsApiGatewayV2StageDetails where
  hashWithSalt _salt AwsApiGatewayV2StageDetails' {..} =
    _salt `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` accessLogSettings
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` autoDeploy
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` stageVariables
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` clientCertificateId
      `Prelude.hashWithSalt` lastDeploymentStatusMessage
      `Prelude.hashWithSalt` defaultRouteSettings
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` apiGatewayManaged
      `Prelude.hashWithSalt` routeSettings

instance Prelude.NFData AwsApiGatewayV2StageDetails where
  rnf AwsApiGatewayV2StageDetails' {..} =
    Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf accessLogSettings
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf autoDeploy
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf stageVariables
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf clientCertificateId
      `Prelude.seq` Prelude.rnf lastDeploymentStatusMessage
      `Prelude.seq` Prelude.rnf defaultRouteSettings
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf apiGatewayManaged
      `Prelude.seq` Prelude.rnf routeSettings

instance Data.ToJSON AwsApiGatewayV2StageDetails where
  toJSON AwsApiGatewayV2StageDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StageName" Data..=) Prelude.<$> stageName,
            ("AccessLogSettings" Data..=)
              Prelude.<$> accessLogSettings,
            ("DeploymentId" Data..=) Prelude.<$> deploymentId,
            ("AutoDeploy" Data..=) Prelude.<$> autoDeploy,
            ("LastUpdatedDate" Data..=)
              Prelude.<$> lastUpdatedDate,
            ("StageVariables" Data..=)
              Prelude.<$> stageVariables,
            ("Description" Data..=) Prelude.<$> description,
            ("ClientCertificateId" Data..=)
              Prelude.<$> clientCertificateId,
            ("LastDeploymentStatusMessage" Data..=)
              Prelude.<$> lastDeploymentStatusMessage,
            ("DefaultRouteSettings" Data..=)
              Prelude.<$> defaultRouteSettings,
            ("CreatedDate" Data..=) Prelude.<$> createdDate,
            ("ApiGatewayManaged" Data..=)
              Prelude.<$> apiGatewayManaged,
            ("RouteSettings" Data..=) Prelude.<$> routeSettings
          ]
      )
