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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | Information about settings for logging access for the stage.
    accessLogSettings :: Prelude.Maybe AwsApiGatewayAccessLogSettings,
    -- | Indicates whether the stage is managed by API Gateway.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether updates to an API automatically trigger a new
    -- deployment.
    autoDeploy :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of a client certificate for a stage. Supported only for
    -- WebSocket API calls.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the stage was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | Default route settings for the stage.
    defaultRouteSettings :: Prelude.Maybe AwsApiGatewayV2RouteSettings,
    -- | The identifier of the deployment that the stage is associated with.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The status of the last deployment of a stage. Supported only if the
    -- stage has automatic deployment enabled.
    lastDeploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the stage was most recently updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastUpdatedDate :: Prelude.Maybe Prelude.Text,
    -- | The route settings for the stage.
    routeSettings :: Prelude.Maybe AwsApiGatewayV2RouteSettings,
    -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text,
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
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'accessLogSettings', 'awsApiGatewayV2StageDetails_accessLogSettings' - Information about settings for logging access for the stage.
--
-- 'apiGatewayManaged', 'awsApiGatewayV2StageDetails_apiGatewayManaged' - Indicates whether the stage is managed by API Gateway.
--
-- 'autoDeploy', 'awsApiGatewayV2StageDetails_autoDeploy' - Indicates whether updates to an API automatically trigger a new
-- deployment.
--
-- 'clientCertificateId', 'awsApiGatewayV2StageDetails_clientCertificateId' - The identifier of a client certificate for a stage. Supported only for
-- WebSocket API calls.
--
-- 'createdDate', 'awsApiGatewayV2StageDetails_createdDate' - Indicates when the stage was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'defaultRouteSettings', 'awsApiGatewayV2StageDetails_defaultRouteSettings' - Default route settings for the stage.
--
-- 'deploymentId', 'awsApiGatewayV2StageDetails_deploymentId' - The identifier of the deployment that the stage is associated with.
--
-- 'description', 'awsApiGatewayV2StageDetails_description' - The description of the stage.
--
-- 'lastDeploymentStatusMessage', 'awsApiGatewayV2StageDetails_lastDeploymentStatusMessage' - The status of the last deployment of a stage. Supported only if the
-- stage has automatic deployment enabled.
--
-- 'lastUpdatedDate', 'awsApiGatewayV2StageDetails_lastUpdatedDate' - Indicates when the stage was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'routeSettings', 'awsApiGatewayV2StageDetails_routeSettings' - The route settings for the stage.
--
-- 'stageName', 'awsApiGatewayV2StageDetails_stageName' - The name of the stage.
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
newAwsApiGatewayV2StageDetails ::
  AwsApiGatewayV2StageDetails
newAwsApiGatewayV2StageDetails =
  AwsApiGatewayV2StageDetails'
    { accessLogSettings =
        Prelude.Nothing,
      apiGatewayManaged = Prelude.Nothing,
      autoDeploy = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      defaultRouteSettings = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      description = Prelude.Nothing,
      lastDeploymentStatusMessage = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      routeSettings = Prelude.Nothing,
      stageName = Prelude.Nothing,
      stageVariables = Prelude.Nothing
    }

-- | Information about settings for logging access for the stage.
awsApiGatewayV2StageDetails_accessLogSettings :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe AwsApiGatewayAccessLogSettings)
awsApiGatewayV2StageDetails_accessLogSettings = Lens.lens (\AwsApiGatewayV2StageDetails' {accessLogSettings} -> accessLogSettings) (\s@AwsApiGatewayV2StageDetails' {} a -> s {accessLogSettings = a} :: AwsApiGatewayV2StageDetails)

-- | Indicates whether the stage is managed by API Gateway.
awsApiGatewayV2StageDetails_apiGatewayManaged :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Bool)
awsApiGatewayV2StageDetails_apiGatewayManaged = Lens.lens (\AwsApiGatewayV2StageDetails' {apiGatewayManaged} -> apiGatewayManaged) (\s@AwsApiGatewayV2StageDetails' {} a -> s {apiGatewayManaged = a} :: AwsApiGatewayV2StageDetails)

-- | Indicates whether updates to an API automatically trigger a new
-- deployment.
awsApiGatewayV2StageDetails_autoDeploy :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Bool)
awsApiGatewayV2StageDetails_autoDeploy = Lens.lens (\AwsApiGatewayV2StageDetails' {autoDeploy} -> autoDeploy) (\s@AwsApiGatewayV2StageDetails' {} a -> s {autoDeploy = a} :: AwsApiGatewayV2StageDetails)

-- | The identifier of a client certificate for a stage. Supported only for
-- WebSocket API calls.
awsApiGatewayV2StageDetails_clientCertificateId :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_clientCertificateId = Lens.lens (\AwsApiGatewayV2StageDetails' {clientCertificateId} -> clientCertificateId) (\s@AwsApiGatewayV2StageDetails' {} a -> s {clientCertificateId = a} :: AwsApiGatewayV2StageDetails)

-- | Indicates when the stage was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayV2StageDetails_createdDate :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_createdDate = Lens.lens (\AwsApiGatewayV2StageDetails' {createdDate} -> createdDate) (\s@AwsApiGatewayV2StageDetails' {} a -> s {createdDate = a} :: AwsApiGatewayV2StageDetails)

-- | Default route settings for the stage.
awsApiGatewayV2StageDetails_defaultRouteSettings :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe AwsApiGatewayV2RouteSettings)
awsApiGatewayV2StageDetails_defaultRouteSettings = Lens.lens (\AwsApiGatewayV2StageDetails' {defaultRouteSettings} -> defaultRouteSettings) (\s@AwsApiGatewayV2StageDetails' {} a -> s {defaultRouteSettings = a} :: AwsApiGatewayV2StageDetails)

-- | The identifier of the deployment that the stage is associated with.
awsApiGatewayV2StageDetails_deploymentId :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_deploymentId = Lens.lens (\AwsApiGatewayV2StageDetails' {deploymentId} -> deploymentId) (\s@AwsApiGatewayV2StageDetails' {} a -> s {deploymentId = a} :: AwsApiGatewayV2StageDetails)

-- | The description of the stage.
awsApiGatewayV2StageDetails_description :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_description = Lens.lens (\AwsApiGatewayV2StageDetails' {description} -> description) (\s@AwsApiGatewayV2StageDetails' {} a -> s {description = a} :: AwsApiGatewayV2StageDetails)

-- | The status of the last deployment of a stage. Supported only if the
-- stage has automatic deployment enabled.
awsApiGatewayV2StageDetails_lastDeploymentStatusMessage :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_lastDeploymentStatusMessage = Lens.lens (\AwsApiGatewayV2StageDetails' {lastDeploymentStatusMessage} -> lastDeploymentStatusMessage) (\s@AwsApiGatewayV2StageDetails' {} a -> s {lastDeploymentStatusMessage = a} :: AwsApiGatewayV2StageDetails)

-- | Indicates when the stage was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayV2StageDetails_lastUpdatedDate :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_lastUpdatedDate = Lens.lens (\AwsApiGatewayV2StageDetails' {lastUpdatedDate} -> lastUpdatedDate) (\s@AwsApiGatewayV2StageDetails' {} a -> s {lastUpdatedDate = a} :: AwsApiGatewayV2StageDetails)

-- | The route settings for the stage.
awsApiGatewayV2StageDetails_routeSettings :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe AwsApiGatewayV2RouteSettings)
awsApiGatewayV2StageDetails_routeSettings = Lens.lens (\AwsApiGatewayV2StageDetails' {routeSettings} -> routeSettings) (\s@AwsApiGatewayV2StageDetails' {} a -> s {routeSettings = a} :: AwsApiGatewayV2StageDetails)

-- | The name of the stage.
awsApiGatewayV2StageDetails_stageName :: Lens.Lens' AwsApiGatewayV2StageDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2StageDetails_stageName = Lens.lens (\AwsApiGatewayV2StageDetails' {stageName} -> stageName) (\s@AwsApiGatewayV2StageDetails' {} a -> s {stageName = a} :: AwsApiGatewayV2StageDetails)

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

instance Data.FromJSON AwsApiGatewayV2StageDetails where
  parseJSON =
    Data.withObject
      "AwsApiGatewayV2StageDetails"
      ( \x ->
          AwsApiGatewayV2StageDetails'
            Prelude.<$> (x Data..:? "AccessLogSettings")
            Prelude.<*> (x Data..:? "ApiGatewayManaged")
            Prelude.<*> (x Data..:? "AutoDeploy")
            Prelude.<*> (x Data..:? "ClientCertificateId")
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "DefaultRouteSettings")
            Prelude.<*> (x Data..:? "DeploymentId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastDeploymentStatusMessage")
            Prelude.<*> (x Data..:? "LastUpdatedDate")
            Prelude.<*> (x Data..:? "RouteSettings")
            Prelude.<*> (x Data..:? "StageName")
            Prelude.<*> ( x
                            Data..:? "StageVariables"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsApiGatewayV2StageDetails where
  hashWithSalt _salt AwsApiGatewayV2StageDetails' {..} =
    _salt
      `Prelude.hashWithSalt` accessLogSettings
      `Prelude.hashWithSalt` apiGatewayManaged
      `Prelude.hashWithSalt` autoDeploy
      `Prelude.hashWithSalt` clientCertificateId
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` defaultRouteSettings
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastDeploymentStatusMessage
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` routeSettings
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` stageVariables

instance Prelude.NFData AwsApiGatewayV2StageDetails where
  rnf AwsApiGatewayV2StageDetails' {..} =
    Prelude.rnf accessLogSettings
      `Prelude.seq` Prelude.rnf apiGatewayManaged
      `Prelude.seq` Prelude.rnf autoDeploy
      `Prelude.seq` Prelude.rnf clientCertificateId
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf defaultRouteSettings
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastDeploymentStatusMessage
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf routeSettings
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf stageVariables

instance Data.ToJSON AwsApiGatewayV2StageDetails where
  toJSON AwsApiGatewayV2StageDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessLogSettings" Data..=)
              Prelude.<$> accessLogSettings,
            ("ApiGatewayManaged" Data..=)
              Prelude.<$> apiGatewayManaged,
            ("AutoDeploy" Data..=) Prelude.<$> autoDeploy,
            ("ClientCertificateId" Data..=)
              Prelude.<$> clientCertificateId,
            ("CreatedDate" Data..=) Prelude.<$> createdDate,
            ("DefaultRouteSettings" Data..=)
              Prelude.<$> defaultRouteSettings,
            ("DeploymentId" Data..=) Prelude.<$> deploymentId,
            ("Description" Data..=) Prelude.<$> description,
            ("LastDeploymentStatusMessage" Data..=)
              Prelude.<$> lastDeploymentStatusMessage,
            ("LastUpdatedDate" Data..=)
              Prelude.<$> lastUpdatedDate,
            ("RouteSettings" Data..=) Prelude.<$> routeSettings,
            ("StageName" Data..=) Prelude.<$> stageName,
            ("StageVariables" Data..=)
              Prelude.<$> stageVariables
          ]
      )
