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
-- Module      : Network.AWS.ApiGatewayV2.Types.Stage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApiGatewayV2.Types.Stage where

import Network.AWS.ApiGatewayV2.Types.AccessLogSettings
import Network.AWS.ApiGatewayV2.Types.RouteSettings
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an API stage.
--
-- /See:/ 'newStage' smart constructor.
data Stage = Stage'
  { -- | Describes the status of the last deployment of a stage. Supported only
    -- for stages with autoDeploy enabled.
    lastDeploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Deployment that the Stage is associated with.
    -- Can\'t be updated if autoDeploy is enabled.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | Route settings for the stage, by routeKey.
    routeSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings),
    -- | Settings for logging access in this stage.
    accessLogSettings :: Prelude.Maybe AccessLogSettings,
    -- | The identifier of a client certificate for a Stage. Supported only for
    -- WebSocket APIs.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | A map that defines the stage variables for a stage resource. Variable
    -- names can have alphanumeric and underscore characters, and the values
    -- must match [A-Za-z0-9-._~:\/?#&=,]+.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether updates to an API automatically trigger a new
    -- deployment. The default value is false.
    autoDeploy :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the stage was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | Default route settings for the stage.
    defaultRouteSettings :: Prelude.Maybe RouteSettings,
    -- | Specifies whether a stage is managed by API Gateway. If you created an
    -- API using quick create, the $default stage is managed by API Gateway.
    -- You can\'t modify the $default stage.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the stage was last updated.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the stage.
    stageName :: Prelude.Text
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
-- 'lastDeploymentStatusMessage', 'stage_lastDeploymentStatusMessage' - Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
--
-- 'deploymentId', 'stage_deploymentId' - The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
--
-- 'routeSettings', 'stage_routeSettings' - Route settings for the stage, by routeKey.
--
-- 'accessLogSettings', 'stage_accessLogSettings' - Settings for logging access in this stage.
--
-- 'clientCertificateId', 'stage_clientCertificateId' - The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
--
-- 'stageVariables', 'stage_stageVariables' - A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
--
-- 'autoDeploy', 'stage_autoDeploy' - Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
--
-- 'createdDate', 'stage_createdDate' - The timestamp when the stage was created.
--
-- 'defaultRouteSettings', 'stage_defaultRouteSettings' - Default route settings for the stage.
--
-- 'apiGatewayManaged', 'stage_apiGatewayManaged' - Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
--
-- 'lastUpdatedDate', 'stage_lastUpdatedDate' - The timestamp when the stage was last updated.
--
-- 'description', 'stage_description' - The description of the stage.
--
-- 'tags', 'stage_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'stageName', 'stage_stageName' - The name of the stage.
newStage ::
  -- | 'stageName'
  Prelude.Text ->
  Stage
newStage pStageName_ =
  Stage'
    { lastDeploymentStatusMessage =
        Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      routeSettings = Prelude.Nothing,
      accessLogSettings = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      stageVariables = Prelude.Nothing,
      autoDeploy = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      defaultRouteSettings = Prelude.Nothing,
      apiGatewayManaged = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      stageName = pStageName_
    }

-- | Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
stage_lastDeploymentStatusMessage :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_lastDeploymentStatusMessage = Lens.lens (\Stage' {lastDeploymentStatusMessage} -> lastDeploymentStatusMessage) (\s@Stage' {} a -> s {lastDeploymentStatusMessage = a} :: Stage)

-- | The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
stage_deploymentId :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_deploymentId = Lens.lens (\Stage' {deploymentId} -> deploymentId) (\s@Stage' {} a -> s {deploymentId = a} :: Stage)

-- | Route settings for the stage, by routeKey.
stage_routeSettings :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings))
stage_routeSettings = Lens.lens (\Stage' {routeSettings} -> routeSettings) (\s@Stage' {} a -> s {routeSettings = a} :: Stage) Prelude.. Lens.mapping Lens.coerced

-- | Settings for logging access in this stage.
stage_accessLogSettings :: Lens.Lens' Stage (Prelude.Maybe AccessLogSettings)
stage_accessLogSettings = Lens.lens (\Stage' {accessLogSettings} -> accessLogSettings) (\s@Stage' {} a -> s {accessLogSettings = a} :: Stage)

-- | The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
stage_clientCertificateId :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_clientCertificateId = Lens.lens (\Stage' {clientCertificateId} -> clientCertificateId) (\s@Stage' {} a -> s {clientCertificateId = a} :: Stage)

-- | A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
stage_stageVariables :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stage_stageVariables = Lens.lens (\Stage' {stageVariables} -> stageVariables) (\s@Stage' {} a -> s {stageVariables = a} :: Stage) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
stage_autoDeploy :: Lens.Lens' Stage (Prelude.Maybe Prelude.Bool)
stage_autoDeploy = Lens.lens (\Stage' {autoDeploy} -> autoDeploy) (\s@Stage' {} a -> s {autoDeploy = a} :: Stage)

-- | The timestamp when the stage was created.
stage_createdDate :: Lens.Lens' Stage (Prelude.Maybe Prelude.UTCTime)
stage_createdDate = Lens.lens (\Stage' {createdDate} -> createdDate) (\s@Stage' {} a -> s {createdDate = a} :: Stage) Prelude.. Lens.mapping Core._Time

-- | Default route settings for the stage.
stage_defaultRouteSettings :: Lens.Lens' Stage (Prelude.Maybe RouteSettings)
stage_defaultRouteSettings = Lens.lens (\Stage' {defaultRouteSettings} -> defaultRouteSettings) (\s@Stage' {} a -> s {defaultRouteSettings = a} :: Stage)

-- | Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
stage_apiGatewayManaged :: Lens.Lens' Stage (Prelude.Maybe Prelude.Bool)
stage_apiGatewayManaged = Lens.lens (\Stage' {apiGatewayManaged} -> apiGatewayManaged) (\s@Stage' {} a -> s {apiGatewayManaged = a} :: Stage)

-- | The timestamp when the stage was last updated.
stage_lastUpdatedDate :: Lens.Lens' Stage (Prelude.Maybe Prelude.UTCTime)
stage_lastUpdatedDate = Lens.lens (\Stage' {lastUpdatedDate} -> lastUpdatedDate) (\s@Stage' {} a -> s {lastUpdatedDate = a} :: Stage) Prelude.. Lens.mapping Core._Time

-- | The description of the stage.
stage_description :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_description = Lens.lens (\Stage' {description} -> description) (\s@Stage' {} a -> s {description = a} :: Stage)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
stage_tags :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stage_tags = Lens.lens (\Stage' {tags} -> tags) (\s@Stage' {} a -> s {tags = a} :: Stage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stage.
stage_stageName :: Lens.Lens' Stage Prelude.Text
stage_stageName = Lens.lens (\Stage' {stageName} -> stageName) (\s@Stage' {} a -> s {stageName = a} :: Stage)

instance Core.FromJSON Stage where
  parseJSON =
    Core.withObject
      "Stage"
      ( \x ->
          Stage'
            Prelude.<$> (x Core..:? "lastDeploymentStatusMessage")
            Prelude.<*> (x Core..:? "deploymentId")
            Prelude.<*> (x Core..:? "routeSettings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "accessLogSettings")
            Prelude.<*> (x Core..:? "clientCertificateId")
            Prelude.<*> (x Core..:? "stageVariables" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "autoDeploy")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "defaultRouteSettings")
            Prelude.<*> (x Core..:? "apiGatewayManaged")
            Prelude.<*> (x Core..:? "lastUpdatedDate")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "stageName")
      )

instance Prelude.Hashable Stage

instance Prelude.NFData Stage
