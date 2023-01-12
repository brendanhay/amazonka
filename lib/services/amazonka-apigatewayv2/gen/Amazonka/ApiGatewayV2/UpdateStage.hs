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
-- Module      : Amazonka.ApiGatewayV2.UpdateStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Stage.
module Amazonka.ApiGatewayV2.UpdateStage
  ( -- * Creating a Request
    UpdateStage (..),
    newUpdateStage,

    -- * Request Lenses
    updateStage_accessLogSettings,
    updateStage_autoDeploy,
    updateStage_clientCertificateId,
    updateStage_defaultRouteSettings,
    updateStage_deploymentId,
    updateStage_description,
    updateStage_routeSettings,
    updateStage_stageVariables,
    updateStage_stageName,
    updateStage_apiId,

    -- * Destructuring the Response
    UpdateStageResponse (..),
    newUpdateStageResponse,

    -- * Response Lenses
    updateStageResponse_accessLogSettings,
    updateStageResponse_apiGatewayManaged,
    updateStageResponse_autoDeploy,
    updateStageResponse_clientCertificateId,
    updateStageResponse_createdDate,
    updateStageResponse_defaultRouteSettings,
    updateStageResponse_deploymentId,
    updateStageResponse_description,
    updateStageResponse_lastDeploymentStatusMessage,
    updateStageResponse_lastUpdatedDate,
    updateStageResponse_routeSettings,
    updateStageResponse_stageName,
    updateStageResponse_stageVariables,
    updateStageResponse_tags,
    updateStageResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates a Stage.
--
-- /See:/ 'newUpdateStage' smart constructor.
data UpdateStage = UpdateStage'
  { -- | Settings for logging access in this stage.
    accessLogSettings :: Prelude.Maybe AccessLogSettings,
    -- | Specifies whether updates to an API automatically trigger a new
    -- deployment. The default value is false.
    autoDeploy :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of a client certificate for a Stage.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The default route settings for the stage.
    defaultRouteSettings :: Prelude.Maybe RouteSettings,
    -- | The deployment identifier for the API stage. Can\'t be updated if
    -- autoDeploy is enabled.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The description for the API stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | Route settings for the stage.
    routeSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings),
    -- | A map that defines the stage variables for a Stage. Variable names can
    -- have alphanumeric and underscore characters, and the values must match
    -- [A-Za-z0-9-._~:\/?#&=,]+.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The stage name. Stage names can contain only alphanumeric characters,
    -- hyphens, and underscores, or be $default. Maximum length is 128
    -- characters.
    stageName :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessLogSettings', 'updateStage_accessLogSettings' - Settings for logging access in this stage.
--
-- 'autoDeploy', 'updateStage_autoDeploy' - Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
--
-- 'clientCertificateId', 'updateStage_clientCertificateId' - The identifier of a client certificate for a Stage.
--
-- 'defaultRouteSettings', 'updateStage_defaultRouteSettings' - The default route settings for the stage.
--
-- 'deploymentId', 'updateStage_deploymentId' - The deployment identifier for the API stage. Can\'t be updated if
-- autoDeploy is enabled.
--
-- 'description', 'updateStage_description' - The description for the API stage.
--
-- 'routeSettings', 'updateStage_routeSettings' - Route settings for the stage.
--
-- 'stageVariables', 'updateStage_stageVariables' - A map that defines the stage variables for a Stage. Variable names can
-- have alphanumeric and underscore characters, and the values must match
-- [A-Za-z0-9-._~:\/?#&=,]+.
--
-- 'stageName', 'updateStage_stageName' - The stage name. Stage names can contain only alphanumeric characters,
-- hyphens, and underscores, or be $default. Maximum length is 128
-- characters.
--
-- 'apiId', 'updateStage_apiId' - The API identifier.
newUpdateStage ::
  -- | 'stageName'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  UpdateStage
newUpdateStage pStageName_ pApiId_ =
  UpdateStage'
    { accessLogSettings = Prelude.Nothing,
      autoDeploy = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      defaultRouteSettings = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      description = Prelude.Nothing,
      routeSettings = Prelude.Nothing,
      stageVariables = Prelude.Nothing,
      stageName = pStageName_,
      apiId = pApiId_
    }

-- | Settings for logging access in this stage.
updateStage_accessLogSettings :: Lens.Lens' UpdateStage (Prelude.Maybe AccessLogSettings)
updateStage_accessLogSettings = Lens.lens (\UpdateStage' {accessLogSettings} -> accessLogSettings) (\s@UpdateStage' {} a -> s {accessLogSettings = a} :: UpdateStage)

-- | Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
updateStage_autoDeploy :: Lens.Lens' UpdateStage (Prelude.Maybe Prelude.Bool)
updateStage_autoDeploy = Lens.lens (\UpdateStage' {autoDeploy} -> autoDeploy) (\s@UpdateStage' {} a -> s {autoDeploy = a} :: UpdateStage)

-- | The identifier of a client certificate for a Stage.
updateStage_clientCertificateId :: Lens.Lens' UpdateStage (Prelude.Maybe Prelude.Text)
updateStage_clientCertificateId = Lens.lens (\UpdateStage' {clientCertificateId} -> clientCertificateId) (\s@UpdateStage' {} a -> s {clientCertificateId = a} :: UpdateStage)

-- | The default route settings for the stage.
updateStage_defaultRouteSettings :: Lens.Lens' UpdateStage (Prelude.Maybe RouteSettings)
updateStage_defaultRouteSettings = Lens.lens (\UpdateStage' {defaultRouteSettings} -> defaultRouteSettings) (\s@UpdateStage' {} a -> s {defaultRouteSettings = a} :: UpdateStage)

-- | The deployment identifier for the API stage. Can\'t be updated if
-- autoDeploy is enabled.
updateStage_deploymentId :: Lens.Lens' UpdateStage (Prelude.Maybe Prelude.Text)
updateStage_deploymentId = Lens.lens (\UpdateStage' {deploymentId} -> deploymentId) (\s@UpdateStage' {} a -> s {deploymentId = a} :: UpdateStage)

-- | The description for the API stage.
updateStage_description :: Lens.Lens' UpdateStage (Prelude.Maybe Prelude.Text)
updateStage_description = Lens.lens (\UpdateStage' {description} -> description) (\s@UpdateStage' {} a -> s {description = a} :: UpdateStage)

-- | Route settings for the stage.
updateStage_routeSettings :: Lens.Lens' UpdateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings))
updateStage_routeSettings = Lens.lens (\UpdateStage' {routeSettings} -> routeSettings) (\s@UpdateStage' {} a -> s {routeSettings = a} :: UpdateStage) Prelude.. Lens.mapping Lens.coerced

-- | A map that defines the stage variables for a Stage. Variable names can
-- have alphanumeric and underscore characters, and the values must match
-- [A-Za-z0-9-._~:\/?#&=,]+.
updateStage_stageVariables :: Lens.Lens' UpdateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateStage_stageVariables = Lens.lens (\UpdateStage' {stageVariables} -> stageVariables) (\s@UpdateStage' {} a -> s {stageVariables = a} :: UpdateStage) Prelude.. Lens.mapping Lens.coerced

-- | The stage name. Stage names can contain only alphanumeric characters,
-- hyphens, and underscores, or be $default. Maximum length is 128
-- characters.
updateStage_stageName :: Lens.Lens' UpdateStage Prelude.Text
updateStage_stageName = Lens.lens (\UpdateStage' {stageName} -> stageName) (\s@UpdateStage' {} a -> s {stageName = a} :: UpdateStage)

-- | The API identifier.
updateStage_apiId :: Lens.Lens' UpdateStage Prelude.Text
updateStage_apiId = Lens.lens (\UpdateStage' {apiId} -> apiId) (\s@UpdateStage' {} a -> s {apiId = a} :: UpdateStage)

instance Core.AWSRequest UpdateStage where
  type AWSResponse UpdateStage = UpdateStageResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStageResponse'
            Prelude.<$> (x Data..?> "accessLogSettings")
            Prelude.<*> (x Data..?> "apiGatewayManaged")
            Prelude.<*> (x Data..?> "autoDeploy")
            Prelude.<*> (x Data..?> "clientCertificateId")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "defaultRouteSettings")
            Prelude.<*> (x Data..?> "deploymentId")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "lastDeploymentStatusMessage")
            Prelude.<*> (x Data..?> "lastUpdatedDate")
            Prelude.<*> (x Data..?> "routeSettings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "stageName")
            Prelude.<*> (x Data..?> "stageVariables" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStage where
  hashWithSalt _salt UpdateStage' {..} =
    _salt `Prelude.hashWithSalt` accessLogSettings
      `Prelude.hashWithSalt` autoDeploy
      `Prelude.hashWithSalt` clientCertificateId
      `Prelude.hashWithSalt` defaultRouteSettings
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` routeSettings
      `Prelude.hashWithSalt` stageVariables
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData UpdateStage where
  rnf UpdateStage' {..} =
    Prelude.rnf accessLogSettings
      `Prelude.seq` Prelude.rnf autoDeploy
      `Prelude.seq` Prelude.rnf clientCertificateId
      `Prelude.seq` Prelude.rnf defaultRouteSettings
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf routeSettings
      `Prelude.seq` Prelude.rnf stageVariables
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders UpdateStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStage where
  toJSON UpdateStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessLogSettings" Data..=)
              Prelude.<$> accessLogSettings,
            ("autoDeploy" Data..=) Prelude.<$> autoDeploy,
            ("clientCertificateId" Data..=)
              Prelude.<$> clientCertificateId,
            ("defaultRouteSettings" Data..=)
              Prelude.<$> defaultRouteSettings,
            ("deploymentId" Data..=) Prelude.<$> deploymentId,
            ("description" Data..=) Prelude.<$> description,
            ("routeSettings" Data..=) Prelude.<$> routeSettings,
            ("stageVariables" Data..=)
              Prelude.<$> stageVariables
          ]
      )

instance Data.ToPath UpdateStage where
  toPath UpdateStage' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/stages/",
        Data.toBS stageName
      ]

instance Data.ToQuery UpdateStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStageResponse' smart constructor.
data UpdateStageResponse = UpdateStageResponse'
  { -- | Settings for logging access in this stage.
    accessLogSettings :: Prelude.Maybe AccessLogSettings,
    -- | Specifies whether a stage is managed by API Gateway. If you created an
    -- API using quick create, the $default stage is managed by API Gateway.
    -- You can\'t modify the $default stage.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether updates to an API automatically trigger a new
    -- deployment. The default value is false.
    autoDeploy :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of a client certificate for a Stage. Supported only for
    -- WebSocket APIs.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the stage was created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | Default route settings for the stage.
    defaultRouteSettings :: Prelude.Maybe RouteSettings,
    -- | The identifier of the Deployment that the Stage is associated with.
    -- Can\'t be updated if autoDeploy is enabled.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | Describes the status of the last deployment of a stage. Supported only
    -- for stages with autoDeploy enabled.
    lastDeploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the stage was last updated.
    lastUpdatedDate :: Prelude.Maybe Data.ISO8601,
    -- | Route settings for the stage, by routeKey.
    routeSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings),
    -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | A map that defines the stage variables for a stage resource. Variable
    -- names can have alphanumeric and underscore characters, and the values
    -- must match [A-Za-z0-9-._~:\/?#&=,]+.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessLogSettings', 'updateStageResponse_accessLogSettings' - Settings for logging access in this stage.
--
-- 'apiGatewayManaged', 'updateStageResponse_apiGatewayManaged' - Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
--
-- 'autoDeploy', 'updateStageResponse_autoDeploy' - Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
--
-- 'clientCertificateId', 'updateStageResponse_clientCertificateId' - The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
--
-- 'createdDate', 'updateStageResponse_createdDate' - The timestamp when the stage was created.
--
-- 'defaultRouteSettings', 'updateStageResponse_defaultRouteSettings' - Default route settings for the stage.
--
-- 'deploymentId', 'updateStageResponse_deploymentId' - The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
--
-- 'description', 'updateStageResponse_description' - The description of the stage.
--
-- 'lastDeploymentStatusMessage', 'updateStageResponse_lastDeploymentStatusMessage' - Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
--
-- 'lastUpdatedDate', 'updateStageResponse_lastUpdatedDate' - The timestamp when the stage was last updated.
--
-- 'routeSettings', 'updateStageResponse_routeSettings' - Route settings for the stage, by routeKey.
--
-- 'stageName', 'updateStageResponse_stageName' - The name of the stage.
--
-- 'stageVariables', 'updateStageResponse_stageVariables' - A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
--
-- 'tags', 'updateStageResponse_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'httpStatus', 'updateStageResponse_httpStatus' - The response's http status code.
newUpdateStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStageResponse
newUpdateStageResponse pHttpStatus_ =
  UpdateStageResponse'
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
      stageVariables = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Settings for logging access in this stage.
updateStageResponse_accessLogSettings :: Lens.Lens' UpdateStageResponse (Prelude.Maybe AccessLogSettings)
updateStageResponse_accessLogSettings = Lens.lens (\UpdateStageResponse' {accessLogSettings} -> accessLogSettings) (\s@UpdateStageResponse' {} a -> s {accessLogSettings = a} :: UpdateStageResponse)

-- | Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
updateStageResponse_apiGatewayManaged :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.Bool)
updateStageResponse_apiGatewayManaged = Lens.lens (\UpdateStageResponse' {apiGatewayManaged} -> apiGatewayManaged) (\s@UpdateStageResponse' {} a -> s {apiGatewayManaged = a} :: UpdateStageResponse)

-- | Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
updateStageResponse_autoDeploy :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.Bool)
updateStageResponse_autoDeploy = Lens.lens (\UpdateStageResponse' {autoDeploy} -> autoDeploy) (\s@UpdateStageResponse' {} a -> s {autoDeploy = a} :: UpdateStageResponse)

-- | The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
updateStageResponse_clientCertificateId :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.Text)
updateStageResponse_clientCertificateId = Lens.lens (\UpdateStageResponse' {clientCertificateId} -> clientCertificateId) (\s@UpdateStageResponse' {} a -> s {clientCertificateId = a} :: UpdateStageResponse)

-- | The timestamp when the stage was created.
updateStageResponse_createdDate :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.UTCTime)
updateStageResponse_createdDate = Lens.lens (\UpdateStageResponse' {createdDate} -> createdDate) (\s@UpdateStageResponse' {} a -> s {createdDate = a} :: UpdateStageResponse) Prelude.. Lens.mapping Data._Time

-- | Default route settings for the stage.
updateStageResponse_defaultRouteSettings :: Lens.Lens' UpdateStageResponse (Prelude.Maybe RouteSettings)
updateStageResponse_defaultRouteSettings = Lens.lens (\UpdateStageResponse' {defaultRouteSettings} -> defaultRouteSettings) (\s@UpdateStageResponse' {} a -> s {defaultRouteSettings = a} :: UpdateStageResponse)

-- | The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
updateStageResponse_deploymentId :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.Text)
updateStageResponse_deploymentId = Lens.lens (\UpdateStageResponse' {deploymentId} -> deploymentId) (\s@UpdateStageResponse' {} a -> s {deploymentId = a} :: UpdateStageResponse)

-- | The description of the stage.
updateStageResponse_description :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.Text)
updateStageResponse_description = Lens.lens (\UpdateStageResponse' {description} -> description) (\s@UpdateStageResponse' {} a -> s {description = a} :: UpdateStageResponse)

-- | Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
updateStageResponse_lastDeploymentStatusMessage :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.Text)
updateStageResponse_lastDeploymentStatusMessage = Lens.lens (\UpdateStageResponse' {lastDeploymentStatusMessage} -> lastDeploymentStatusMessage) (\s@UpdateStageResponse' {} a -> s {lastDeploymentStatusMessage = a} :: UpdateStageResponse)

-- | The timestamp when the stage was last updated.
updateStageResponse_lastUpdatedDate :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.UTCTime)
updateStageResponse_lastUpdatedDate = Lens.lens (\UpdateStageResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@UpdateStageResponse' {} a -> s {lastUpdatedDate = a} :: UpdateStageResponse) Prelude.. Lens.mapping Data._Time

-- | Route settings for the stage, by routeKey.
updateStageResponse_routeSettings :: Lens.Lens' UpdateStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings))
updateStageResponse_routeSettings = Lens.lens (\UpdateStageResponse' {routeSettings} -> routeSettings) (\s@UpdateStageResponse' {} a -> s {routeSettings = a} :: UpdateStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stage.
updateStageResponse_stageName :: Lens.Lens' UpdateStageResponse (Prelude.Maybe Prelude.Text)
updateStageResponse_stageName = Lens.lens (\UpdateStageResponse' {stageName} -> stageName) (\s@UpdateStageResponse' {} a -> s {stageName = a} :: UpdateStageResponse)

-- | A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
updateStageResponse_stageVariables :: Lens.Lens' UpdateStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateStageResponse_stageVariables = Lens.lens (\UpdateStageResponse' {stageVariables} -> stageVariables) (\s@UpdateStageResponse' {} a -> s {stageVariables = a} :: UpdateStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The collection of tags. Each tag element is associated with a given
-- resource.
updateStageResponse_tags :: Lens.Lens' UpdateStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateStageResponse_tags = Lens.lens (\UpdateStageResponse' {tags} -> tags) (\s@UpdateStageResponse' {} a -> s {tags = a} :: UpdateStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateStageResponse_httpStatus :: Lens.Lens' UpdateStageResponse Prelude.Int
updateStageResponse_httpStatus = Lens.lens (\UpdateStageResponse' {httpStatus} -> httpStatus) (\s@UpdateStageResponse' {} a -> s {httpStatus = a} :: UpdateStageResponse)

instance Prelude.NFData UpdateStageResponse where
  rnf UpdateStageResponse' {..} =
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
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
