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
-- Module      : Amazonka.ApiGatewayV2.CreateStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Stage for an API.
module Amazonka.ApiGatewayV2.CreateStage
  ( -- * Creating a Request
    CreateStage (..),
    newCreateStage,

    -- * Request Lenses
    createStage_accessLogSettings,
    createStage_autoDeploy,
    createStage_clientCertificateId,
    createStage_defaultRouteSettings,
    createStage_deploymentId,
    createStage_description,
    createStage_routeSettings,
    createStage_stageVariables,
    createStage_tags,
    createStage_apiId,
    createStage_stageName,

    -- * Destructuring the Response
    CreateStageResponse (..),
    newCreateStageResponse,

    -- * Response Lenses
    createStageResponse_accessLogSettings,
    createStageResponse_apiGatewayManaged,
    createStageResponse_autoDeploy,
    createStageResponse_clientCertificateId,
    createStageResponse_createdDate,
    createStageResponse_defaultRouteSettings,
    createStageResponse_deploymentId,
    createStageResponse_description,
    createStageResponse_lastDeploymentStatusMessage,
    createStageResponse_lastUpdatedDate,
    createStageResponse_routeSettings,
    createStageResponse_stageName,
    createStageResponse_stageVariables,
    createStageResponse_tags,
    createStageResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new Stage resource to represent a stage.
--
-- /See:/ 'newCreateStage' smart constructor.
data CreateStage = CreateStage'
  { -- | Settings for logging access in this stage.
    accessLogSettings :: Prelude.Maybe AccessLogSettings,
    -- | Specifies whether updates to an API automatically trigger a new
    -- deployment. The default value is false.
    autoDeploy :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of a client certificate for a Stage. Supported only for
    -- WebSocket APIs.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The default route settings for the stage.
    defaultRouteSettings :: Prelude.Maybe RouteSettings,
    -- | The deployment identifier of the API stage.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The description for the API stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | Route settings for the stage, by routeKey.
    routeSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings),
    -- | A map that defines the stage variables for a Stage. Variable names can
    -- have alphanumeric and underscore characters, and the values must match
    -- [A-Za-z0-9-._~:\/?#&=,]+.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
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
-- 'accessLogSettings', 'createStage_accessLogSettings' - Settings for logging access in this stage.
--
-- 'autoDeploy', 'createStage_autoDeploy' - Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
--
-- 'clientCertificateId', 'createStage_clientCertificateId' - The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
--
-- 'defaultRouteSettings', 'createStage_defaultRouteSettings' - The default route settings for the stage.
--
-- 'deploymentId', 'createStage_deploymentId' - The deployment identifier of the API stage.
--
-- 'description', 'createStage_description' - The description for the API stage.
--
-- 'routeSettings', 'createStage_routeSettings' - Route settings for the stage, by routeKey.
--
-- 'stageVariables', 'createStage_stageVariables' - A map that defines the stage variables for a Stage. Variable names can
-- have alphanumeric and underscore characters, and the values must match
-- [A-Za-z0-9-._~:\/?#&=,]+.
--
-- 'tags', 'createStage_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'apiId', 'createStage_apiId' - The API identifier.
--
-- 'stageName', 'createStage_stageName' - The name of the stage.
newCreateStage ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  CreateStage
newCreateStage pApiId_ pStageName_ =
  CreateStage'
    { accessLogSettings = Prelude.Nothing,
      autoDeploy = Prelude.Nothing,
      clientCertificateId = Prelude.Nothing,
      defaultRouteSettings = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      description = Prelude.Nothing,
      routeSettings = Prelude.Nothing,
      stageVariables = Prelude.Nothing,
      tags = Prelude.Nothing,
      apiId = pApiId_,
      stageName = pStageName_
    }

-- | Settings for logging access in this stage.
createStage_accessLogSettings :: Lens.Lens' CreateStage (Prelude.Maybe AccessLogSettings)
createStage_accessLogSettings = Lens.lens (\CreateStage' {accessLogSettings} -> accessLogSettings) (\s@CreateStage' {} a -> s {accessLogSettings = a} :: CreateStage)

-- | Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
createStage_autoDeploy :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Bool)
createStage_autoDeploy = Lens.lens (\CreateStage' {autoDeploy} -> autoDeploy) (\s@CreateStage' {} a -> s {autoDeploy = a} :: CreateStage)

-- | The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
createStage_clientCertificateId :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_clientCertificateId = Lens.lens (\CreateStage' {clientCertificateId} -> clientCertificateId) (\s@CreateStage' {} a -> s {clientCertificateId = a} :: CreateStage)

-- | The default route settings for the stage.
createStage_defaultRouteSettings :: Lens.Lens' CreateStage (Prelude.Maybe RouteSettings)
createStage_defaultRouteSettings = Lens.lens (\CreateStage' {defaultRouteSettings} -> defaultRouteSettings) (\s@CreateStage' {} a -> s {defaultRouteSettings = a} :: CreateStage)

-- | The deployment identifier of the API stage.
createStage_deploymentId :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_deploymentId = Lens.lens (\CreateStage' {deploymentId} -> deploymentId) (\s@CreateStage' {} a -> s {deploymentId = a} :: CreateStage)

-- | The description for the API stage.
createStage_description :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_description = Lens.lens (\CreateStage' {description} -> description) (\s@CreateStage' {} a -> s {description = a} :: CreateStage)

-- | Route settings for the stage, by routeKey.
createStage_routeSettings :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings))
createStage_routeSettings = Lens.lens (\CreateStage' {routeSettings} -> routeSettings) (\s@CreateStage' {} a -> s {routeSettings = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | A map that defines the stage variables for a Stage. Variable names can
-- have alphanumeric and underscore characters, and the values must match
-- [A-Za-z0-9-._~:\/?#&=,]+.
createStage_stageVariables :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStage_stageVariables = Lens.lens (\CreateStage' {stageVariables} -> stageVariables) (\s@CreateStage' {} a -> s {stageVariables = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | The collection of tags. Each tag element is associated with a given
-- resource.
createStage_tags :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStage_tags = Lens.lens (\CreateStage' {tags} -> tags) (\s@CreateStage' {} a -> s {tags = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | The API identifier.
createStage_apiId :: Lens.Lens' CreateStage Prelude.Text
createStage_apiId = Lens.lens (\CreateStage' {apiId} -> apiId) (\s@CreateStage' {} a -> s {apiId = a} :: CreateStage)

-- | The name of the stage.
createStage_stageName :: Lens.Lens' CreateStage Prelude.Text
createStage_stageName = Lens.lens (\CreateStage' {stageName} -> stageName) (\s@CreateStage' {} a -> s {stageName = a} :: CreateStage)

instance Core.AWSRequest CreateStage where
  type AWSResponse CreateStage = CreateStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStageResponse'
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

instance Prelude.Hashable CreateStage where
  hashWithSalt _salt CreateStage' {..} =
    _salt
      `Prelude.hashWithSalt` accessLogSettings
      `Prelude.hashWithSalt` autoDeploy
      `Prelude.hashWithSalt` clientCertificateId
      `Prelude.hashWithSalt` defaultRouteSettings
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` routeSettings
      `Prelude.hashWithSalt` stageVariables
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData CreateStage where
  rnf CreateStage' {..} =
    Prelude.rnf accessLogSettings `Prelude.seq`
      Prelude.rnf autoDeploy `Prelude.seq`
        Prelude.rnf clientCertificateId `Prelude.seq`
          Prelude.rnf defaultRouteSettings `Prelude.seq`
            Prelude.rnf deploymentId `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf routeSettings `Prelude.seq`
                  Prelude.rnf stageVariables `Prelude.seq`
                    Prelude.rnf tags `Prelude.seq`
                      Prelude.rnf apiId `Prelude.seq`
                        Prelude.rnf stageName

instance Data.ToHeaders CreateStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStage where
  toJSON CreateStage' {..} =
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
              Prelude.<$> stageVariables,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("stageName" Data..= stageName)
          ]
      )

instance Data.ToPath CreateStage where
  toPath CreateStage' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/stages"]

instance Data.ToQuery CreateStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStageResponse' smart constructor.
data CreateStageResponse = CreateStageResponse'
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
-- Create a value of 'CreateStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessLogSettings', 'createStageResponse_accessLogSettings' - Settings for logging access in this stage.
--
-- 'apiGatewayManaged', 'createStageResponse_apiGatewayManaged' - Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
--
-- 'autoDeploy', 'createStageResponse_autoDeploy' - Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
--
-- 'clientCertificateId', 'createStageResponse_clientCertificateId' - The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
--
-- 'createdDate', 'createStageResponse_createdDate' - The timestamp when the stage was created.
--
-- 'defaultRouteSettings', 'createStageResponse_defaultRouteSettings' - Default route settings for the stage.
--
-- 'deploymentId', 'createStageResponse_deploymentId' - The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
--
-- 'description', 'createStageResponse_description' - The description of the stage.
--
-- 'lastDeploymentStatusMessage', 'createStageResponse_lastDeploymentStatusMessage' - Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
--
-- 'lastUpdatedDate', 'createStageResponse_lastUpdatedDate' - The timestamp when the stage was last updated.
--
-- 'routeSettings', 'createStageResponse_routeSettings' - Route settings for the stage, by routeKey.
--
-- 'stageName', 'createStageResponse_stageName' - The name of the stage.
--
-- 'stageVariables', 'createStageResponse_stageVariables' - A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
--
-- 'tags', 'createStageResponse_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'httpStatus', 'createStageResponse_httpStatus' - The response's http status code.
newCreateStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStageResponse
newCreateStageResponse pHttpStatus_ =
  CreateStageResponse'
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
createStageResponse_accessLogSettings :: Lens.Lens' CreateStageResponse (Prelude.Maybe AccessLogSettings)
createStageResponse_accessLogSettings = Lens.lens (\CreateStageResponse' {accessLogSettings} -> accessLogSettings) (\s@CreateStageResponse' {} a -> s {accessLogSettings = a} :: CreateStageResponse)

-- | Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
createStageResponse_apiGatewayManaged :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.Bool)
createStageResponse_apiGatewayManaged = Lens.lens (\CreateStageResponse' {apiGatewayManaged} -> apiGatewayManaged) (\s@CreateStageResponse' {} a -> s {apiGatewayManaged = a} :: CreateStageResponse)

-- | Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
createStageResponse_autoDeploy :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.Bool)
createStageResponse_autoDeploy = Lens.lens (\CreateStageResponse' {autoDeploy} -> autoDeploy) (\s@CreateStageResponse' {} a -> s {autoDeploy = a} :: CreateStageResponse)

-- | The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
createStageResponse_clientCertificateId :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.Text)
createStageResponse_clientCertificateId = Lens.lens (\CreateStageResponse' {clientCertificateId} -> clientCertificateId) (\s@CreateStageResponse' {} a -> s {clientCertificateId = a} :: CreateStageResponse)

-- | The timestamp when the stage was created.
createStageResponse_createdDate :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.UTCTime)
createStageResponse_createdDate = Lens.lens (\CreateStageResponse' {createdDate} -> createdDate) (\s@CreateStageResponse' {} a -> s {createdDate = a} :: CreateStageResponse) Prelude.. Lens.mapping Data._Time

-- | Default route settings for the stage.
createStageResponse_defaultRouteSettings :: Lens.Lens' CreateStageResponse (Prelude.Maybe RouteSettings)
createStageResponse_defaultRouteSettings = Lens.lens (\CreateStageResponse' {defaultRouteSettings} -> defaultRouteSettings) (\s@CreateStageResponse' {} a -> s {defaultRouteSettings = a} :: CreateStageResponse)

-- | The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
createStageResponse_deploymentId :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.Text)
createStageResponse_deploymentId = Lens.lens (\CreateStageResponse' {deploymentId} -> deploymentId) (\s@CreateStageResponse' {} a -> s {deploymentId = a} :: CreateStageResponse)

-- | The description of the stage.
createStageResponse_description :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.Text)
createStageResponse_description = Lens.lens (\CreateStageResponse' {description} -> description) (\s@CreateStageResponse' {} a -> s {description = a} :: CreateStageResponse)

-- | Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
createStageResponse_lastDeploymentStatusMessage :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.Text)
createStageResponse_lastDeploymentStatusMessage = Lens.lens (\CreateStageResponse' {lastDeploymentStatusMessage} -> lastDeploymentStatusMessage) (\s@CreateStageResponse' {} a -> s {lastDeploymentStatusMessage = a} :: CreateStageResponse)

-- | The timestamp when the stage was last updated.
createStageResponse_lastUpdatedDate :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.UTCTime)
createStageResponse_lastUpdatedDate = Lens.lens (\CreateStageResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreateStageResponse' {} a -> s {lastUpdatedDate = a} :: CreateStageResponse) Prelude.. Lens.mapping Data._Time

-- | Route settings for the stage, by routeKey.
createStageResponse_routeSettings :: Lens.Lens' CreateStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings))
createStageResponse_routeSettings = Lens.lens (\CreateStageResponse' {routeSettings} -> routeSettings) (\s@CreateStageResponse' {} a -> s {routeSettings = a} :: CreateStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stage.
createStageResponse_stageName :: Lens.Lens' CreateStageResponse (Prelude.Maybe Prelude.Text)
createStageResponse_stageName = Lens.lens (\CreateStageResponse' {stageName} -> stageName) (\s@CreateStageResponse' {} a -> s {stageName = a} :: CreateStageResponse)

-- | A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
createStageResponse_stageVariables :: Lens.Lens' CreateStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStageResponse_stageVariables = Lens.lens (\CreateStageResponse' {stageVariables} -> stageVariables) (\s@CreateStageResponse' {} a -> s {stageVariables = a} :: CreateStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The collection of tags. Each tag element is associated with a given
-- resource.
createStageResponse_tags :: Lens.Lens' CreateStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStageResponse_tags = Lens.lens (\CreateStageResponse' {tags} -> tags) (\s@CreateStageResponse' {} a -> s {tags = a} :: CreateStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createStageResponse_httpStatus :: Lens.Lens' CreateStageResponse Prelude.Int
createStageResponse_httpStatus = Lens.lens (\CreateStageResponse' {httpStatus} -> httpStatus) (\s@CreateStageResponse' {} a -> s {httpStatus = a} :: CreateStageResponse)

instance Prelude.NFData CreateStageResponse where
  rnf CreateStageResponse' {..} =
    Prelude.rnf accessLogSettings `Prelude.seq`
      Prelude.rnf apiGatewayManaged `Prelude.seq`
        Prelude.rnf autoDeploy `Prelude.seq`
          Prelude.rnf clientCertificateId `Prelude.seq`
            Prelude.rnf createdDate `Prelude.seq`
              Prelude.rnf defaultRouteSettings `Prelude.seq`
                Prelude.rnf deploymentId `Prelude.seq`
                  Prelude.rnf description `Prelude.seq`
                    Prelude.rnf lastDeploymentStatusMessage `Prelude.seq`
                      Prelude.rnf lastUpdatedDate `Prelude.seq`
                        Prelude.rnf routeSettings `Prelude.seq`
                          Prelude.rnf stageName `Prelude.seq`
                            Prelude.rnf stageVariables `Prelude.seq`
                              Prelude.rnf tags `Prelude.seq`
                                Prelude.rnf httpStatus
