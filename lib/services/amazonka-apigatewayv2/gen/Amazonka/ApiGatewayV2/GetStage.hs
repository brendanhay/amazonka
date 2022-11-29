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
-- Module      : Amazonka.ApiGatewayV2.GetStage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Stage.
module Amazonka.ApiGatewayV2.GetStage
  ( -- * Creating a Request
    GetStage (..),
    newGetStage,

    -- * Request Lenses
    getStage_stageName,
    getStage_apiId,

    -- * Destructuring the Response
    GetStageResponse (..),
    newGetStageResponse,

    -- * Response Lenses
    getStageResponse_tags,
    getStageResponse_stageName,
    getStageResponse_accessLogSettings,
    getStageResponse_deploymentId,
    getStageResponse_autoDeploy,
    getStageResponse_lastUpdatedDate,
    getStageResponse_stageVariables,
    getStageResponse_description,
    getStageResponse_clientCertificateId,
    getStageResponse_lastDeploymentStatusMessage,
    getStageResponse_defaultRouteSettings,
    getStageResponse_createdDate,
    getStageResponse_apiGatewayManaged,
    getStageResponse_routeSettings,
    getStageResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStage' smart constructor.
data GetStage = GetStage'
  { -- | The stage name. Stage names can only contain alphanumeric characters,
    -- hyphens, and underscores. Maximum length is 128 characters.
    stageName :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'getStage_stageName' - The stage name. Stage names can only contain alphanumeric characters,
-- hyphens, and underscores. Maximum length is 128 characters.
--
-- 'apiId', 'getStage_apiId' - The API identifier.
newGetStage ::
  -- | 'stageName'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  GetStage
newGetStage pStageName_ pApiId_ =
  GetStage' {stageName = pStageName_, apiId = pApiId_}

-- | The stage name. Stage names can only contain alphanumeric characters,
-- hyphens, and underscores. Maximum length is 128 characters.
getStage_stageName :: Lens.Lens' GetStage Prelude.Text
getStage_stageName = Lens.lens (\GetStage' {stageName} -> stageName) (\s@GetStage' {} a -> s {stageName = a} :: GetStage)

-- | The API identifier.
getStage_apiId :: Lens.Lens' GetStage Prelude.Text
getStage_apiId = Lens.lens (\GetStage' {apiId} -> apiId) (\s@GetStage' {} a -> s {apiId = a} :: GetStage)

instance Core.AWSRequest GetStage where
  type AWSResponse GetStage = GetStageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStageResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "stageName")
            Prelude.<*> (x Core..?> "accessLogSettings")
            Prelude.<*> (x Core..?> "deploymentId")
            Prelude.<*> (x Core..?> "autoDeploy")
            Prelude.<*> (x Core..?> "lastUpdatedDate")
            Prelude.<*> (x Core..?> "stageVariables" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "clientCertificateId")
            Prelude.<*> (x Core..?> "lastDeploymentStatusMessage")
            Prelude.<*> (x Core..?> "defaultRouteSettings")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "apiGatewayManaged")
            Prelude.<*> (x Core..?> "routeSettings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStage where
  hashWithSalt _salt GetStage' {..} =
    _salt `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetStage where
  rnf GetStage' {..} =
    Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf apiId

instance Core.ToHeaders GetStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetStage where
  toPath GetStage' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/stages/",
        Core.toBS stageName
      ]

instance Core.ToQuery GetStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStageResponse' smart constructor.
data GetStageResponse = GetStageResponse'
  { -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | Settings for logging access in this stage.
    accessLogSettings :: Prelude.Maybe AccessLogSettings,
    -- | The identifier of the Deployment that the Stage is associated with.
    -- Can\'t be updated if autoDeploy is enabled.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether updates to an API automatically trigger a new
    -- deployment. The default value is false.
    autoDeploy :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the stage was last updated.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | A map that defines the stage variables for a stage resource. Variable
    -- names can have alphanumeric and underscore characters, and the values
    -- must match [A-Za-z0-9-._~:\/?#&=,]+.
    stageVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a client certificate for a Stage. Supported only for
    -- WebSocket APIs.
    clientCertificateId :: Prelude.Maybe Prelude.Text,
    -- | Describes the status of the last deployment of a stage. Supported only
    -- for stages with autoDeploy enabled.
    lastDeploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | Default route settings for the stage.
    defaultRouteSettings :: Prelude.Maybe RouteSettings,
    -- | The timestamp when the stage was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether a stage is managed by API Gateway. If you created an
    -- API using quick create, the $default stage is managed by API Gateway.
    -- You can\'t modify the $default stage.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | Route settings for the stage, by routeKey.
    routeSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getStageResponse_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'stageName', 'getStageResponse_stageName' - The name of the stage.
--
-- 'accessLogSettings', 'getStageResponse_accessLogSettings' - Settings for logging access in this stage.
--
-- 'deploymentId', 'getStageResponse_deploymentId' - The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
--
-- 'autoDeploy', 'getStageResponse_autoDeploy' - Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
--
-- 'lastUpdatedDate', 'getStageResponse_lastUpdatedDate' - The timestamp when the stage was last updated.
--
-- 'stageVariables', 'getStageResponse_stageVariables' - A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
--
-- 'description', 'getStageResponse_description' - The description of the stage.
--
-- 'clientCertificateId', 'getStageResponse_clientCertificateId' - The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
--
-- 'lastDeploymentStatusMessage', 'getStageResponse_lastDeploymentStatusMessage' - Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
--
-- 'defaultRouteSettings', 'getStageResponse_defaultRouteSettings' - Default route settings for the stage.
--
-- 'createdDate', 'getStageResponse_createdDate' - The timestamp when the stage was created.
--
-- 'apiGatewayManaged', 'getStageResponse_apiGatewayManaged' - Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
--
-- 'routeSettings', 'getStageResponse_routeSettings' - Route settings for the stage, by routeKey.
--
-- 'httpStatus', 'getStageResponse_httpStatus' - The response's http status code.
newGetStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStageResponse
newGetStageResponse pHttpStatus_ =
  GetStageResponse'
    { tags = Prelude.Nothing,
      stageName = Prelude.Nothing,
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
      routeSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The collection of tags. Each tag element is associated with a given
-- resource.
getStageResponse_tags :: Lens.Lens' GetStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getStageResponse_tags = Lens.lens (\GetStageResponse' {tags} -> tags) (\s@GetStageResponse' {} a -> s {tags = a} :: GetStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stage.
getStageResponse_stageName :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_stageName = Lens.lens (\GetStageResponse' {stageName} -> stageName) (\s@GetStageResponse' {} a -> s {stageName = a} :: GetStageResponse)

-- | Settings for logging access in this stage.
getStageResponse_accessLogSettings :: Lens.Lens' GetStageResponse (Prelude.Maybe AccessLogSettings)
getStageResponse_accessLogSettings = Lens.lens (\GetStageResponse' {accessLogSettings} -> accessLogSettings) (\s@GetStageResponse' {} a -> s {accessLogSettings = a} :: GetStageResponse)

-- | The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
getStageResponse_deploymentId :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_deploymentId = Lens.lens (\GetStageResponse' {deploymentId} -> deploymentId) (\s@GetStageResponse' {} a -> s {deploymentId = a} :: GetStageResponse)

-- | Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
getStageResponse_autoDeploy :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Bool)
getStageResponse_autoDeploy = Lens.lens (\GetStageResponse' {autoDeploy} -> autoDeploy) (\s@GetStageResponse' {} a -> s {autoDeploy = a} :: GetStageResponse)

-- | The timestamp when the stage was last updated.
getStageResponse_lastUpdatedDate :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.UTCTime)
getStageResponse_lastUpdatedDate = Lens.lens (\GetStageResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetStageResponse' {} a -> s {lastUpdatedDate = a} :: GetStageResponse) Prelude.. Lens.mapping Core._Time

-- | A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
getStageResponse_stageVariables :: Lens.Lens' GetStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getStageResponse_stageVariables = Lens.lens (\GetStageResponse' {stageVariables} -> stageVariables) (\s@GetStageResponse' {} a -> s {stageVariables = a} :: GetStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The description of the stage.
getStageResponse_description :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_description = Lens.lens (\GetStageResponse' {description} -> description) (\s@GetStageResponse' {} a -> s {description = a} :: GetStageResponse)

-- | The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
getStageResponse_clientCertificateId :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_clientCertificateId = Lens.lens (\GetStageResponse' {clientCertificateId} -> clientCertificateId) (\s@GetStageResponse' {} a -> s {clientCertificateId = a} :: GetStageResponse)

-- | Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
getStageResponse_lastDeploymentStatusMessage :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_lastDeploymentStatusMessage = Lens.lens (\GetStageResponse' {lastDeploymentStatusMessage} -> lastDeploymentStatusMessage) (\s@GetStageResponse' {} a -> s {lastDeploymentStatusMessage = a} :: GetStageResponse)

-- | Default route settings for the stage.
getStageResponse_defaultRouteSettings :: Lens.Lens' GetStageResponse (Prelude.Maybe RouteSettings)
getStageResponse_defaultRouteSettings = Lens.lens (\GetStageResponse' {defaultRouteSettings} -> defaultRouteSettings) (\s@GetStageResponse' {} a -> s {defaultRouteSettings = a} :: GetStageResponse)

-- | The timestamp when the stage was created.
getStageResponse_createdDate :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.UTCTime)
getStageResponse_createdDate = Lens.lens (\GetStageResponse' {createdDate} -> createdDate) (\s@GetStageResponse' {} a -> s {createdDate = a} :: GetStageResponse) Prelude.. Lens.mapping Core._Time

-- | Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
getStageResponse_apiGatewayManaged :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Bool)
getStageResponse_apiGatewayManaged = Lens.lens (\GetStageResponse' {apiGatewayManaged} -> apiGatewayManaged) (\s@GetStageResponse' {} a -> s {apiGatewayManaged = a} :: GetStageResponse)

-- | Route settings for the stage, by routeKey.
getStageResponse_routeSettings :: Lens.Lens' GetStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings))
getStageResponse_routeSettings = Lens.lens (\GetStageResponse' {routeSettings} -> routeSettings) (\s@GetStageResponse' {} a -> s {routeSettings = a} :: GetStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getStageResponse_httpStatus :: Lens.Lens' GetStageResponse Prelude.Int
getStageResponse_httpStatus = Lens.lens (\GetStageResponse' {httpStatus} -> httpStatus) (\s@GetStageResponse' {} a -> s {httpStatus = a} :: GetStageResponse)

instance Prelude.NFData GetStageResponse where
  rnf GetStageResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf stageName
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
      `Prelude.seq` Prelude.rnf httpStatus
