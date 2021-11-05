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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getStageResponse_lastDeploymentStatusMessage,
    getStageResponse_deploymentId,
    getStageResponse_routeSettings,
    getStageResponse_accessLogSettings,
    getStageResponse_clientCertificateId,
    getStageResponse_stageVariables,
    getStageResponse_autoDeploy,
    getStageResponse_createdDate,
    getStageResponse_defaultRouteSettings,
    getStageResponse_apiGatewayManaged,
    getStageResponse_stageName,
    getStageResponse_lastUpdatedDate,
    getStageResponse_description,
    getStageResponse_tags,
    getStageResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStageResponse'
            Prelude.<$> (x Core..?> "lastDeploymentStatusMessage")
            Prelude.<*> (x Core..?> "deploymentId")
            Prelude.<*> (x Core..?> "routeSettings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "accessLogSettings")
            Prelude.<*> (x Core..?> "clientCertificateId")
            Prelude.<*> (x Core..?> "stageVariables" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "autoDeploy")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "defaultRouteSettings")
            Prelude.<*> (x Core..?> "apiGatewayManaged")
            Prelude.<*> (x Core..?> "stageName")
            Prelude.<*> (x Core..?> "lastUpdatedDate")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStage

instance Prelude.NFData GetStage

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
    -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the stage was last updated.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'lastDeploymentStatusMessage', 'getStageResponse_lastDeploymentStatusMessage' - Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
--
-- 'deploymentId', 'getStageResponse_deploymentId' - The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
--
-- 'routeSettings', 'getStageResponse_routeSettings' - Route settings for the stage, by routeKey.
--
-- 'accessLogSettings', 'getStageResponse_accessLogSettings' - Settings for logging access in this stage.
--
-- 'clientCertificateId', 'getStageResponse_clientCertificateId' - The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
--
-- 'stageVariables', 'getStageResponse_stageVariables' - A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
--
-- 'autoDeploy', 'getStageResponse_autoDeploy' - Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
--
-- 'createdDate', 'getStageResponse_createdDate' - The timestamp when the stage was created.
--
-- 'defaultRouteSettings', 'getStageResponse_defaultRouteSettings' - Default route settings for the stage.
--
-- 'apiGatewayManaged', 'getStageResponse_apiGatewayManaged' - Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
--
-- 'stageName', 'getStageResponse_stageName' - The name of the stage.
--
-- 'lastUpdatedDate', 'getStageResponse_lastUpdatedDate' - The timestamp when the stage was last updated.
--
-- 'description', 'getStageResponse_description' - The description of the stage.
--
-- 'tags', 'getStageResponse_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'httpStatus', 'getStageResponse_httpStatus' - The response's http status code.
newGetStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStageResponse
newGetStageResponse pHttpStatus_ =
  GetStageResponse'
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
      stageName = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the status of the last deployment of a stage. Supported only
-- for stages with autoDeploy enabled.
getStageResponse_lastDeploymentStatusMessage :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_lastDeploymentStatusMessage = Lens.lens (\GetStageResponse' {lastDeploymentStatusMessage} -> lastDeploymentStatusMessage) (\s@GetStageResponse' {} a -> s {lastDeploymentStatusMessage = a} :: GetStageResponse)

-- | The identifier of the Deployment that the Stage is associated with.
-- Can\'t be updated if autoDeploy is enabled.
getStageResponse_deploymentId :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_deploymentId = Lens.lens (\GetStageResponse' {deploymentId} -> deploymentId) (\s@GetStageResponse' {} a -> s {deploymentId = a} :: GetStageResponse)

-- | Route settings for the stage, by routeKey.
getStageResponse_routeSettings :: Lens.Lens' GetStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text RouteSettings))
getStageResponse_routeSettings = Lens.lens (\GetStageResponse' {routeSettings} -> routeSettings) (\s@GetStageResponse' {} a -> s {routeSettings = a} :: GetStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | Settings for logging access in this stage.
getStageResponse_accessLogSettings :: Lens.Lens' GetStageResponse (Prelude.Maybe AccessLogSettings)
getStageResponse_accessLogSettings = Lens.lens (\GetStageResponse' {accessLogSettings} -> accessLogSettings) (\s@GetStageResponse' {} a -> s {accessLogSettings = a} :: GetStageResponse)

-- | The identifier of a client certificate for a Stage. Supported only for
-- WebSocket APIs.
getStageResponse_clientCertificateId :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_clientCertificateId = Lens.lens (\GetStageResponse' {clientCertificateId} -> clientCertificateId) (\s@GetStageResponse' {} a -> s {clientCertificateId = a} :: GetStageResponse)

-- | A map that defines the stage variables for a stage resource. Variable
-- names can have alphanumeric and underscore characters, and the values
-- must match [A-Za-z0-9-._~:\/?#&=,]+.
getStageResponse_stageVariables :: Lens.Lens' GetStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getStageResponse_stageVariables = Lens.lens (\GetStageResponse' {stageVariables} -> stageVariables) (\s@GetStageResponse' {} a -> s {stageVariables = a} :: GetStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether updates to an API automatically trigger a new
-- deployment. The default value is false.
getStageResponse_autoDeploy :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Bool)
getStageResponse_autoDeploy = Lens.lens (\GetStageResponse' {autoDeploy} -> autoDeploy) (\s@GetStageResponse' {} a -> s {autoDeploy = a} :: GetStageResponse)

-- | The timestamp when the stage was created.
getStageResponse_createdDate :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.UTCTime)
getStageResponse_createdDate = Lens.lens (\GetStageResponse' {createdDate} -> createdDate) (\s@GetStageResponse' {} a -> s {createdDate = a} :: GetStageResponse) Prelude.. Lens.mapping Core._Time

-- | Default route settings for the stage.
getStageResponse_defaultRouteSettings :: Lens.Lens' GetStageResponse (Prelude.Maybe RouteSettings)
getStageResponse_defaultRouteSettings = Lens.lens (\GetStageResponse' {defaultRouteSettings} -> defaultRouteSettings) (\s@GetStageResponse' {} a -> s {defaultRouteSettings = a} :: GetStageResponse)

-- | Specifies whether a stage is managed by API Gateway. If you created an
-- API using quick create, the $default stage is managed by API Gateway.
-- You can\'t modify the $default stage.
getStageResponse_apiGatewayManaged :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Bool)
getStageResponse_apiGatewayManaged = Lens.lens (\GetStageResponse' {apiGatewayManaged} -> apiGatewayManaged) (\s@GetStageResponse' {} a -> s {apiGatewayManaged = a} :: GetStageResponse)

-- | The name of the stage.
getStageResponse_stageName :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_stageName = Lens.lens (\GetStageResponse' {stageName} -> stageName) (\s@GetStageResponse' {} a -> s {stageName = a} :: GetStageResponse)

-- | The timestamp when the stage was last updated.
getStageResponse_lastUpdatedDate :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.UTCTime)
getStageResponse_lastUpdatedDate = Lens.lens (\GetStageResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetStageResponse' {} a -> s {lastUpdatedDate = a} :: GetStageResponse) Prelude.. Lens.mapping Core._Time

-- | The description of the stage.
getStageResponse_description :: Lens.Lens' GetStageResponse (Prelude.Maybe Prelude.Text)
getStageResponse_description = Lens.lens (\GetStageResponse' {description} -> description) (\s@GetStageResponse' {} a -> s {description = a} :: GetStageResponse)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
getStageResponse_tags :: Lens.Lens' GetStageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getStageResponse_tags = Lens.lens (\GetStageResponse' {tags} -> tags) (\s@GetStageResponse' {} a -> s {tags = a} :: GetStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getStageResponse_httpStatus :: Lens.Lens' GetStageResponse Prelude.Int
getStageResponse_httpStatus = Lens.lens (\GetStageResponse' {httpStatus} -> httpStatus) (\s@GetStageResponse' {} a -> s {httpStatus = a} :: GetStageResponse)

instance Prelude.NFData GetStageResponse
