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
-- Module      : Network.AWS.APIGateway.GetDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Deployment resource.
module Network.AWS.APIGateway.GetDeployment
  ( -- * Creating a Request
    GetDeployment (..),
    newGetDeployment,

    -- * Request Lenses
    getDeployment_embed,
    getDeployment_restApiId,
    getDeployment_deploymentId,

    -- * Destructuring the Response
    Deployment (..),
    newDeployment,

    -- * Response Lenses
    deployment_createdDate,
    deployment_id,
    deployment_apiSummary,
    deployment_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about a Deployment resource.
--
-- /See:/ 'newGetDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { -- | A query parameter to retrieve the specified embedded resources of the
    -- returned Deployment resource in the response. In a REST API call, this
    -- @embed@ parameter value is a list of comma-separated strings, as in
    -- @GET \/restapis\/{restapi_id}\/deployments\/{deployment_id}?embed=var1,var2@.
    -- The SDK and other platform-dependent libraries might use a different
    -- format for the list. Currently, this request supports only retrieval of
    -- the embedded API summary this way. Hence, the parameter value must be a
    -- single-valued list containing only the @\"apisummary\"@ string. For
    -- example,
    -- @GET \/restapis\/{restapi_id}\/deployments\/{deployment_id}?embed=apisummary@.
    embed :: Core.Maybe [Core.Text],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The identifier of the Deployment resource to get information
    -- about.
    deploymentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'embed', 'getDeployment_embed' - A query parameter to retrieve the specified embedded resources of the
-- returned Deployment resource in the response. In a REST API call, this
-- @embed@ parameter value is a list of comma-separated strings, as in
-- @GET \/restapis\/{restapi_id}\/deployments\/{deployment_id}?embed=var1,var2@.
-- The SDK and other platform-dependent libraries might use a different
-- format for the list. Currently, this request supports only retrieval of
-- the embedded API summary this way. Hence, the parameter value must be a
-- single-valued list containing only the @\"apisummary\"@ string. For
-- example,
-- @GET \/restapis\/{restapi_id}\/deployments\/{deployment_id}?embed=apisummary@.
--
-- 'restApiId', 'getDeployment_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'deploymentId', 'getDeployment_deploymentId' - [Required] The identifier of the Deployment resource to get information
-- about.
newGetDeployment ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'deploymentId'
  Core.Text ->
  GetDeployment
newGetDeployment pRestApiId_ pDeploymentId_ =
  GetDeployment'
    { embed = Core.Nothing,
      restApiId = pRestApiId_,
      deploymentId = pDeploymentId_
    }

-- | A query parameter to retrieve the specified embedded resources of the
-- returned Deployment resource in the response. In a REST API call, this
-- @embed@ parameter value is a list of comma-separated strings, as in
-- @GET \/restapis\/{restapi_id}\/deployments\/{deployment_id}?embed=var1,var2@.
-- The SDK and other platform-dependent libraries might use a different
-- format for the list. Currently, this request supports only retrieval of
-- the embedded API summary this way. Hence, the parameter value must be a
-- single-valued list containing only the @\"apisummary\"@ string. For
-- example,
-- @GET \/restapis\/{restapi_id}\/deployments\/{deployment_id}?embed=apisummary@.
getDeployment_embed :: Lens.Lens' GetDeployment (Core.Maybe [Core.Text])
getDeployment_embed = Lens.lens (\GetDeployment' {embed} -> embed) (\s@GetDeployment' {} a -> s {embed = a} :: GetDeployment) Core.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
getDeployment_restApiId :: Lens.Lens' GetDeployment Core.Text
getDeployment_restApiId = Lens.lens (\GetDeployment' {restApiId} -> restApiId) (\s@GetDeployment' {} a -> s {restApiId = a} :: GetDeployment)

-- | [Required] The identifier of the Deployment resource to get information
-- about.
getDeployment_deploymentId :: Lens.Lens' GetDeployment Core.Text
getDeployment_deploymentId = Lens.lens (\GetDeployment' {deploymentId} -> deploymentId) (\s@GetDeployment' {} a -> s {deploymentId = a} :: GetDeployment)

instance Core.AWSRequest GetDeployment where
  type AWSResponse GetDeployment = Deployment
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetDeployment

instance Core.NFData GetDeployment

instance Core.ToHeaders GetDeployment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetDeployment where
  toPath GetDeployment' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/deployments/",
        Core.toBS deploymentId
      ]

instance Core.ToQuery GetDeployment where
  toQuery GetDeployment' {..} =
    Core.mconcat
      [ "embed"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> embed)
      ]
