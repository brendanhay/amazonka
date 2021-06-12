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
-- Module      : Network.AWS.APIGateway.GetIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the integration settings.
module Network.AWS.APIGateway.GetIntegration
  ( -- * Creating a Request
    GetIntegration (..),
    newGetIntegration,

    -- * Request Lenses
    getIntegration_restApiId,
    getIntegration_resourceId,
    getIntegration_httpMethod,

    -- * Destructuring the Response
    Integration (..),
    newIntegration,

    -- * Response Lenses
    integration_httpMethod,
    integration_passthroughBehavior,
    integration_contentHandling,
    integration_uri,
    integration_connectionType,
    integration_connectionId,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_cacheNamespace,
    integration_cacheKeyParameters,
    integration_tlsConfig,
    integration_integrationResponses,
    integration_requestParameters,
    integration_type,
    integration_credentials,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to get the integration configuration.
--
-- /See:/ 'newGetIntegration' smart constructor.
data GetIntegration = GetIntegration'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] Specifies a get integration request\'s resource identifier
    resourceId :: Core.Text,
    -- | [Required] Specifies a get integration request\'s HTTP method.
    httpMethod :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getIntegration_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'getIntegration_resourceId' - [Required] Specifies a get integration request\'s resource identifier
--
-- 'httpMethod', 'getIntegration_httpMethod' - [Required] Specifies a get integration request\'s HTTP method.
newGetIntegration ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  GetIntegration
newGetIntegration
  pRestApiId_
  pResourceId_
  pHttpMethod_ =
    GetIntegration'
      { restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_
      }

-- | [Required] The string identifier of the associated RestApi.
getIntegration_restApiId :: Lens.Lens' GetIntegration Core.Text
getIntegration_restApiId = Lens.lens (\GetIntegration' {restApiId} -> restApiId) (\s@GetIntegration' {} a -> s {restApiId = a} :: GetIntegration)

-- | [Required] Specifies a get integration request\'s resource identifier
getIntegration_resourceId :: Lens.Lens' GetIntegration Core.Text
getIntegration_resourceId = Lens.lens (\GetIntegration' {resourceId} -> resourceId) (\s@GetIntegration' {} a -> s {resourceId = a} :: GetIntegration)

-- | [Required] Specifies a get integration request\'s HTTP method.
getIntegration_httpMethod :: Lens.Lens' GetIntegration Core.Text
getIntegration_httpMethod = Lens.lens (\GetIntegration' {httpMethod} -> httpMethod) (\s@GetIntegration' {} a -> s {httpMethod = a} :: GetIntegration)

instance Core.AWSRequest GetIntegration where
  type AWSResponse GetIntegration = Integration
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetIntegration

instance Core.NFData GetIntegration

instance Core.ToHeaders GetIntegration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetIntegration where
  toPath GetIntegration' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/integration"
      ]

instance Core.ToQuery GetIntegration where
  toQuery = Core.const Core.mempty
