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
-- Module      : Amazonka.APIGateway.GetIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the integration settings.
module Amazonka.APIGateway.GetIntegration
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
    integration_requestTemplates,
    integration_credentials,
    integration_connectionId,
    integration_requestParameters,
    integration_contentHandling,
    integration_passthroughBehavior,
    integration_uri,
    integration_integrationResponses,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_timeoutInMillis,
    integration_type,
    integration_connectionType,
    integration_cacheKeyParameters,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to get the integration configuration.
--
-- /See:/ 'newGetIntegration' smart constructor.
data GetIntegration = GetIntegration'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Specifies a get integration request\'s resource identifier
    resourceId :: Prelude.Text,
    -- | [Required] Specifies a get integration request\'s HTTP method.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
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
getIntegration_restApiId :: Lens.Lens' GetIntegration Prelude.Text
getIntegration_restApiId = Lens.lens (\GetIntegration' {restApiId} -> restApiId) (\s@GetIntegration' {} a -> s {restApiId = a} :: GetIntegration)

-- | [Required] Specifies a get integration request\'s resource identifier
getIntegration_resourceId :: Lens.Lens' GetIntegration Prelude.Text
getIntegration_resourceId = Lens.lens (\GetIntegration' {resourceId} -> resourceId) (\s@GetIntegration' {} a -> s {resourceId = a} :: GetIntegration)

-- | [Required] Specifies a get integration request\'s HTTP method.
getIntegration_httpMethod :: Lens.Lens' GetIntegration Prelude.Text
getIntegration_httpMethod = Lens.lens (\GetIntegration' {httpMethod} -> httpMethod) (\s@GetIntegration' {} a -> s {httpMethod = a} :: GetIntegration)

instance Core.AWSRequest GetIntegration where
  type AWSResponse GetIntegration = Integration
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetIntegration

instance Prelude.NFData GetIntegration

instance Core.ToHeaders GetIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetIntegration where
  toPath GetIntegration' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/integration"
      ]

instance Core.ToQuery GetIntegration where
  toQuery = Prelude.const Prelude.mempty
