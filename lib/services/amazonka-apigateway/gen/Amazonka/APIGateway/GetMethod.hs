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
-- Module      : Amazonka.APIGateway.GetMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing Method resource.
module Amazonka.APIGateway.GetMethod
  ( -- * Creating a Request
    GetMethod (..),
    newGetMethod,

    -- * Request Lenses
    getMethod_restApiId,
    getMethod_resourceId,
    getMethod_httpMethod,

    -- * Destructuring the Response
    Method (..),
    newMethod,

    -- * Response Lenses
    method_requestModels,
    method_requestParameters,
    method_methodResponses,
    method_apiKeyRequired,
    method_requestValidatorId,
    method_httpMethod,
    method_methodIntegration,
    method_authorizationScopes,
    method_authorizationType,
    method_operationName,
    method_authorizerId,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe an existing Method resource.
--
-- /See:/ 'newGetMethod' smart constructor.
data GetMethod = GetMethod'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The Resource identifier for the Method resource.
    resourceId :: Prelude.Text,
    -- | Specifies the method request\'s HTTP method type.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getMethod_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'getMethod_resourceId' - The Resource identifier for the Method resource.
--
-- 'httpMethod', 'getMethod_httpMethod' - Specifies the method request\'s HTTP method type.
newGetMethod ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  GetMethod
newGetMethod pRestApiId_ pResourceId_ pHttpMethod_ =
  GetMethod'
    { restApiId = pRestApiId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | The string identifier of the associated RestApi.
getMethod_restApiId :: Lens.Lens' GetMethod Prelude.Text
getMethod_restApiId = Lens.lens (\GetMethod' {restApiId} -> restApiId) (\s@GetMethod' {} a -> s {restApiId = a} :: GetMethod)

-- | The Resource identifier for the Method resource.
getMethod_resourceId :: Lens.Lens' GetMethod Prelude.Text
getMethod_resourceId = Lens.lens (\GetMethod' {resourceId} -> resourceId) (\s@GetMethod' {} a -> s {resourceId = a} :: GetMethod)

-- | Specifies the method request\'s HTTP method type.
getMethod_httpMethod :: Lens.Lens' GetMethod Prelude.Text
getMethod_httpMethod = Lens.lens (\GetMethod' {httpMethod} -> httpMethod) (\s@GetMethod' {} a -> s {httpMethod = a} :: GetMethod)

instance Core.AWSRequest GetMethod where
  type AWSResponse GetMethod = Method
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetMethod where
  hashWithSalt _salt GetMethod' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod

instance Prelude.NFData GetMethod where
  rnf GetMethod' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod

instance Data.ToHeaders GetMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetMethod where
  toPath GetMethod' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod
      ]

instance Data.ToQuery GetMethod where
  toQuery = Prelude.const Prelude.mempty
