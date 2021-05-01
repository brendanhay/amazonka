{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.GetMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing Method resource.
module Network.AWS.APIGateway.GetMethod
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
    method_httpMethod,
    method_methodIntegration,
    method_apiKeyRequired,
    method_authorizationType,
    method_requestModels,
    method_operationName,
    method_requestValidatorId,
    method_methodResponses,
    method_authorizerId,
    method_requestParameters,
    method_authorizationScopes,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe an existing Method resource.
--
-- /See:/ 'newGetMethod' smart constructor.
data GetMethod = GetMethod'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The Resource identifier for the Method resource.
    resourceId :: Prelude.Text,
    -- | [Required] Specifies the method request\'s HTTP method type.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getMethod_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'getMethod_resourceId' - [Required] The Resource identifier for the Method resource.
--
-- 'httpMethod', 'getMethod_httpMethod' - [Required] Specifies the method request\'s HTTP method type.
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

-- | [Required] The string identifier of the associated RestApi.
getMethod_restApiId :: Lens.Lens' GetMethod Prelude.Text
getMethod_restApiId = Lens.lens (\GetMethod' {restApiId} -> restApiId) (\s@GetMethod' {} a -> s {restApiId = a} :: GetMethod)

-- | [Required] The Resource identifier for the Method resource.
getMethod_resourceId :: Lens.Lens' GetMethod Prelude.Text
getMethod_resourceId = Lens.lens (\GetMethod' {resourceId} -> resourceId) (\s@GetMethod' {} a -> s {resourceId = a} :: GetMethod)

-- | [Required] Specifies the method request\'s HTTP method type.
getMethod_httpMethod :: Lens.Lens' GetMethod Prelude.Text
getMethod_httpMethod = Lens.lens (\GetMethod' {httpMethod} -> httpMethod) (\s@GetMethod' {} a -> s {httpMethod = a} :: GetMethod)

instance Prelude.AWSRequest GetMethod where
  type Rs GetMethod = Method
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable GetMethod

instance Prelude.NFData GetMethod

instance Prelude.ToHeaders GetMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetMethod where
  toPath GetMethod' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/resources/",
        Prelude.toBS resourceId,
        "/methods/",
        Prelude.toBS httpMethod
      ]

instance Prelude.ToQuery GetMethod where
  toQuery = Prelude.const Prelude.mempty
