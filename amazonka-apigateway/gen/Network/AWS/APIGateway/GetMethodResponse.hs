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
-- Module      : Network.AWS.APIGateway.GetMethodResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a MethodResponse resource.
module Network.AWS.APIGateway.GetMethodResponse
  ( -- * Creating a Request
    GetMethodResponse (..),
    newGetMethodResponse,

    -- * Request Lenses
    getMethodResponse_restApiId,
    getMethodResponse_resourceId,
    getMethodResponse_httpMethod,
    getMethodResponse_statusCode,

    -- * Destructuring the Response
    MethodResponse (..),
    newMethodResponse,

    -- * Response Lenses
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe a MethodResponse resource.
--
-- /See:/ 'newGetMethodResponse' smart constructor.
data GetMethodResponse = GetMethodResponse'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The Resource identifier for the MethodResponse resource.
    resourceId :: Prelude.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text,
    -- | [Required] The status code for the MethodResponse resource.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getMethodResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'getMethodResponse_resourceId' - [Required] The Resource identifier for the MethodResponse resource.
--
-- 'httpMethod', 'getMethodResponse_httpMethod' - [Required] The HTTP verb of the Method resource.
--
-- 'statusCode', 'getMethodResponse_statusCode' - [Required] The status code for the MethodResponse resource.
newGetMethodResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
  GetMethodResponse
newGetMethodResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    GetMethodResponse'
      { restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | [Required] The string identifier of the associated RestApi.
getMethodResponse_restApiId :: Lens.Lens' GetMethodResponse Prelude.Text
getMethodResponse_restApiId = Lens.lens (\GetMethodResponse' {restApiId} -> restApiId) (\s@GetMethodResponse' {} a -> s {restApiId = a} :: GetMethodResponse)

-- | [Required] The Resource identifier for the MethodResponse resource.
getMethodResponse_resourceId :: Lens.Lens' GetMethodResponse Prelude.Text
getMethodResponse_resourceId = Lens.lens (\GetMethodResponse' {resourceId} -> resourceId) (\s@GetMethodResponse' {} a -> s {resourceId = a} :: GetMethodResponse)

-- | [Required] The HTTP verb of the Method resource.
getMethodResponse_httpMethod :: Lens.Lens' GetMethodResponse Prelude.Text
getMethodResponse_httpMethod = Lens.lens (\GetMethodResponse' {httpMethod} -> httpMethod) (\s@GetMethodResponse' {} a -> s {httpMethod = a} :: GetMethodResponse)

-- | [Required] The status code for the MethodResponse resource.
getMethodResponse_statusCode :: Lens.Lens' GetMethodResponse Prelude.Text
getMethodResponse_statusCode = Lens.lens (\GetMethodResponse' {statusCode} -> statusCode) (\s@GetMethodResponse' {} a -> s {statusCode = a} :: GetMethodResponse)

instance Prelude.AWSRequest GetMethodResponse where
  type Rs GetMethodResponse = MethodResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable GetMethodResponse

instance Prelude.NFData GetMethodResponse

instance Prelude.ToHeaders GetMethodResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetMethodResponse where
  toPath GetMethodResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/resources/",
        Prelude.toBS resourceId,
        "/methods/",
        Prelude.toBS httpMethod,
        "/responses/",
        Prelude.toBS statusCode
      ]

instance Prelude.ToQuery GetMethodResponse where
  toQuery = Prelude.const Prelude.mempty
