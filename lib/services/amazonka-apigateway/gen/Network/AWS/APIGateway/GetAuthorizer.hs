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
-- Module      : Network.AWS.APIGateway.GetAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing Authorizer resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizer.html AWS CLI>
module Network.AWS.APIGateway.GetAuthorizer
  ( -- * Creating a Request
    GetAuthorizer (..),
    newGetAuthorizer,

    -- * Request Lenses
    getAuthorizer_restApiId,
    getAuthorizer_authorizerId,

    -- * Destructuring the Response
    Authorizer (..),
    newAuthorizer,

    -- * Response Lenses
    authorizer_authorizerUri,
    authorizer_identityValidationExpression,
    authorizer_providerARNs,
    authorizer_name,
    authorizer_id,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authType,
    authorizer_type,
    authorizer_identitySource,
    authorizer_authorizerCredentials,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe an existing Authorizer resource.
--
-- /See:/ 'newGetAuthorizer' smart constructor.
data GetAuthorizer = GetAuthorizer'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The identifier of the Authorizer resource.
    authorizerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getAuthorizer_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'authorizerId', 'getAuthorizer_authorizerId' - [Required] The identifier of the Authorizer resource.
newGetAuthorizer ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'authorizerId'
  Prelude.Text ->
  GetAuthorizer
newGetAuthorizer pRestApiId_ pAuthorizerId_ =
  GetAuthorizer'
    { restApiId = pRestApiId_,
      authorizerId = pAuthorizerId_
    }

-- | [Required] The string identifier of the associated RestApi.
getAuthorizer_restApiId :: Lens.Lens' GetAuthorizer Prelude.Text
getAuthorizer_restApiId = Lens.lens (\GetAuthorizer' {restApiId} -> restApiId) (\s@GetAuthorizer' {} a -> s {restApiId = a} :: GetAuthorizer)

-- | [Required] The identifier of the Authorizer resource.
getAuthorizer_authorizerId :: Lens.Lens' GetAuthorizer Prelude.Text
getAuthorizer_authorizerId = Lens.lens (\GetAuthorizer' {authorizerId} -> authorizerId) (\s@GetAuthorizer' {} a -> s {authorizerId = a} :: GetAuthorizer)

instance Core.AWSRequest GetAuthorizer where
  type AWSResponse GetAuthorizer = Authorizer
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetAuthorizer

instance Prelude.NFData GetAuthorizer

instance Core.ToHeaders GetAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetAuthorizer where
  toPath GetAuthorizer' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/authorizers/",
        Core.toBS authorizerId
      ]

instance Core.ToQuery GetAuthorizer where
  toQuery = Prelude.const Prelude.mempty
