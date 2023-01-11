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
-- Module      : Amazonka.CognitoIdentityProvider.CreateResourceServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OAuth2.0 resource server and defines custom scopes within
-- it.
module Amazonka.CognitoIdentityProvider.CreateResourceServer
  ( -- * Creating a Request
    CreateResourceServer (..),
    newCreateResourceServer,

    -- * Request Lenses
    createResourceServer_scopes,
    createResourceServer_userPoolId,
    createResourceServer_identifier,
    createResourceServer_name,

    -- * Destructuring the Response
    CreateResourceServerResponse (..),
    newCreateResourceServerResponse,

    -- * Response Lenses
    createResourceServerResponse_httpStatus,
    createResourceServerResponse_resourceServer,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResourceServer' smart constructor.
data CreateResourceServer = CreateResourceServer'
  { -- | A list of scopes. Each scope is a key-value map with the keys @name@ and
    -- @description@.
    scopes :: Prelude.Maybe [ResourceServerScopeType],
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | A unique resource server identifier for the resource server. This could
    -- be an HTTPS endpoint where the resource server is located, such as
    -- @https:\/\/my-weather-api.example.com@.
    identifier :: Prelude.Text,
    -- | A friendly name for the resource server.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scopes', 'createResourceServer_scopes' - A list of scopes. Each scope is a key-value map with the keys @name@ and
-- @description@.
--
-- 'userPoolId', 'createResourceServer_userPoolId' - The user pool ID for the user pool.
--
-- 'identifier', 'createResourceServer_identifier' - A unique resource server identifier for the resource server. This could
-- be an HTTPS endpoint where the resource server is located, such as
-- @https:\/\/my-weather-api.example.com@.
--
-- 'name', 'createResourceServer_name' - A friendly name for the resource server.
newCreateResourceServer ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'identifier'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateResourceServer
newCreateResourceServer
  pUserPoolId_
  pIdentifier_
  pName_ =
    CreateResourceServer'
      { scopes = Prelude.Nothing,
        userPoolId = pUserPoolId_,
        identifier = pIdentifier_,
        name = pName_
      }

-- | A list of scopes. Each scope is a key-value map with the keys @name@ and
-- @description@.
createResourceServer_scopes :: Lens.Lens' CreateResourceServer (Prelude.Maybe [ResourceServerScopeType])
createResourceServer_scopes = Lens.lens (\CreateResourceServer' {scopes} -> scopes) (\s@CreateResourceServer' {} a -> s {scopes = a} :: CreateResourceServer) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool.
createResourceServer_userPoolId :: Lens.Lens' CreateResourceServer Prelude.Text
createResourceServer_userPoolId = Lens.lens (\CreateResourceServer' {userPoolId} -> userPoolId) (\s@CreateResourceServer' {} a -> s {userPoolId = a} :: CreateResourceServer)

-- | A unique resource server identifier for the resource server. This could
-- be an HTTPS endpoint where the resource server is located, such as
-- @https:\/\/my-weather-api.example.com@.
createResourceServer_identifier :: Lens.Lens' CreateResourceServer Prelude.Text
createResourceServer_identifier = Lens.lens (\CreateResourceServer' {identifier} -> identifier) (\s@CreateResourceServer' {} a -> s {identifier = a} :: CreateResourceServer)

-- | A friendly name for the resource server.
createResourceServer_name :: Lens.Lens' CreateResourceServer Prelude.Text
createResourceServer_name = Lens.lens (\CreateResourceServer' {name} -> name) (\s@CreateResourceServer' {} a -> s {name = a} :: CreateResourceServer)

instance Core.AWSRequest CreateResourceServer where
  type
    AWSResponse CreateResourceServer =
      CreateResourceServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ResourceServer")
      )

instance Prelude.Hashable CreateResourceServer where
  hashWithSalt _salt CreateResourceServer' {..} =
    _salt `Prelude.hashWithSalt` scopes
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateResourceServer where
  rnf CreateResourceServer' {..} =
    Prelude.rnf scopes
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateResourceServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.CreateResourceServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResourceServer where
  toJSON CreateResourceServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Scopes" Data..=) Prelude.<$> scopes,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Identifier" Data..= identifier),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateResourceServer where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateResourceServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceServerResponse' smart constructor.
data CreateResourceServerResponse = CreateResourceServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The newly created resource server.
    resourceServer :: ResourceServerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createResourceServerResponse_httpStatus' - The response's http status code.
--
-- 'resourceServer', 'createResourceServerResponse_resourceServer' - The newly created resource server.
newCreateResourceServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceServer'
  ResourceServerType ->
  CreateResourceServerResponse
newCreateResourceServerResponse
  pHttpStatus_
  pResourceServer_ =
    CreateResourceServerResponse'
      { httpStatus =
          pHttpStatus_,
        resourceServer = pResourceServer_
      }

-- | The response's http status code.
createResourceServerResponse_httpStatus :: Lens.Lens' CreateResourceServerResponse Prelude.Int
createResourceServerResponse_httpStatus = Lens.lens (\CreateResourceServerResponse' {httpStatus} -> httpStatus) (\s@CreateResourceServerResponse' {} a -> s {httpStatus = a} :: CreateResourceServerResponse)

-- | The newly created resource server.
createResourceServerResponse_resourceServer :: Lens.Lens' CreateResourceServerResponse ResourceServerType
createResourceServerResponse_resourceServer = Lens.lens (\CreateResourceServerResponse' {resourceServer} -> resourceServer) (\s@CreateResourceServerResponse' {} a -> s {resourceServer = a} :: CreateResourceServerResponse)

instance Prelude.NFData CreateResourceServerResponse where
  rnf CreateResourceServerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceServer
