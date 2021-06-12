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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateResourceServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OAuth2.0 resource server and defines custom scopes in it.
module Network.AWS.CognitoIdentityProvider.CreateResourceServer
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateResourceServer' smart constructor.
data CreateResourceServer = CreateResourceServer'
  { -- | A list of scopes. Each scope is map, where the keys are @name@ and
    -- @description@.
    scopes :: Core.Maybe [ResourceServerScopeType],
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Text,
    -- | A unique resource server identifier for the resource server. This could
    -- be an HTTPS endpoint where the resource server is located. For example,
    -- @https:\/\/my-weather-api.example.com@.
    identifier :: Core.Text,
    -- | A friendly name for the resource server.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scopes', 'createResourceServer_scopes' - A list of scopes. Each scope is map, where the keys are @name@ and
-- @description@.
--
-- 'userPoolId', 'createResourceServer_userPoolId' - The user pool ID for the user pool.
--
-- 'identifier', 'createResourceServer_identifier' - A unique resource server identifier for the resource server. This could
-- be an HTTPS endpoint where the resource server is located. For example,
-- @https:\/\/my-weather-api.example.com@.
--
-- 'name', 'createResourceServer_name' - A friendly name for the resource server.
newCreateResourceServer ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'identifier'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CreateResourceServer
newCreateResourceServer
  pUserPoolId_
  pIdentifier_
  pName_ =
    CreateResourceServer'
      { scopes = Core.Nothing,
        userPoolId = pUserPoolId_,
        identifier = pIdentifier_,
        name = pName_
      }

-- | A list of scopes. Each scope is map, where the keys are @name@ and
-- @description@.
createResourceServer_scopes :: Lens.Lens' CreateResourceServer (Core.Maybe [ResourceServerScopeType])
createResourceServer_scopes = Lens.lens (\CreateResourceServer' {scopes} -> scopes) (\s@CreateResourceServer' {} a -> s {scopes = a} :: CreateResourceServer) Core.. Lens.mapping Lens._Coerce

-- | The user pool ID for the user pool.
createResourceServer_userPoolId :: Lens.Lens' CreateResourceServer Core.Text
createResourceServer_userPoolId = Lens.lens (\CreateResourceServer' {userPoolId} -> userPoolId) (\s@CreateResourceServer' {} a -> s {userPoolId = a} :: CreateResourceServer)

-- | A unique resource server identifier for the resource server. This could
-- be an HTTPS endpoint where the resource server is located. For example,
-- @https:\/\/my-weather-api.example.com@.
createResourceServer_identifier :: Lens.Lens' CreateResourceServer Core.Text
createResourceServer_identifier = Lens.lens (\CreateResourceServer' {identifier} -> identifier) (\s@CreateResourceServer' {} a -> s {identifier = a} :: CreateResourceServer)

-- | A friendly name for the resource server.
createResourceServer_name :: Lens.Lens' CreateResourceServer Core.Text
createResourceServer_name = Lens.lens (\CreateResourceServer' {name} -> name) (\s@CreateResourceServer' {} a -> s {name = a} :: CreateResourceServer)

instance Core.AWSRequest CreateResourceServer where
  type
    AWSResponse CreateResourceServer =
      CreateResourceServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceServerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ResourceServer")
      )

instance Core.Hashable CreateResourceServer

instance Core.NFData CreateResourceServer

instance Core.ToHeaders CreateResourceServer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateResourceServer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateResourceServer where
  toJSON CreateResourceServer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Scopes" Core..=) Core.<$> scopes,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Identifier" Core..= identifier),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateResourceServer where
  toPath = Core.const "/"

instance Core.ToQuery CreateResourceServer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateResourceServerResponse' smart constructor.
data CreateResourceServerResponse = CreateResourceServerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The newly created resource server.
    resourceServer :: ResourceServerType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
createResourceServerResponse_httpStatus :: Lens.Lens' CreateResourceServerResponse Core.Int
createResourceServerResponse_httpStatus = Lens.lens (\CreateResourceServerResponse' {httpStatus} -> httpStatus) (\s@CreateResourceServerResponse' {} a -> s {httpStatus = a} :: CreateResourceServerResponse)

-- | The newly created resource server.
createResourceServerResponse_resourceServer :: Lens.Lens' CreateResourceServerResponse ResourceServerType
createResourceServerResponse_resourceServer = Lens.lens (\CreateResourceServerResponse' {resourceServer} -> resourceServer) (\s@CreateResourceServerResponse' {} a -> s {resourceServer = a} :: CreateResourceServerResponse)

instance Core.NFData CreateResourceServerResponse
