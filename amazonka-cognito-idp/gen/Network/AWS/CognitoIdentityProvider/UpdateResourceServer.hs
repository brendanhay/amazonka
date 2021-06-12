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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateResourceServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and scopes of resource server. All other fields are
-- read-only.
--
-- If you don\'t provide a value for an attribute, it will be set to the
-- default value.
module Network.AWS.CognitoIdentityProvider.UpdateResourceServer
  ( -- * Creating a Request
    UpdateResourceServer (..),
    newUpdateResourceServer,

    -- * Request Lenses
    updateResourceServer_scopes,
    updateResourceServer_userPoolId,
    updateResourceServer_identifier,
    updateResourceServer_name,

    -- * Destructuring the Response
    UpdateResourceServerResponse (..),
    newUpdateResourceServerResponse,

    -- * Response Lenses
    updateResourceServerResponse_httpStatus,
    updateResourceServerResponse_resourceServer,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateResourceServer' smart constructor.
data UpdateResourceServer = UpdateResourceServer'
  { -- | The scope values to be set for the resource server.
    scopes :: Core.Maybe [ResourceServerScopeType],
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Text,
    -- | The identifier for the resource server.
    identifier :: Core.Text,
    -- | The name of the resource server.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateResourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scopes', 'updateResourceServer_scopes' - The scope values to be set for the resource server.
--
-- 'userPoolId', 'updateResourceServer_userPoolId' - The user pool ID for the user pool.
--
-- 'identifier', 'updateResourceServer_identifier' - The identifier for the resource server.
--
-- 'name', 'updateResourceServer_name' - The name of the resource server.
newUpdateResourceServer ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'identifier'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  UpdateResourceServer
newUpdateResourceServer
  pUserPoolId_
  pIdentifier_
  pName_ =
    UpdateResourceServer'
      { scopes = Core.Nothing,
        userPoolId = pUserPoolId_,
        identifier = pIdentifier_,
        name = pName_
      }

-- | The scope values to be set for the resource server.
updateResourceServer_scopes :: Lens.Lens' UpdateResourceServer (Core.Maybe [ResourceServerScopeType])
updateResourceServer_scopes = Lens.lens (\UpdateResourceServer' {scopes} -> scopes) (\s@UpdateResourceServer' {} a -> s {scopes = a} :: UpdateResourceServer) Core.. Lens.mapping Lens._Coerce

-- | The user pool ID for the user pool.
updateResourceServer_userPoolId :: Lens.Lens' UpdateResourceServer Core.Text
updateResourceServer_userPoolId = Lens.lens (\UpdateResourceServer' {userPoolId} -> userPoolId) (\s@UpdateResourceServer' {} a -> s {userPoolId = a} :: UpdateResourceServer)

-- | The identifier for the resource server.
updateResourceServer_identifier :: Lens.Lens' UpdateResourceServer Core.Text
updateResourceServer_identifier = Lens.lens (\UpdateResourceServer' {identifier} -> identifier) (\s@UpdateResourceServer' {} a -> s {identifier = a} :: UpdateResourceServer)

-- | The name of the resource server.
updateResourceServer_name :: Lens.Lens' UpdateResourceServer Core.Text
updateResourceServer_name = Lens.lens (\UpdateResourceServer' {name} -> name) (\s@UpdateResourceServer' {} a -> s {name = a} :: UpdateResourceServer)

instance Core.AWSRequest UpdateResourceServer where
  type
    AWSResponse UpdateResourceServer =
      UpdateResourceServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourceServerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ResourceServer")
      )

instance Core.Hashable UpdateResourceServer

instance Core.NFData UpdateResourceServer

instance Core.ToHeaders UpdateResourceServer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateResourceServer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateResourceServer where
  toJSON UpdateResourceServer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Scopes" Core..=) Core.<$> scopes,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Identifier" Core..= identifier),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateResourceServer where
  toPath = Core.const "/"

instance Core.ToQuery UpdateResourceServer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateResourceServerResponse' smart constructor.
data UpdateResourceServerResponse = UpdateResourceServerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The resource server.
    resourceServer :: ResourceServerType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateResourceServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourceServerResponse_httpStatus' - The response's http status code.
--
-- 'resourceServer', 'updateResourceServerResponse_resourceServer' - The resource server.
newUpdateResourceServerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'resourceServer'
  ResourceServerType ->
  UpdateResourceServerResponse
newUpdateResourceServerResponse
  pHttpStatus_
  pResourceServer_ =
    UpdateResourceServerResponse'
      { httpStatus =
          pHttpStatus_,
        resourceServer = pResourceServer_
      }

-- | The response's http status code.
updateResourceServerResponse_httpStatus :: Lens.Lens' UpdateResourceServerResponse Core.Int
updateResourceServerResponse_httpStatus = Lens.lens (\UpdateResourceServerResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceServerResponse' {} a -> s {httpStatus = a} :: UpdateResourceServerResponse)

-- | The resource server.
updateResourceServerResponse_resourceServer :: Lens.Lens' UpdateResourceServerResponse ResourceServerType
updateResourceServerResponse_resourceServer = Lens.lens (\UpdateResourceServerResponse' {resourceServer} -> resourceServer) (\s@UpdateResourceServerResponse' {} a -> s {resourceServer = a} :: UpdateResourceServerResponse)

instance Core.NFData UpdateResourceServerResponse
