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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateResourceServer' smart constructor.
data UpdateResourceServer = UpdateResourceServer'
  { -- | The scope values to be set for the resource server.
    scopes :: Prelude.Maybe [ResourceServerScopeType],
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | The identifier for the resource server.
    identifier :: Prelude.Text,
    -- | The name of the resource server.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'identifier'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateResourceServer
newUpdateResourceServer
  pUserPoolId_
  pIdentifier_
  pName_ =
    UpdateResourceServer'
      { scopes = Prelude.Nothing,
        userPoolId = pUserPoolId_,
        identifier = pIdentifier_,
        name = pName_
      }

-- | The scope values to be set for the resource server.
updateResourceServer_scopes :: Lens.Lens' UpdateResourceServer (Prelude.Maybe [ResourceServerScopeType])
updateResourceServer_scopes = Lens.lens (\UpdateResourceServer' {scopes} -> scopes) (\s@UpdateResourceServer' {} a -> s {scopes = a} :: UpdateResourceServer) Prelude.. Lens.mapping Prelude._Coerce

-- | The user pool ID for the user pool.
updateResourceServer_userPoolId :: Lens.Lens' UpdateResourceServer Prelude.Text
updateResourceServer_userPoolId = Lens.lens (\UpdateResourceServer' {userPoolId} -> userPoolId) (\s@UpdateResourceServer' {} a -> s {userPoolId = a} :: UpdateResourceServer)

-- | The identifier for the resource server.
updateResourceServer_identifier :: Lens.Lens' UpdateResourceServer Prelude.Text
updateResourceServer_identifier = Lens.lens (\UpdateResourceServer' {identifier} -> identifier) (\s@UpdateResourceServer' {} a -> s {identifier = a} :: UpdateResourceServer)

-- | The name of the resource server.
updateResourceServer_name :: Lens.Lens' UpdateResourceServer Prelude.Text
updateResourceServer_name = Lens.lens (\UpdateResourceServer' {name} -> name) (\s@UpdateResourceServer' {} a -> s {name = a} :: UpdateResourceServer)

instance Prelude.AWSRequest UpdateResourceServer where
  type
    Rs UpdateResourceServer =
      UpdateResourceServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourceServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ResourceServer")
      )

instance Prelude.Hashable UpdateResourceServer

instance Prelude.NFData UpdateResourceServer

instance Prelude.ToHeaders UpdateResourceServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.UpdateResourceServer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateResourceServer where
  toJSON UpdateResourceServer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Scopes" Prelude..=) Prelude.<$> scopes,
            Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Identifier" Prelude..= identifier),
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath UpdateResourceServer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateResourceServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceServerResponse' smart constructor.
data UpdateResourceServerResponse = UpdateResourceServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The resource server.
    resourceServer :: ResourceServerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
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
updateResourceServerResponse_httpStatus :: Lens.Lens' UpdateResourceServerResponse Prelude.Int
updateResourceServerResponse_httpStatus = Lens.lens (\UpdateResourceServerResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceServerResponse' {} a -> s {httpStatus = a} :: UpdateResourceServerResponse)

-- | The resource server.
updateResourceServerResponse_resourceServer :: Lens.Lens' UpdateResourceServerResponse ResourceServerType
updateResourceServerResponse_resourceServer = Lens.lens (\UpdateResourceServerResponse' {resourceServer} -> resourceServer) (\s@UpdateResourceServerResponse' {} a -> s {resourceServer = a} :: UpdateResourceServerResponse)

instance Prelude.NFData UpdateResourceServerResponse
