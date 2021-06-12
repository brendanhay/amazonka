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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteResourceServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource server.
module Network.AWS.CognitoIdentityProvider.DeleteResourceServer
  ( -- * Creating a Request
    DeleteResourceServer (..),
    newDeleteResourceServer,

    -- * Request Lenses
    deleteResourceServer_userPoolId,
    deleteResourceServer_identifier,

    -- * Destructuring the Response
    DeleteResourceServerResponse (..),
    newDeleteResourceServerResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResourceServer' smart constructor.
data DeleteResourceServer = DeleteResourceServer'
  { -- | The user pool ID for the user pool that hosts the resource server.
    userPoolId :: Core.Text,
    -- | The identifier for the resource server.
    identifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'deleteResourceServer_userPoolId' - The user pool ID for the user pool that hosts the resource server.
--
-- 'identifier', 'deleteResourceServer_identifier' - The identifier for the resource server.
newDeleteResourceServer ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'identifier'
  Core.Text ->
  DeleteResourceServer
newDeleteResourceServer pUserPoolId_ pIdentifier_ =
  DeleteResourceServer'
    { userPoolId = pUserPoolId_,
      identifier = pIdentifier_
    }

-- | The user pool ID for the user pool that hosts the resource server.
deleteResourceServer_userPoolId :: Lens.Lens' DeleteResourceServer Core.Text
deleteResourceServer_userPoolId = Lens.lens (\DeleteResourceServer' {userPoolId} -> userPoolId) (\s@DeleteResourceServer' {} a -> s {userPoolId = a} :: DeleteResourceServer)

-- | The identifier for the resource server.
deleteResourceServer_identifier :: Lens.Lens' DeleteResourceServer Core.Text
deleteResourceServer_identifier = Lens.lens (\DeleteResourceServer' {identifier} -> identifier) (\s@DeleteResourceServer' {} a -> s {identifier = a} :: DeleteResourceServer)

instance Core.AWSRequest DeleteResourceServer where
  type
    AWSResponse DeleteResourceServer =
      DeleteResourceServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteResourceServerResponse'

instance Core.Hashable DeleteResourceServer

instance Core.NFData DeleteResourceServer

instance Core.ToHeaders DeleteResourceServer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DeleteResourceServer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteResourceServer where
  toJSON DeleteResourceServer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Identifier" Core..= identifier)
          ]
      )

instance Core.ToPath DeleteResourceServer where
  toPath = Core.const "/"

instance Core.ToQuery DeleteResourceServer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteResourceServerResponse' smart constructor.
data DeleteResourceServerResponse = DeleteResourceServerResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourceServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourceServerResponse ::
  DeleteResourceServerResponse
newDeleteResourceServerResponse =
  DeleteResourceServerResponse'

instance Core.NFData DeleteResourceServerResponse
