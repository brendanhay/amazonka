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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the developer to delete the user pool client.
module Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
  ( -- * Creating a Request
    DeleteUserPoolClient (..),
    newDeleteUserPoolClient,

    -- * Request Lenses
    deleteUserPoolClient_userPoolId,
    deleteUserPoolClient_clientId,

    -- * Destructuring the Response
    DeleteUserPoolClientResponse (..),
    newDeleteUserPoolClientResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete a user pool client.
--
-- /See:/ 'newDeleteUserPoolClient' smart constructor.
data DeleteUserPoolClient = DeleteUserPoolClient'
  { -- | The user pool ID for the user pool where you want to delete the client.
    userPoolId :: Core.Text,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserPoolClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'deleteUserPoolClient_userPoolId' - The user pool ID for the user pool where you want to delete the client.
--
-- 'clientId', 'deleteUserPoolClient_clientId' - The app client ID of the app associated with the user pool.
newDeleteUserPoolClient ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'clientId'
  Core.Text ->
  DeleteUserPoolClient
newDeleteUserPoolClient pUserPoolId_ pClientId_ =
  DeleteUserPoolClient'
    { userPoolId = pUserPoolId_,
      clientId = Core._Sensitive Lens.# pClientId_
    }

-- | The user pool ID for the user pool where you want to delete the client.
deleteUserPoolClient_userPoolId :: Lens.Lens' DeleteUserPoolClient Core.Text
deleteUserPoolClient_userPoolId = Lens.lens (\DeleteUserPoolClient' {userPoolId} -> userPoolId) (\s@DeleteUserPoolClient' {} a -> s {userPoolId = a} :: DeleteUserPoolClient)

-- | The app client ID of the app associated with the user pool.
deleteUserPoolClient_clientId :: Lens.Lens' DeleteUserPoolClient Core.Text
deleteUserPoolClient_clientId = Lens.lens (\DeleteUserPoolClient' {clientId} -> clientId) (\s@DeleteUserPoolClient' {} a -> s {clientId = a} :: DeleteUserPoolClient) Core.. Core._Sensitive

instance Core.AWSRequest DeleteUserPoolClient where
  type
    AWSResponse DeleteUserPoolClient =
      DeleteUserPoolClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteUserPoolClientResponse'

instance Core.Hashable DeleteUserPoolClient

instance Core.NFData DeleteUserPoolClient

instance Core.ToHeaders DeleteUserPoolClient where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DeleteUserPoolClient" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteUserPoolClient where
  toJSON DeleteUserPoolClient' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ClientId" Core..= clientId)
          ]
      )

instance Core.ToPath DeleteUserPoolClient where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUserPoolClient where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUserPoolClientResponse' smart constructor.
data DeleteUserPoolClientResponse = DeleteUserPoolClientResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserPoolClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserPoolClientResponse ::
  DeleteUserPoolClientResponse
newDeleteUserPoolClientResponse =
  DeleteUserPoolClientResponse'

instance Core.NFData DeleteUserPoolClientResponse
