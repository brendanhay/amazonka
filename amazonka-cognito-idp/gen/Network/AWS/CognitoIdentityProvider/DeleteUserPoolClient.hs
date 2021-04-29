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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete a user pool client.
--
-- /See:/ 'newDeleteUserPoolClient' smart constructor.
data DeleteUserPoolClient = DeleteUserPoolClient'
  { -- | The user pool ID for the user pool where you want to delete the client.
    userPoolId :: Prelude.Text,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  DeleteUserPoolClient
newDeleteUserPoolClient pUserPoolId_ pClientId_ =
  DeleteUserPoolClient'
    { userPoolId = pUserPoolId_,
      clientId = Prelude._Sensitive Lens.# pClientId_
    }

-- | The user pool ID for the user pool where you want to delete the client.
deleteUserPoolClient_userPoolId :: Lens.Lens' DeleteUserPoolClient Prelude.Text
deleteUserPoolClient_userPoolId = Lens.lens (\DeleteUserPoolClient' {userPoolId} -> userPoolId) (\s@DeleteUserPoolClient' {} a -> s {userPoolId = a} :: DeleteUserPoolClient)

-- | The app client ID of the app associated with the user pool.
deleteUserPoolClient_clientId :: Lens.Lens' DeleteUserPoolClient Prelude.Text
deleteUserPoolClient_clientId = Lens.lens (\DeleteUserPoolClient' {clientId} -> clientId) (\s@DeleteUserPoolClient' {} a -> s {clientId = a} :: DeleteUserPoolClient) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest DeleteUserPoolClient where
  type
    Rs DeleteUserPoolClient =
      DeleteUserPoolClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteUserPoolClientResponse'

instance Prelude.Hashable DeleteUserPoolClient

instance Prelude.NFData DeleteUserPoolClient

instance Prelude.ToHeaders DeleteUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.DeleteUserPoolClient" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteUserPoolClient where
  toJSON DeleteUserPoolClient' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("ClientId" Prelude..= clientId)
          ]
      )

instance Prelude.ToPath DeleteUserPoolClient where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteUserPoolClient where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserPoolClientResponse' smart constructor.
data DeleteUserPoolClientResponse = DeleteUserPoolClientResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPoolClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserPoolClientResponse ::
  DeleteUserPoolClientResponse
newDeleteUserPoolClientResponse =
  DeleteUserPoolClientResponse'

instance Prelude.NFData DeleteUserPoolClientResponse
