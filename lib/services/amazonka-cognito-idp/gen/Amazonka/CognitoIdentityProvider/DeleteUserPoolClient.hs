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
-- Module      : Amazonka.CognitoIdentityProvider.DeleteUserPoolClient
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the developer to delete the user pool client.
module Amazonka.CognitoIdentityProvider.DeleteUserPoolClient
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to delete a user pool client.
--
-- /See:/ 'newDeleteUserPoolClient' smart constructor.
data DeleteUserPoolClient = DeleteUserPoolClient'
  { -- | The user pool ID for the user pool where you want to delete the client.
    userPoolId :: Prelude.Text,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
      clientId = Data._Sensitive Lens.# pClientId_
    }

-- | The user pool ID for the user pool where you want to delete the client.
deleteUserPoolClient_userPoolId :: Lens.Lens' DeleteUserPoolClient Prelude.Text
deleteUserPoolClient_userPoolId = Lens.lens (\DeleteUserPoolClient' {userPoolId} -> userPoolId) (\s@DeleteUserPoolClient' {} a -> s {userPoolId = a} :: DeleteUserPoolClient)

-- | The app client ID of the app associated with the user pool.
deleteUserPoolClient_clientId :: Lens.Lens' DeleteUserPoolClient Prelude.Text
deleteUserPoolClient_clientId = Lens.lens (\DeleteUserPoolClient' {clientId} -> clientId) (\s@DeleteUserPoolClient' {} a -> s {clientId = a} :: DeleteUserPoolClient) Prelude.. Data._Sensitive

instance Core.AWSRequest DeleteUserPoolClient where
  type
    AWSResponse DeleteUserPoolClient =
      DeleteUserPoolClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteUserPoolClientResponse'

instance Prelude.Hashable DeleteUserPoolClient where
  hashWithSalt _salt DeleteUserPoolClient' {..} =
    _salt `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` clientId

instance Prelude.NFData DeleteUserPoolClient where
  rnf DeleteUserPoolClient' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf clientId

instance Data.ToHeaders DeleteUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DeleteUserPoolClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteUserPoolClient where
  toJSON DeleteUserPoolClient' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("ClientId" Data..= clientId)
          ]
      )

instance Data.ToPath DeleteUserPoolClient where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUserPoolClient where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserPoolClientResponse' smart constructor.
data DeleteUserPoolClientResponse = DeleteUserPoolClientResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPoolClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserPoolClientResponse ::
  DeleteUserPoolClientResponse
newDeleteUserPoolClientResponse =
  DeleteUserPoolClientResponse'

instance Prelude.NFData DeleteUserPoolClientResponse where
  rnf _ = ()
