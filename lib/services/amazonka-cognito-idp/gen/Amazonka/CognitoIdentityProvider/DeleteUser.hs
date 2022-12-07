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
-- Module      : Amazonka.CognitoIdentityProvider.DeleteUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to delete himself or herself.
module Amazonka.CognitoIdentityProvider.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_accessToken,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to delete a user.
--
-- /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | A valid access token that Amazon Cognito issued to the user whose user
    -- profile you want to delete.
    accessToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'deleteUser_accessToken' - A valid access token that Amazon Cognito issued to the user whose user
-- profile you want to delete.
newDeleteUser ::
  -- | 'accessToken'
  Prelude.Text ->
  DeleteUser
newDeleteUser pAccessToken_ =
  DeleteUser'
    { accessToken =
        Data._Sensitive Lens.# pAccessToken_
    }

-- | A valid access token that Amazon Cognito issued to the user whose user
-- profile you want to delete.
deleteUser_accessToken :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_accessToken = Lens.lens (\DeleteUser' {accessToken} -> accessToken) (\s@DeleteUser' {} a -> s {accessToken = a} :: DeleteUser) Prelude.. Data._Sensitive

instance Core.AWSRequest DeleteUser where
  type AWSResponse DeleteUser = DeleteUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteUserResponse'

instance Prelude.Hashable DeleteUser where
  hashWithSalt _salt DeleteUser' {..} =
    _salt `Prelude.hashWithSalt` accessToken

instance Prelude.NFData DeleteUser where
  rnf DeleteUser' {..} = Prelude.rnf accessToken

instance Data.ToHeaders DeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DeleteUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccessToken" Data..= accessToken)]
      )

instance Data.ToPath DeleteUser where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserResponse ::
  DeleteUserResponse
newDeleteUserResponse = DeleteUserResponse'

instance Prelude.NFData DeleteUserResponse where
  rnf _ = ()
