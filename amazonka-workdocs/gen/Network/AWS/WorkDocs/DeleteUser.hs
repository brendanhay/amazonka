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
-- Module      : Network.AWS.WorkDocs.DeleteUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified user from a Simple AD or Microsoft AD directory.
module Network.AWS.WorkDocs.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_authenticationToken,
    deleteUser_userId,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | Amazon WorkDocs authentication token. Do not set this field when using
    -- administrative API actions, as in accessing the API using AWS
    -- credentials.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deleteUser_authenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using
-- administrative API actions, as in accessing the API using AWS
-- credentials.
--
-- 'userId', 'deleteUser_userId' - The ID of the user.
newDeleteUser ::
  -- | 'userId'
  Prelude.Text ->
  DeleteUser
newDeleteUser pUserId_ =
  DeleteUser'
    { authenticationToken = Prelude.Nothing,
      userId = pUserId_
    }

-- | Amazon WorkDocs authentication token. Do not set this field when using
-- administrative API actions, as in accessing the API using AWS
-- credentials.
deleteUser_authenticationToken :: Lens.Lens' DeleteUser (Prelude.Maybe Prelude.Text)
deleteUser_authenticationToken = Lens.lens (\DeleteUser' {authenticationToken} -> authenticationToken) (\s@DeleteUser' {} a -> s {authenticationToken = a} :: DeleteUser) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the user.
deleteUser_userId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_userId = Lens.lens (\DeleteUser' {userId} -> userId) (\s@DeleteUser' {} a -> s {userId = a} :: DeleteUser)

instance Prelude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteUserResponse'

instance Prelude.Hashable DeleteUser

instance Prelude.NFData DeleteUser

instance Prelude.ToHeaders DeleteUser where
  toHeaders DeleteUser' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DeleteUser where
  toPath DeleteUser' {..} =
    Prelude.mconcat
      ["/api/v1/users/", Prelude.toBS userId]

instance Prelude.ToQuery DeleteUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserResponse ::
  DeleteUserResponse
newDeleteUserResponse = DeleteUserResponse'

instance Prelude.NFData DeleteUserResponse
