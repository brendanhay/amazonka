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
-- Module      : Amazonka.Connect.DeleteUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user account from the specified Amazon Connect instance.
--
-- For information about what happens to a user\'s data when their account
-- is deleted, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/delete-users.html Delete Users from Your Amazon Connect Instance>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_instanceId,
    deleteUser_userId,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteUser_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'userId', 'deleteUser_userId' - The identifier of the user.
newDeleteUser ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DeleteUser
newDeleteUser pInstanceId_ pUserId_ =
  DeleteUser'
    { instanceId = pInstanceId_,
      userId = pUserId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteUser_instanceId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_instanceId = Lens.lens (\DeleteUser' {instanceId} -> instanceId) (\s@DeleteUser' {} a -> s {instanceId = a} :: DeleteUser)

-- | The identifier of the user.
deleteUser_userId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_userId = Lens.lens (\DeleteUser' {userId} -> userId) (\s@DeleteUser' {} a -> s {userId = a} :: DeleteUser)

instance Core.AWSRequest DeleteUser where
  type AWSResponse DeleteUser = DeleteUserResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteUserResponse'

instance Prelude.Hashable DeleteUser where
  hashWithSalt _salt DeleteUser' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DeleteUser where
  rnf DeleteUser' {..} =
    Prelude.rnf instanceId `Prelude.seq`
      Prelude.rnf userId

instance Data.ToHeaders DeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteUser where
  toPath DeleteUser' {..} =
    Prelude.mconcat
      [ "/users/",
        Data.toBS instanceId,
        "/",
        Data.toBS userId
      ]

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
