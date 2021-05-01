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
-- Module      : Network.AWS.MQ.DeleteUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an ActiveMQ user.
module Network.AWS.MQ.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_username,
    deleteUser_brokerId,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,

    -- * Response Lenses
    deleteUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'deleteUser_username' - The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
--
-- 'brokerId', 'deleteUser_brokerId' - The unique ID that Amazon MQ generates for the broker.
newDeleteUser ::
  -- | 'username'
  Prelude.Text ->
  -- | 'brokerId'
  Prelude.Text ->
  DeleteUser
newDeleteUser pUsername_ pBrokerId_ =
  DeleteUser'
    { username = pUsername_,
      brokerId = pBrokerId_
    }

-- | The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
deleteUser_username :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_username = Lens.lens (\DeleteUser' {username} -> username) (\s@DeleteUser' {} a -> s {username = a} :: DeleteUser)

-- | The unique ID that Amazon MQ generates for the broker.
deleteUser_brokerId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_brokerId = Lens.lens (\DeleteUser' {brokerId} -> brokerId) (\s@DeleteUser' {} a -> s {brokerId = a} :: DeleteUser)

instance Prelude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUser

instance Prelude.NFData DeleteUser

instance Prelude.ToHeaders DeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteUser where
  toPath DeleteUser' {..} =
    Prelude.mconcat
      [ "/v1/brokers/",
        Prelude.toBS brokerId,
        "/users/",
        Prelude.toBS username
      ]

instance Prelude.ToQuery DeleteUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserResponse_httpStatus' - The response's http status code.
newDeleteUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteUserResponse
newDeleteUserResponse pHttpStatus_ =
  DeleteUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteUserResponse_httpStatus :: Lens.Lens' DeleteUserResponse Prelude.Int
deleteUserResponse_httpStatus = Lens.lens (\DeleteUserResponse' {httpStatus} -> httpStatus) (\s@DeleteUserResponse' {} a -> s {httpStatus = a} :: DeleteUserResponse)

instance Prelude.NFData DeleteUserResponse
