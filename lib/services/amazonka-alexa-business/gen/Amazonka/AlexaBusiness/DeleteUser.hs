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
-- Module      : Amazonka.AlexaBusiness.DeleteUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified user by user ARN and enrollment ARN.
module Amazonka.AlexaBusiness.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_userArn,
    deleteUser_enrollmentId,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,

    -- * Response Lenses
    deleteUserResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The ARN of the user to delete in the organization. Required.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user\'s enrollment in the organization. Required.
    enrollmentId :: Prelude.Text
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
-- 'userArn', 'deleteUser_userArn' - The ARN of the user to delete in the organization. Required.
--
-- 'enrollmentId', 'deleteUser_enrollmentId' - The ARN of the user\'s enrollment in the organization. Required.
newDeleteUser ::
  -- | 'enrollmentId'
  Prelude.Text ->
  DeleteUser
newDeleteUser pEnrollmentId_ =
  DeleteUser'
    { userArn = Prelude.Nothing,
      enrollmentId = pEnrollmentId_
    }

-- | The ARN of the user to delete in the organization. Required.
deleteUser_userArn :: Lens.Lens' DeleteUser (Prelude.Maybe Prelude.Text)
deleteUser_userArn = Lens.lens (\DeleteUser' {userArn} -> userArn) (\s@DeleteUser' {} a -> s {userArn = a} :: DeleteUser)

-- | The ARN of the user\'s enrollment in the organization. Required.
deleteUser_enrollmentId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_enrollmentId = Lens.lens (\DeleteUser' {enrollmentId} -> enrollmentId) (\s@DeleteUser' {} a -> s {enrollmentId = a} :: DeleteUser)

instance Core.AWSRequest DeleteUser where
  type AWSResponse DeleteUser = DeleteUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUser where
  hashWithSalt _salt DeleteUser' {..} =
    _salt `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` enrollmentId

instance Prelude.NFData DeleteUser where
  rnf DeleteUser' {..} =
    Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf enrollmentId

instance Data.ToHeaders DeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DeleteUser" ::
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
          [ ("UserArn" Data..=) Prelude.<$> userArn,
            Prelude.Just ("EnrollmentId" Data..= enrollmentId)
          ]
      )

instance Data.ToPath DeleteUser where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteUserResponse where
  rnf DeleteUserResponse' {..} = Prelude.rnf httpStatus
