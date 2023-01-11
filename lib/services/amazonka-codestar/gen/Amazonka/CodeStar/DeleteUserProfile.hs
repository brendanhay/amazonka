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
-- Module      : Amazonka.CodeStar.DeleteUserProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile in AWS CodeStar, including all personal
-- preference data associated with that profile, such as display name and
-- email address. It does not delete the history of that user, for example
-- the history of commits made by that user.
module Amazonka.CodeStar.DeleteUserProfile
  ( -- * Creating a Request
    DeleteUserProfile (..),
    newDeleteUserProfile,

    -- * Request Lenses
    deleteUserProfile_userArn,

    -- * Destructuring the Response
    DeleteUserProfileResponse (..),
    newDeleteUserProfileResponse,

    -- * Response Lenses
    deleteUserProfileResponse_httpStatus,
    deleteUserProfileResponse_userArn,
  )
where

import Amazonka.CodeStar.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUserProfile' smart constructor.
data DeleteUserProfile = DeleteUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'deleteUserProfile_userArn' - The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
newDeleteUserProfile ::
  -- | 'userArn'
  Prelude.Text ->
  DeleteUserProfile
newDeleteUserProfile pUserArn_ =
  DeleteUserProfile' {userArn = pUserArn_}

-- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
deleteUserProfile_userArn :: Lens.Lens' DeleteUserProfile Prelude.Text
deleteUserProfile_userArn = Lens.lens (\DeleteUserProfile' {userArn} -> userArn) (\s@DeleteUserProfile' {} a -> s {userArn = a} :: DeleteUserProfile)

instance Core.AWSRequest DeleteUserProfile where
  type
    AWSResponse DeleteUserProfile =
      DeleteUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUserProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "userArn")
      )

instance Prelude.Hashable DeleteUserProfile where
  hashWithSalt _salt DeleteUserProfile' {..} =
    _salt `Prelude.hashWithSalt` userArn

instance Prelude.NFData DeleteUserProfile where
  rnf DeleteUserProfile' {..} = Prelude.rnf userArn

instance Data.ToHeaders DeleteUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeStar_20170419.DeleteUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteUserProfile where
  toJSON DeleteUserProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("userArn" Data..= userArn)]
      )

instance Data.ToPath DeleteUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserProfileResponse_httpStatus' - The response's http status code.
--
-- 'userArn', 'deleteUserProfileResponse_userArn' - The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
newDeleteUserProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'userArn'
  Prelude.Text ->
  DeleteUserProfileResponse
newDeleteUserProfileResponse pHttpStatus_ pUserArn_ =
  DeleteUserProfileResponse'
    { httpStatus =
        pHttpStatus_,
      userArn = pUserArn_
    }

-- | The response's http status code.
deleteUserProfileResponse_httpStatus :: Lens.Lens' DeleteUserProfileResponse Prelude.Int
deleteUserProfileResponse_httpStatus = Lens.lens (\DeleteUserProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteUserProfileResponse' {} a -> s {httpStatus = a} :: DeleteUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
deleteUserProfileResponse_userArn :: Lens.Lens' DeleteUserProfileResponse Prelude.Text
deleteUserProfileResponse_userArn = Lens.lens (\DeleteUserProfileResponse' {userArn} -> userArn) (\s@DeleteUserProfileResponse' {} a -> s {userArn = a} :: DeleteUserProfileResponse)

instance Prelude.NFData DeleteUserProfileResponse where
  rnf DeleteUserProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf userArn
