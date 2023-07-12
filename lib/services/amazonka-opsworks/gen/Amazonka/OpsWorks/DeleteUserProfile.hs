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
-- Module      : Amazonka.OpsWorks.DeleteUserProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DeleteUserProfile
  ( -- * Creating a Request
    DeleteUserProfile (..),
    newDeleteUserProfile,

    -- * Request Lenses
    deleteUserProfile_iamUserArn,

    -- * Destructuring the Response
    DeleteUserProfileResponse (..),
    newDeleteUserProfileResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUserProfile' smart constructor.
data DeleteUserProfile = DeleteUserProfile'
  { -- | The user\'s IAM ARN. This can also be a federated user\'s ARN.
    iamUserArn :: Prelude.Text
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
-- 'iamUserArn', 'deleteUserProfile_iamUserArn' - The user\'s IAM ARN. This can also be a federated user\'s ARN.
newDeleteUserProfile ::
  -- | 'iamUserArn'
  Prelude.Text ->
  DeleteUserProfile
newDeleteUserProfile pIamUserArn_ =
  DeleteUserProfile' {iamUserArn = pIamUserArn_}

-- | The user\'s IAM ARN. This can also be a federated user\'s ARN.
deleteUserProfile_iamUserArn :: Lens.Lens' DeleteUserProfile Prelude.Text
deleteUserProfile_iamUserArn = Lens.lens (\DeleteUserProfile' {iamUserArn} -> iamUserArn) (\s@DeleteUserProfile' {} a -> s {iamUserArn = a} :: DeleteUserProfile)

instance Core.AWSRequest DeleteUserProfile where
  type
    AWSResponse DeleteUserProfile =
      DeleteUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteUserProfileResponse'

instance Prelude.Hashable DeleteUserProfile where
  hashWithSalt _salt DeleteUserProfile' {..} =
    _salt `Prelude.hashWithSalt` iamUserArn

instance Prelude.NFData DeleteUserProfile where
  rnf DeleteUserProfile' {..} = Prelude.rnf iamUserArn

instance Data.ToHeaders DeleteUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DeleteUserProfile" ::
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
          [Prelude.Just ("IamUserArn" Data..= iamUserArn)]
      )

instance Data.ToPath DeleteUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserProfileResponse ::
  DeleteUserProfileResponse
newDeleteUserProfileResponse =
  DeleteUserProfileResponse'

instance Prelude.NFData DeleteUserProfileResponse where
  rnf _ = ()
