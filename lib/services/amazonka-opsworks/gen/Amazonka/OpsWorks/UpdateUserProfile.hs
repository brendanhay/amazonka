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
-- Module      : Amazonka.OpsWorks.UpdateUserProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified user profile.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.UpdateUserProfile
  ( -- * Creating a Request
    UpdateUserProfile (..),
    newUpdateUserProfile,

    -- * Request Lenses
    updateUserProfile_sshPublicKey,
    updateUserProfile_sshUsername,
    updateUserProfile_allowSelfManagement,
    updateUserProfile_iamUserArn,

    -- * Destructuring the Response
    UpdateUserProfileResponse (..),
    newUpdateUserProfileResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | The user\'s new SSH public key.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
    -- [0-9], \'-\', and \'_\'. If the specified name includes other
    -- punctuation marks, AWS OpsWorks Stacks removes them. For example,
    -- @my.name@ will be changed to @myname@. If you do not specify an SSH user
    -- name, AWS OpsWorks Stacks generates one from the IAM user name.
    sshUsername :: Prelude.Maybe Prelude.Text,
    -- | Whether users can specify their own SSH public key through the My
    -- Settings page. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
    allowSelfManagement :: Prelude.Maybe Prelude.Bool,
    -- | The user IAM ARN. This can also be a federated user\'s ARN.
    iamUserArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sshPublicKey', 'updateUserProfile_sshPublicKey' - The user\'s new SSH public key.
--
-- 'sshUsername', 'updateUserProfile_sshUsername' - The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks Stacks removes them. For example,
-- @my.name@ will be changed to @myname@. If you do not specify an SSH user
-- name, AWS OpsWorks Stacks generates one from the IAM user name.
--
-- 'allowSelfManagement', 'updateUserProfile_allowSelfManagement' - Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
--
-- 'iamUserArn', 'updateUserProfile_iamUserArn' - The user IAM ARN. This can also be a federated user\'s ARN.
newUpdateUserProfile ::
  -- | 'iamUserArn'
  Prelude.Text ->
  UpdateUserProfile
newUpdateUserProfile pIamUserArn_ =
  UpdateUserProfile'
    { sshPublicKey = Prelude.Nothing,
      sshUsername = Prelude.Nothing,
      allowSelfManagement = Prelude.Nothing,
      iamUserArn = pIamUserArn_
    }

-- | The user\'s new SSH public key.
updateUserProfile_sshPublicKey :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Text)
updateUserProfile_sshPublicKey = Lens.lens (\UpdateUserProfile' {sshPublicKey} -> sshPublicKey) (\s@UpdateUserProfile' {} a -> s {sshPublicKey = a} :: UpdateUserProfile)

-- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks Stacks removes them. For example,
-- @my.name@ will be changed to @myname@. If you do not specify an SSH user
-- name, AWS OpsWorks Stacks generates one from the IAM user name.
updateUserProfile_sshUsername :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Text)
updateUserProfile_sshUsername = Lens.lens (\UpdateUserProfile' {sshUsername} -> sshUsername) (\s@UpdateUserProfile' {} a -> s {sshUsername = a} :: UpdateUserProfile)

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
updateUserProfile_allowSelfManagement :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Bool)
updateUserProfile_allowSelfManagement = Lens.lens (\UpdateUserProfile' {allowSelfManagement} -> allowSelfManagement) (\s@UpdateUserProfile' {} a -> s {allowSelfManagement = a} :: UpdateUserProfile)

-- | The user IAM ARN. This can also be a federated user\'s ARN.
updateUserProfile_iamUserArn :: Lens.Lens' UpdateUserProfile Prelude.Text
updateUserProfile_iamUserArn = Lens.lens (\UpdateUserProfile' {iamUserArn} -> iamUserArn) (\s@UpdateUserProfile' {} a -> s {iamUserArn = a} :: UpdateUserProfile)

instance Core.AWSRequest UpdateUserProfile where
  type
    AWSResponse UpdateUserProfile =
      UpdateUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateUserProfileResponse'

instance Prelude.Hashable UpdateUserProfile where
  hashWithSalt _salt UpdateUserProfile' {..} =
    _salt `Prelude.hashWithSalt` sshPublicKey
      `Prelude.hashWithSalt` sshUsername
      `Prelude.hashWithSalt` allowSelfManagement
      `Prelude.hashWithSalt` iamUserArn

instance Prelude.NFData UpdateUserProfile where
  rnf UpdateUserProfile' {..} =
    Prelude.rnf sshPublicKey
      `Prelude.seq` Prelude.rnf sshUsername
      `Prelude.seq` Prelude.rnf allowSelfManagement
      `Prelude.seq` Prelude.rnf iamUserArn

instance Core.ToHeaders UpdateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.UpdateUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SshPublicKey" Core..=) Prelude.<$> sshPublicKey,
            ("SshUsername" Core..=) Prelude.<$> sshUsername,
            ("AllowSelfManagement" Core..=)
              Prelude.<$> allowSelfManagement,
            Prelude.Just ("IamUserArn" Core..= iamUserArn)
          ]
      )

instance Core.ToPath UpdateUserProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserProfileResponse ::
  UpdateUserProfileResponse
newUpdateUserProfileResponse =
  UpdateUserProfileResponse'

instance Prelude.NFData UpdateUserProfileResponse where
  rnf _ = ()
