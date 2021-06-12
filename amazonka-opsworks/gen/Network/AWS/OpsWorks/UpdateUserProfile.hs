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
-- Module      : Network.AWS.OpsWorks.UpdateUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.UpdateUserProfile
  ( -- * Creating a Request
    UpdateUserProfile (..),
    newUpdateUserProfile,

    -- * Request Lenses
    updateUserProfile_allowSelfManagement,
    updateUserProfile_sshUsername,
    updateUserProfile_sshPublicKey,
    updateUserProfile_iamUserArn,

    -- * Destructuring the Response
    UpdateUserProfileResponse (..),
    newUpdateUserProfileResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | Whether users can specify their own SSH public key through the My
    -- Settings page. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
    allowSelfManagement :: Core.Maybe Core.Bool,
    -- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
    -- [0-9], \'-\', and \'_\'. If the specified name includes other
    -- punctuation marks, AWS OpsWorks Stacks removes them. For example,
    -- @my.name@ will be changed to @myname@. If you do not specify an SSH user
    -- name, AWS OpsWorks Stacks generates one from the IAM user name.
    sshUsername :: Core.Maybe Core.Text,
    -- | The user\'s new SSH public key.
    sshPublicKey :: Core.Maybe Core.Text,
    -- | The user IAM ARN. This can also be a federated user\'s ARN.
    iamUserArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowSelfManagement', 'updateUserProfile_allowSelfManagement' - Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
--
-- 'sshUsername', 'updateUserProfile_sshUsername' - The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks Stacks removes them. For example,
-- @my.name@ will be changed to @myname@. If you do not specify an SSH user
-- name, AWS OpsWorks Stacks generates one from the IAM user name.
--
-- 'sshPublicKey', 'updateUserProfile_sshPublicKey' - The user\'s new SSH public key.
--
-- 'iamUserArn', 'updateUserProfile_iamUserArn' - The user IAM ARN. This can also be a federated user\'s ARN.
newUpdateUserProfile ::
  -- | 'iamUserArn'
  Core.Text ->
  UpdateUserProfile
newUpdateUserProfile pIamUserArn_ =
  UpdateUserProfile'
    { allowSelfManagement =
        Core.Nothing,
      sshUsername = Core.Nothing,
      sshPublicKey = Core.Nothing,
      iamUserArn = pIamUserArn_
    }

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
updateUserProfile_allowSelfManagement :: Lens.Lens' UpdateUserProfile (Core.Maybe Core.Bool)
updateUserProfile_allowSelfManagement = Lens.lens (\UpdateUserProfile' {allowSelfManagement} -> allowSelfManagement) (\s@UpdateUserProfile' {} a -> s {allowSelfManagement = a} :: UpdateUserProfile)

-- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks Stacks removes them. For example,
-- @my.name@ will be changed to @myname@. If you do not specify an SSH user
-- name, AWS OpsWorks Stacks generates one from the IAM user name.
updateUserProfile_sshUsername :: Lens.Lens' UpdateUserProfile (Core.Maybe Core.Text)
updateUserProfile_sshUsername = Lens.lens (\UpdateUserProfile' {sshUsername} -> sshUsername) (\s@UpdateUserProfile' {} a -> s {sshUsername = a} :: UpdateUserProfile)

-- | The user\'s new SSH public key.
updateUserProfile_sshPublicKey :: Lens.Lens' UpdateUserProfile (Core.Maybe Core.Text)
updateUserProfile_sshPublicKey = Lens.lens (\UpdateUserProfile' {sshPublicKey} -> sshPublicKey) (\s@UpdateUserProfile' {} a -> s {sshPublicKey = a} :: UpdateUserProfile)

-- | The user IAM ARN. This can also be a federated user\'s ARN.
updateUserProfile_iamUserArn :: Lens.Lens' UpdateUserProfile Core.Text
updateUserProfile_iamUserArn = Lens.lens (\UpdateUserProfile' {iamUserArn} -> iamUserArn) (\s@UpdateUserProfile' {} a -> s {iamUserArn = a} :: UpdateUserProfile)

instance Core.AWSRequest UpdateUserProfile where
  type
    AWSResponse UpdateUserProfile =
      UpdateUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateUserProfileResponse'

instance Core.Hashable UpdateUserProfile

instance Core.NFData UpdateUserProfile

instance Core.ToHeaders UpdateUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.UpdateUserProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowSelfManagement" Core..=)
              Core.<$> allowSelfManagement,
            ("SshUsername" Core..=) Core.<$> sshUsername,
            ("SshPublicKey" Core..=) Core.<$> sshPublicKey,
            Core.Just ("IamUserArn" Core..= iamUserArn)
          ]
      )

instance Core.ToPath UpdateUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery UpdateUserProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserProfileResponse ::
  UpdateUserProfileResponse
newUpdateUserProfileResponse =
  UpdateUserProfileResponse'

instance Core.NFData UpdateUserProfileResponse
