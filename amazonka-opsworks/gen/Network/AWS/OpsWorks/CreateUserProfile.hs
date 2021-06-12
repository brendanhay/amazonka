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
-- Module      : Network.AWS.OpsWorks.CreateUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user profile.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.CreateUserProfile
  ( -- * Creating a Request
    CreateUserProfile (..),
    newCreateUserProfile,

    -- * Request Lenses
    createUserProfile_allowSelfManagement,
    createUserProfile_sshUsername,
    createUserProfile_sshPublicKey,
    createUserProfile_iamUserArn,

    -- * Destructuring the Response
    CreateUserProfileResponse (..),
    newCreateUserProfileResponse,

    -- * Response Lenses
    createUserProfileResponse_iamUserArn,
    createUserProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | Whether users can specify their own SSH public key through the My
    -- Settings page. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User\'s Public SSH Key>.
    allowSelfManagement :: Core.Maybe Core.Bool,
    -- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
    -- [0-9], \'-\', and \'_\'. If the specified name includes other
    -- punctuation marks, AWS OpsWorks Stacks removes them. For example,
    -- @my.name@ will be changed to @myname@. If you do not specify an SSH user
    -- name, AWS OpsWorks Stacks generates one from the IAM user name.
    sshUsername :: Core.Maybe Core.Text,
    -- | The user\'s public SSH key.
    sshPublicKey :: Core.Maybe Core.Text,
    -- | The user\'s IAM ARN; this can also be a federated user\'s ARN.
    iamUserArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowSelfManagement', 'createUserProfile_allowSelfManagement' - Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User\'s Public SSH Key>.
--
-- 'sshUsername', 'createUserProfile_sshUsername' - The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks Stacks removes them. For example,
-- @my.name@ will be changed to @myname@. If you do not specify an SSH user
-- name, AWS OpsWorks Stacks generates one from the IAM user name.
--
-- 'sshPublicKey', 'createUserProfile_sshPublicKey' - The user\'s public SSH key.
--
-- 'iamUserArn', 'createUserProfile_iamUserArn' - The user\'s IAM ARN; this can also be a federated user\'s ARN.
newCreateUserProfile ::
  -- | 'iamUserArn'
  Core.Text ->
  CreateUserProfile
newCreateUserProfile pIamUserArn_ =
  CreateUserProfile'
    { allowSelfManagement =
        Core.Nothing,
      sshUsername = Core.Nothing,
      sshPublicKey = Core.Nothing,
      iamUserArn = pIamUserArn_
    }

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User\'s Public SSH Key>.
createUserProfile_allowSelfManagement :: Lens.Lens' CreateUserProfile (Core.Maybe Core.Bool)
createUserProfile_allowSelfManagement = Lens.lens (\CreateUserProfile' {allowSelfManagement} -> allowSelfManagement) (\s@CreateUserProfile' {} a -> s {allowSelfManagement = a} :: CreateUserProfile)

-- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks Stacks removes them. For example,
-- @my.name@ will be changed to @myname@. If you do not specify an SSH user
-- name, AWS OpsWorks Stacks generates one from the IAM user name.
createUserProfile_sshUsername :: Lens.Lens' CreateUserProfile (Core.Maybe Core.Text)
createUserProfile_sshUsername = Lens.lens (\CreateUserProfile' {sshUsername} -> sshUsername) (\s@CreateUserProfile' {} a -> s {sshUsername = a} :: CreateUserProfile)

-- | The user\'s public SSH key.
createUserProfile_sshPublicKey :: Lens.Lens' CreateUserProfile (Core.Maybe Core.Text)
createUserProfile_sshPublicKey = Lens.lens (\CreateUserProfile' {sshPublicKey} -> sshPublicKey) (\s@CreateUserProfile' {} a -> s {sshPublicKey = a} :: CreateUserProfile)

-- | The user\'s IAM ARN; this can also be a federated user\'s ARN.
createUserProfile_iamUserArn :: Lens.Lens' CreateUserProfile Core.Text
createUserProfile_iamUserArn = Lens.lens (\CreateUserProfile' {iamUserArn} -> iamUserArn) (\s@CreateUserProfile' {} a -> s {iamUserArn = a} :: CreateUserProfile)

instance Core.AWSRequest CreateUserProfile where
  type
    AWSResponse CreateUserProfile =
      CreateUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Core.<$> (x Core..?> "IamUserArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUserProfile

instance Core.NFData CreateUserProfile

instance Core.ToHeaders CreateUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.CreateUserProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUserProfile where
  toJSON CreateUserProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowSelfManagement" Core..=)
              Core.<$> allowSelfManagement,
            ("SshUsername" Core..=) Core.<$> sshUsername,
            ("SshPublicKey" Core..=) Core.<$> sshPublicKey,
            Core.Just ("IamUserArn" Core..= iamUserArn)
          ]
      )

instance Core.ToPath CreateUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery CreateUserProfile where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @CreateUserProfile@ request.
--
-- /See:/ 'newCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The user\'s IAM ARN.
    iamUserArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUserArn', 'createUserProfileResponse_iamUserArn' - The user\'s IAM ARN.
--
-- 'httpStatus', 'createUserProfileResponse_httpStatus' - The response's http status code.
newCreateUserProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserProfileResponse
newCreateUserProfileResponse pHttpStatus_ =
  CreateUserProfileResponse'
    { iamUserArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user\'s IAM ARN.
createUserProfileResponse_iamUserArn :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.Text)
createUserProfileResponse_iamUserArn = Lens.lens (\CreateUserProfileResponse' {iamUserArn} -> iamUserArn) (\s@CreateUserProfileResponse' {} a -> s {iamUserArn = a} :: CreateUserProfileResponse)

-- | The response's http status code.
createUserProfileResponse_httpStatus :: Lens.Lens' CreateUserProfileResponse Core.Int
createUserProfileResponse_httpStatus = Lens.lens (\CreateUserProfileResponse' {httpStatus} -> httpStatus) (\s@CreateUserProfileResponse' {} a -> s {httpStatus = a} :: CreateUserProfileResponse)

instance Core.NFData CreateUserProfileResponse
