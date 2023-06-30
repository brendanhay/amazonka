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
-- Module      : Amazonka.OpsWorks.CreateUserProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.OpsWorks.CreateUserProfile
  ( -- * Creating a Request
    CreateUserProfile (..),
    newCreateUserProfile,

    -- * Request Lenses
    createUserProfile_allowSelfManagement,
    createUserProfile_sshPublicKey,
    createUserProfile_sshUsername,
    createUserProfile_iamUserArn,

    -- * Destructuring the Response
    CreateUserProfileResponse (..),
    newCreateUserProfileResponse,

    -- * Response Lenses
    createUserProfileResponse_iamUserArn,
    createUserProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | Whether users can specify their own SSH public key through the My
    -- Settings page. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User\'s Public SSH Key>.
    allowSelfManagement :: Prelude.Maybe Prelude.Bool,
    -- | The user\'s public SSH key.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
    -- [0-9], \'-\', and \'_\'. If the specified name includes other
    -- punctuation marks, AWS OpsWorks Stacks removes them. For example,
    -- @my.name@ will be changed to @myname@. If you do not specify an SSH user
    -- name, AWS OpsWorks Stacks generates one from the IAM user name.
    sshUsername :: Prelude.Maybe Prelude.Text,
    -- | The user\'s IAM ARN; this can also be a federated user\'s ARN.
    iamUserArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'sshPublicKey', 'createUserProfile_sshPublicKey' - The user\'s public SSH key.
--
-- 'sshUsername', 'createUserProfile_sshUsername' - The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks Stacks removes them. For example,
-- @my.name@ will be changed to @myname@. If you do not specify an SSH user
-- name, AWS OpsWorks Stacks generates one from the IAM user name.
--
-- 'iamUserArn', 'createUserProfile_iamUserArn' - The user\'s IAM ARN; this can also be a federated user\'s ARN.
newCreateUserProfile ::
  -- | 'iamUserArn'
  Prelude.Text ->
  CreateUserProfile
newCreateUserProfile pIamUserArn_ =
  CreateUserProfile'
    { allowSelfManagement =
        Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      sshUsername = Prelude.Nothing,
      iamUserArn = pIamUserArn_
    }

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User\'s Public SSH Key>.
createUserProfile_allowSelfManagement :: Lens.Lens' CreateUserProfile (Prelude.Maybe Prelude.Bool)
createUserProfile_allowSelfManagement = Lens.lens (\CreateUserProfile' {allowSelfManagement} -> allowSelfManagement) (\s@CreateUserProfile' {} a -> s {allowSelfManagement = a} :: CreateUserProfile)

-- | The user\'s public SSH key.
createUserProfile_sshPublicKey :: Lens.Lens' CreateUserProfile (Prelude.Maybe Prelude.Text)
createUserProfile_sshPublicKey = Lens.lens (\CreateUserProfile' {sshPublicKey} -> sshPublicKey) (\s@CreateUserProfile' {} a -> s {sshPublicKey = a} :: CreateUserProfile)

-- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks Stacks removes them. For example,
-- @my.name@ will be changed to @myname@. If you do not specify an SSH user
-- name, AWS OpsWorks Stacks generates one from the IAM user name.
createUserProfile_sshUsername :: Lens.Lens' CreateUserProfile (Prelude.Maybe Prelude.Text)
createUserProfile_sshUsername = Lens.lens (\CreateUserProfile' {sshUsername} -> sshUsername) (\s@CreateUserProfile' {} a -> s {sshUsername = a} :: CreateUserProfile)

-- | The user\'s IAM ARN; this can also be a federated user\'s ARN.
createUserProfile_iamUserArn :: Lens.Lens' CreateUserProfile Prelude.Text
createUserProfile_iamUserArn = Lens.lens (\CreateUserProfile' {iamUserArn} -> iamUserArn) (\s@CreateUserProfile' {} a -> s {iamUserArn = a} :: CreateUserProfile)

instance Core.AWSRequest CreateUserProfile where
  type
    AWSResponse CreateUserProfile =
      CreateUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Prelude.<$> (x Data..?> "IamUserArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserProfile where
  hashWithSalt _salt CreateUserProfile' {..} =
    _salt
      `Prelude.hashWithSalt` allowSelfManagement
      `Prelude.hashWithSalt` sshPublicKey
      `Prelude.hashWithSalt` sshUsername
      `Prelude.hashWithSalt` iamUserArn

instance Prelude.NFData CreateUserProfile where
  rnf CreateUserProfile' {..} =
    Prelude.rnf allowSelfManagement
      `Prelude.seq` Prelude.rnf sshPublicKey
      `Prelude.seq` Prelude.rnf sshUsername
      `Prelude.seq` Prelude.rnf iamUserArn

instance Data.ToHeaders CreateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.CreateUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUserProfile where
  toJSON CreateUserProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowSelfManagement" Data..=)
              Prelude.<$> allowSelfManagement,
            ("SshPublicKey" Data..=) Prelude.<$> sshPublicKey,
            ("SshUsername" Data..=) Prelude.<$> sshUsername,
            Prelude.Just ("IamUserArn" Data..= iamUserArn)
          ]
      )

instance Data.ToPath CreateUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @CreateUserProfile@ request.
--
-- /See:/ 'newCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The user\'s IAM ARN.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateUserProfileResponse
newCreateUserProfileResponse pHttpStatus_ =
  CreateUserProfileResponse'
    { iamUserArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user\'s IAM ARN.
createUserProfileResponse_iamUserArn :: Lens.Lens' CreateUserProfileResponse (Prelude.Maybe Prelude.Text)
createUserProfileResponse_iamUserArn = Lens.lens (\CreateUserProfileResponse' {iamUserArn} -> iamUserArn) (\s@CreateUserProfileResponse' {} a -> s {iamUserArn = a} :: CreateUserProfileResponse)

-- | The response's http status code.
createUserProfileResponse_httpStatus :: Lens.Lens' CreateUserProfileResponse Prelude.Int
createUserProfileResponse_httpStatus = Lens.lens (\CreateUserProfileResponse' {httpStatus} -> httpStatus) (\s@CreateUserProfileResponse' {} a -> s {httpStatus = a} :: CreateUserProfileResponse)

instance Prelude.NFData CreateUserProfileResponse where
  rnf CreateUserProfileResponse' {..} =
    Prelude.rnf iamUserArn
      `Prelude.seq` Prelude.rnf httpStatus
