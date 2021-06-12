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
-- Module      : Network.AWS.CodeStar.CreateUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile for a user that includes user preferences, such as the
-- display name and email address assocciated with the user, in AWS
-- CodeStar. The user profile is not project-specific. Information in the
-- user profile is displayed wherever the user\'s information appears to
-- other users in AWS CodeStar.
module Network.AWS.CodeStar.CreateUserProfile
  ( -- * Creating a Request
    CreateUserProfile (..),
    newCreateUserProfile,

    -- * Request Lenses
    createUserProfile_sshPublicKey,
    createUserProfile_userArn,
    createUserProfile_displayName,
    createUserProfile_emailAddress,

    -- * Destructuring the Response
    CreateUserProfileResponse (..),
    newCreateUserProfileResponse,

    -- * Response Lenses
    createUserProfileResponse_createdTimestamp,
    createUserProfileResponse_lastModifiedTimestamp,
    createUserProfileResponse_sshPublicKey,
    createUserProfileResponse_displayName,
    createUserProfileResponse_emailAddress,
    createUserProfileResponse_httpStatus,
    createUserProfileResponse_userArn,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | The SSH public key associated with the user in AWS CodeStar. If a
    -- project owner allows the user remote access to project resources, this
    -- public key will be used along with the user\'s private key for SSH
    -- access.
    sshPublicKey :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Core.Text,
    -- | The name that will be displayed as the friendly name for the user in AWS
    -- CodeStar.
    displayName :: Core.Sensitive Core.Text,
    -- | The email address that will be displayed as part of the user\'s profile
    -- in AWS CodeStar.
    emailAddress :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sshPublicKey', 'createUserProfile_sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
--
-- 'userArn', 'createUserProfile_userArn' - The Amazon Resource Name (ARN) of the user in IAM.
--
-- 'displayName', 'createUserProfile_displayName' - The name that will be displayed as the friendly name for the user in AWS
-- CodeStar.
--
-- 'emailAddress', 'createUserProfile_emailAddress' - The email address that will be displayed as part of the user\'s profile
-- in AWS CodeStar.
newCreateUserProfile ::
  -- | 'userArn'
  Core.Text ->
  -- | 'displayName'
  Core.Text ->
  -- | 'emailAddress'
  Core.Text ->
  CreateUserProfile
newCreateUserProfile
  pUserArn_
  pDisplayName_
  pEmailAddress_ =
    CreateUserProfile'
      { sshPublicKey = Core.Nothing,
        userArn = pUserArn_,
        displayName = Core._Sensitive Lens.# pDisplayName_,
        emailAddress = Core._Sensitive Lens.# pEmailAddress_
      }

-- | The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
createUserProfile_sshPublicKey :: Lens.Lens' CreateUserProfile (Core.Maybe Core.Text)
createUserProfile_sshPublicKey = Lens.lens (\CreateUserProfile' {sshPublicKey} -> sshPublicKey) (\s@CreateUserProfile' {} a -> s {sshPublicKey = a} :: CreateUserProfile)

-- | The Amazon Resource Name (ARN) of the user in IAM.
createUserProfile_userArn :: Lens.Lens' CreateUserProfile Core.Text
createUserProfile_userArn = Lens.lens (\CreateUserProfile' {userArn} -> userArn) (\s@CreateUserProfile' {} a -> s {userArn = a} :: CreateUserProfile)

-- | The name that will be displayed as the friendly name for the user in AWS
-- CodeStar.
createUserProfile_displayName :: Lens.Lens' CreateUserProfile Core.Text
createUserProfile_displayName = Lens.lens (\CreateUserProfile' {displayName} -> displayName) (\s@CreateUserProfile' {} a -> s {displayName = a} :: CreateUserProfile) Core.. Core._Sensitive

-- | The email address that will be displayed as part of the user\'s profile
-- in AWS CodeStar.
createUserProfile_emailAddress :: Lens.Lens' CreateUserProfile Core.Text
createUserProfile_emailAddress = Lens.lens (\CreateUserProfile' {emailAddress} -> emailAddress) (\s@CreateUserProfile' {} a -> s {emailAddress = a} :: CreateUserProfile) Core.. Core._Sensitive

instance Core.AWSRequest CreateUserProfile where
  type
    AWSResponse CreateUserProfile =
      CreateUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Core.<$> (x Core..?> "createdTimestamp")
            Core.<*> (x Core..?> "lastModifiedTimestamp")
            Core.<*> (x Core..?> "sshPublicKey")
            Core.<*> (x Core..?> "displayName")
            Core.<*> (x Core..?> "emailAddress")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "userArn")
      )

instance Core.Hashable CreateUserProfile

instance Core.NFData CreateUserProfile

instance Core.ToHeaders CreateUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.CreateUserProfile" ::
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
          [ ("sshPublicKey" Core..=) Core.<$> sshPublicKey,
            Core.Just ("userArn" Core..= userArn),
            Core.Just ("displayName" Core..= displayName),
            Core.Just ("emailAddress" Core..= emailAddress)
          ]
      )

instance Core.ToPath CreateUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery CreateUserProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The date the user profile was created, in timestamp format.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The date the user profile was last modified, in timestamp format.
    lastModifiedTimestamp :: Core.Maybe Core.POSIX,
    -- | The SSH public key associated with the user in AWS CodeStar. This is the
    -- public portion of the public\/private keypair the user can use to access
    -- project resources if a project owner allows the user remote access to
    -- those resources.
    sshPublicKey :: Core.Maybe Core.Text,
    -- | The name that is displayed as the friendly name for the user in AWS
    -- CodeStar.
    displayName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The email address that is displayed as part of the user\'s profile in
    -- AWS CodeStar.
    emailAddress :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'createUserProfileResponse_createdTimestamp' - The date the user profile was created, in timestamp format.
--
-- 'lastModifiedTimestamp', 'createUserProfileResponse_lastModifiedTimestamp' - The date the user profile was last modified, in timestamp format.
--
-- 'sshPublicKey', 'createUserProfileResponse_sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. This is the
-- public portion of the public\/private keypair the user can use to access
-- project resources if a project owner allows the user remote access to
-- those resources.
--
-- 'displayName', 'createUserProfileResponse_displayName' - The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
--
-- 'emailAddress', 'createUserProfileResponse_emailAddress' - The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
--
-- 'httpStatus', 'createUserProfileResponse_httpStatus' - The response's http status code.
--
-- 'userArn', 'createUserProfileResponse_userArn' - The Amazon Resource Name (ARN) of the user in IAM.
newCreateUserProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'userArn'
  Core.Text ->
  CreateUserProfileResponse
newCreateUserProfileResponse pHttpStatus_ pUserArn_ =
  CreateUserProfileResponse'
    { createdTimestamp =
        Core.Nothing,
      lastModifiedTimestamp = Core.Nothing,
      sshPublicKey = Core.Nothing,
      displayName = Core.Nothing,
      emailAddress = Core.Nothing,
      httpStatus = pHttpStatus_,
      userArn = pUserArn_
    }

-- | The date the user profile was created, in timestamp format.
createUserProfileResponse_createdTimestamp :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.UTCTime)
createUserProfileResponse_createdTimestamp = Lens.lens (\CreateUserProfileResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateUserProfileResponse' {} a -> s {createdTimestamp = a} :: CreateUserProfileResponse) Core.. Lens.mapping Core._Time

-- | The date the user profile was last modified, in timestamp format.
createUserProfileResponse_lastModifiedTimestamp :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.UTCTime)
createUserProfileResponse_lastModifiedTimestamp = Lens.lens (\CreateUserProfileResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@CreateUserProfileResponse' {} a -> s {lastModifiedTimestamp = a} :: CreateUserProfileResponse) Core.. Lens.mapping Core._Time

-- | The SSH public key associated with the user in AWS CodeStar. This is the
-- public portion of the public\/private keypair the user can use to access
-- project resources if a project owner allows the user remote access to
-- those resources.
createUserProfileResponse_sshPublicKey :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.Text)
createUserProfileResponse_sshPublicKey = Lens.lens (\CreateUserProfileResponse' {sshPublicKey} -> sshPublicKey) (\s@CreateUserProfileResponse' {} a -> s {sshPublicKey = a} :: CreateUserProfileResponse)

-- | The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
createUserProfileResponse_displayName :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.Text)
createUserProfileResponse_displayName = Lens.lens (\CreateUserProfileResponse' {displayName} -> displayName) (\s@CreateUserProfileResponse' {} a -> s {displayName = a} :: CreateUserProfileResponse) Core.. Lens.mapping Core._Sensitive

-- | The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
createUserProfileResponse_emailAddress :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.Text)
createUserProfileResponse_emailAddress = Lens.lens (\CreateUserProfileResponse' {emailAddress} -> emailAddress) (\s@CreateUserProfileResponse' {} a -> s {emailAddress = a} :: CreateUserProfileResponse) Core.. Lens.mapping Core._Sensitive

-- | The response's http status code.
createUserProfileResponse_httpStatus :: Lens.Lens' CreateUserProfileResponse Core.Int
createUserProfileResponse_httpStatus = Lens.lens (\CreateUserProfileResponse' {httpStatus} -> httpStatus) (\s@CreateUserProfileResponse' {} a -> s {httpStatus = a} :: CreateUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user in IAM.
createUserProfileResponse_userArn :: Lens.Lens' CreateUserProfileResponse Core.Text
createUserProfileResponse_userArn = Lens.lens (\CreateUserProfileResponse' {userArn} -> userArn) (\s@CreateUserProfileResponse' {} a -> s {userArn = a} :: CreateUserProfileResponse)

instance Core.NFData CreateUserProfileResponse
