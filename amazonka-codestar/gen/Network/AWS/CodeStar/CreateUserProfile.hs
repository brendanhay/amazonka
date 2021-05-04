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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | The SSH public key associated with the user in AWS CodeStar. If a
    -- project owner allows the user remote access to project resources, this
    -- public key will be used along with the user\'s private key for SSH
    -- access.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Prelude.Text,
    -- | The name that will be displayed as the friendly name for the user in AWS
    -- CodeStar.
    displayName :: Prelude.Sensitive Prelude.Text,
    -- | The email address that will be displayed as part of the user\'s profile
    -- in AWS CodeStar.
    emailAddress :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'displayName'
  Prelude.Text ->
  -- | 'emailAddress'
  Prelude.Text ->
  CreateUserProfile
newCreateUserProfile
  pUserArn_
  pDisplayName_
  pEmailAddress_ =
    CreateUserProfile'
      { sshPublicKey = Prelude.Nothing,
        userArn = pUserArn_,
        displayName =
          Prelude._Sensitive Lens.# pDisplayName_,
        emailAddress =
          Prelude._Sensitive Lens.# pEmailAddress_
      }

-- | The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
createUserProfile_sshPublicKey :: Lens.Lens' CreateUserProfile (Prelude.Maybe Prelude.Text)
createUserProfile_sshPublicKey = Lens.lens (\CreateUserProfile' {sshPublicKey} -> sshPublicKey) (\s@CreateUserProfile' {} a -> s {sshPublicKey = a} :: CreateUserProfile)

-- | The Amazon Resource Name (ARN) of the user in IAM.
createUserProfile_userArn :: Lens.Lens' CreateUserProfile Prelude.Text
createUserProfile_userArn = Lens.lens (\CreateUserProfile' {userArn} -> userArn) (\s@CreateUserProfile' {} a -> s {userArn = a} :: CreateUserProfile)

-- | The name that will be displayed as the friendly name for the user in AWS
-- CodeStar.
createUserProfile_displayName :: Lens.Lens' CreateUserProfile Prelude.Text
createUserProfile_displayName = Lens.lens (\CreateUserProfile' {displayName} -> displayName) (\s@CreateUserProfile' {} a -> s {displayName = a} :: CreateUserProfile) Prelude.. Prelude._Sensitive

-- | The email address that will be displayed as part of the user\'s profile
-- in AWS CodeStar.
createUserProfile_emailAddress :: Lens.Lens' CreateUserProfile Prelude.Text
createUserProfile_emailAddress = Lens.lens (\CreateUserProfile' {emailAddress} -> emailAddress) (\s@CreateUserProfile' {} a -> s {emailAddress = a} :: CreateUserProfile) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest CreateUserProfile where
  type Rs CreateUserProfile = CreateUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Prelude.<$> (x Prelude..?> "createdTimestamp")
            Prelude.<*> (x Prelude..?> "lastModifiedTimestamp")
            Prelude.<*> (x Prelude..?> "sshPublicKey")
            Prelude.<*> (x Prelude..?> "displayName")
            Prelude.<*> (x Prelude..?> "emailAddress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "userArn")
      )

instance Prelude.Hashable CreateUserProfile

instance Prelude.NFData CreateUserProfile

instance Prelude.ToHeaders CreateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeStar_20170419.CreateUserProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateUserProfile where
  toJSON CreateUserProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("sshPublicKey" Prelude..=)
              Prelude.<$> sshPublicKey,
            Prelude.Just ("userArn" Prelude..= userArn),
            Prelude.Just ("displayName" Prelude..= displayName),
            Prelude.Just
              ("emailAddress" Prelude..= emailAddress)
          ]
      )

instance Prelude.ToPath CreateUserProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The date the user profile was created, in timestamp format.
    createdTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The date the user profile was last modified, in timestamp format.
    lastModifiedTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The SSH public key associated with the user in AWS CodeStar. This is the
    -- public portion of the public\/private keypair the user can use to access
    -- project resources if a project owner allows the user remote access to
    -- those resources.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The name that is displayed as the friendly name for the user in AWS
    -- CodeStar.
    displayName :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The email address that is displayed as part of the user\'s profile in
    -- AWS CodeStar.
    emailAddress :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'userArn'
  Prelude.Text ->
  CreateUserProfileResponse
newCreateUserProfileResponse pHttpStatus_ pUserArn_ =
  CreateUserProfileResponse'
    { createdTimestamp =
        Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      displayName = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      userArn = pUserArn_
    }

-- | The date the user profile was created, in timestamp format.
createUserProfileResponse_createdTimestamp :: Lens.Lens' CreateUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
createUserProfileResponse_createdTimestamp = Lens.lens (\CreateUserProfileResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateUserProfileResponse' {} a -> s {createdTimestamp = a} :: CreateUserProfileResponse) Prelude.. Lens.mapping Prelude._Time

-- | The date the user profile was last modified, in timestamp format.
createUserProfileResponse_lastModifiedTimestamp :: Lens.Lens' CreateUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
createUserProfileResponse_lastModifiedTimestamp = Lens.lens (\CreateUserProfileResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@CreateUserProfileResponse' {} a -> s {lastModifiedTimestamp = a} :: CreateUserProfileResponse) Prelude.. Lens.mapping Prelude._Time

-- | The SSH public key associated with the user in AWS CodeStar. This is the
-- public portion of the public\/private keypair the user can use to access
-- project resources if a project owner allows the user remote access to
-- those resources.
createUserProfileResponse_sshPublicKey :: Lens.Lens' CreateUserProfileResponse (Prelude.Maybe Prelude.Text)
createUserProfileResponse_sshPublicKey = Lens.lens (\CreateUserProfileResponse' {sshPublicKey} -> sshPublicKey) (\s@CreateUserProfileResponse' {} a -> s {sshPublicKey = a} :: CreateUserProfileResponse)

-- | The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
createUserProfileResponse_displayName :: Lens.Lens' CreateUserProfileResponse (Prelude.Maybe Prelude.Text)
createUserProfileResponse_displayName = Lens.lens (\CreateUserProfileResponse' {displayName} -> displayName) (\s@CreateUserProfileResponse' {} a -> s {displayName = a} :: CreateUserProfileResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
createUserProfileResponse_emailAddress :: Lens.Lens' CreateUserProfileResponse (Prelude.Maybe Prelude.Text)
createUserProfileResponse_emailAddress = Lens.lens (\CreateUserProfileResponse' {emailAddress} -> emailAddress) (\s@CreateUserProfileResponse' {} a -> s {emailAddress = a} :: CreateUserProfileResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The response's http status code.
createUserProfileResponse_httpStatus :: Lens.Lens' CreateUserProfileResponse Prelude.Int
createUserProfileResponse_httpStatus = Lens.lens (\CreateUserProfileResponse' {httpStatus} -> httpStatus) (\s@CreateUserProfileResponse' {} a -> s {httpStatus = a} :: CreateUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user in IAM.
createUserProfileResponse_userArn :: Lens.Lens' CreateUserProfileResponse Prelude.Text
createUserProfileResponse_userArn = Lens.lens (\CreateUserProfileResponse' {userArn} -> userArn) (\s@CreateUserProfileResponse' {} a -> s {userArn = a} :: CreateUserProfileResponse)

instance Prelude.NFData CreateUserProfileResponse
