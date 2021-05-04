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
-- Module      : Network.AWS.CodeStar.UpdateUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user\'s profile in AWS CodeStar. The user profile is not
-- project-specific. Information in the user profile is displayed wherever
-- the user\'s information appears to other users in AWS CodeStar.
module Network.AWS.CodeStar.UpdateUserProfile
  ( -- * Creating a Request
    UpdateUserProfile (..),
    newUpdateUserProfile,

    -- * Request Lenses
    updateUserProfile_sshPublicKey,
    updateUserProfile_displayName,
    updateUserProfile_emailAddress,
    updateUserProfile_userArn,

    -- * Destructuring the Response
    UpdateUserProfileResponse (..),
    newUpdateUserProfileResponse,

    -- * Response Lenses
    updateUserProfileResponse_createdTimestamp,
    updateUserProfileResponse_lastModifiedTimestamp,
    updateUserProfileResponse_sshPublicKey,
    updateUserProfileResponse_displayName,
    updateUserProfileResponse_emailAddress,
    updateUserProfileResponse_httpStatus,
    updateUserProfileResponse_userArn,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | The SSH public key associated with the user in AWS CodeStar. If a
    -- project owner allows the user remote access to project resources, this
    -- public key will be used along with the user\'s private key for SSH
    -- access.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The name that is displayed as the friendly name for the user in AWS
    -- CodeStar.
    displayName :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The email address that is displayed as part of the user\'s profile in
    -- AWS CodeStar.
    emailAddress :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The name that will be displayed as the friendly name for the user in AWS
    -- CodeStar.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sshPublicKey', 'updateUserProfile_sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
--
-- 'displayName', 'updateUserProfile_displayName' - The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
--
-- 'emailAddress', 'updateUserProfile_emailAddress' - The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
--
-- 'userArn', 'updateUserProfile_userArn' - The name that will be displayed as the friendly name for the user in AWS
-- CodeStar.
newUpdateUserProfile ::
  -- | 'userArn'
  Prelude.Text ->
  UpdateUserProfile
newUpdateUserProfile pUserArn_ =
  UpdateUserProfile'
    { sshPublicKey = Prelude.Nothing,
      displayName = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      userArn = pUserArn_
    }

-- | The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
updateUserProfile_sshPublicKey :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Text)
updateUserProfile_sshPublicKey = Lens.lens (\UpdateUserProfile' {sshPublicKey} -> sshPublicKey) (\s@UpdateUserProfile' {} a -> s {sshPublicKey = a} :: UpdateUserProfile)

-- | The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
updateUserProfile_displayName :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Text)
updateUserProfile_displayName = Lens.lens (\UpdateUserProfile' {displayName} -> displayName) (\s@UpdateUserProfile' {} a -> s {displayName = a} :: UpdateUserProfile) Prelude.. Lens.mapping Prelude._Sensitive

-- | The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
updateUserProfile_emailAddress :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Text)
updateUserProfile_emailAddress = Lens.lens (\UpdateUserProfile' {emailAddress} -> emailAddress) (\s@UpdateUserProfile' {} a -> s {emailAddress = a} :: UpdateUserProfile) Prelude.. Lens.mapping Prelude._Sensitive

-- | The name that will be displayed as the friendly name for the user in AWS
-- CodeStar.
updateUserProfile_userArn :: Lens.Lens' UpdateUserProfile Prelude.Text
updateUserProfile_userArn = Lens.lens (\UpdateUserProfile' {userArn} -> userArn) (\s@UpdateUserProfile' {} a -> s {userArn = a} :: UpdateUserProfile)

instance Prelude.AWSRequest UpdateUserProfile where
  type Rs UpdateUserProfile = UpdateUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserProfileResponse'
            Prelude.<$> (x Prelude..?> "createdTimestamp")
            Prelude.<*> (x Prelude..?> "lastModifiedTimestamp")
            Prelude.<*> (x Prelude..?> "sshPublicKey")
            Prelude.<*> (x Prelude..?> "displayName")
            Prelude.<*> (x Prelude..?> "emailAddress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "userArn")
      )

instance Prelude.Hashable UpdateUserProfile

instance Prelude.NFData UpdateUserProfile

instance Prelude.ToHeaders UpdateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeStar_20170419.UpdateUserProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("sshPublicKey" Prelude..=)
              Prelude.<$> sshPublicKey,
            ("displayName" Prelude..=) Prelude.<$> displayName,
            ("emailAddress" Prelude..=) Prelude.<$> emailAddress,
            Prelude.Just ("userArn" Prelude..= userArn)
          ]
      )

instance Prelude.ToPath UpdateUserProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
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
-- Create a value of 'UpdateUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'updateUserProfileResponse_createdTimestamp' - The date the user profile was created, in timestamp format.
--
-- 'lastModifiedTimestamp', 'updateUserProfileResponse_lastModifiedTimestamp' - The date the user profile was last modified, in timestamp format.
--
-- 'sshPublicKey', 'updateUserProfileResponse_sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. This is the
-- public portion of the public\/private keypair the user can use to access
-- project resources if a project owner allows the user remote access to
-- those resources.
--
-- 'displayName', 'updateUserProfileResponse_displayName' - The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
--
-- 'emailAddress', 'updateUserProfileResponse_emailAddress' - The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
--
-- 'httpStatus', 'updateUserProfileResponse_httpStatus' - The response's http status code.
--
-- 'userArn', 'updateUserProfileResponse_userArn' - The Amazon Resource Name (ARN) of the user in IAM.
newUpdateUserProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'userArn'
  Prelude.Text ->
  UpdateUserProfileResponse
newUpdateUserProfileResponse pHttpStatus_ pUserArn_ =
  UpdateUserProfileResponse'
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
updateUserProfileResponse_createdTimestamp :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
updateUserProfileResponse_createdTimestamp = Lens.lens (\UpdateUserProfileResponse' {createdTimestamp} -> createdTimestamp) (\s@UpdateUserProfileResponse' {} a -> s {createdTimestamp = a} :: UpdateUserProfileResponse) Prelude.. Lens.mapping Prelude._Time

-- | The date the user profile was last modified, in timestamp format.
updateUserProfileResponse_lastModifiedTimestamp :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
updateUserProfileResponse_lastModifiedTimestamp = Lens.lens (\UpdateUserProfileResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@UpdateUserProfileResponse' {} a -> s {lastModifiedTimestamp = a} :: UpdateUserProfileResponse) Prelude.. Lens.mapping Prelude._Time

-- | The SSH public key associated with the user in AWS CodeStar. This is the
-- public portion of the public\/private keypair the user can use to access
-- project resources if a project owner allows the user remote access to
-- those resources.
updateUserProfileResponse_sshPublicKey :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.Text)
updateUserProfileResponse_sshPublicKey = Lens.lens (\UpdateUserProfileResponse' {sshPublicKey} -> sshPublicKey) (\s@UpdateUserProfileResponse' {} a -> s {sshPublicKey = a} :: UpdateUserProfileResponse)

-- | The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
updateUserProfileResponse_displayName :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.Text)
updateUserProfileResponse_displayName = Lens.lens (\UpdateUserProfileResponse' {displayName} -> displayName) (\s@UpdateUserProfileResponse' {} a -> s {displayName = a} :: UpdateUserProfileResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
updateUserProfileResponse_emailAddress :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.Text)
updateUserProfileResponse_emailAddress = Lens.lens (\UpdateUserProfileResponse' {emailAddress} -> emailAddress) (\s@UpdateUserProfileResponse' {} a -> s {emailAddress = a} :: UpdateUserProfileResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The response's http status code.
updateUserProfileResponse_httpStatus :: Lens.Lens' UpdateUserProfileResponse Prelude.Int
updateUserProfileResponse_httpStatus = Lens.lens (\UpdateUserProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateUserProfileResponse' {} a -> s {httpStatus = a} :: UpdateUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user in IAM.
updateUserProfileResponse_userArn :: Lens.Lens' UpdateUserProfileResponse Prelude.Text
updateUserProfileResponse_userArn = Lens.lens (\UpdateUserProfileResponse' {userArn} -> userArn) (\s@UpdateUserProfileResponse' {} a -> s {userArn = a} :: UpdateUserProfileResponse)

instance Prelude.NFData UpdateUserProfileResponse
