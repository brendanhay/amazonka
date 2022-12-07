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
-- Module      : Amazonka.CodeStar.UpdateUserProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user\'s profile in AWS CodeStar. The user profile is not
-- project-specific. Information in the user profile is displayed wherever
-- the user\'s information appears to other users in AWS CodeStar.
module Amazonka.CodeStar.UpdateUserProfile
  ( -- * Creating a Request
    UpdateUserProfile (..),
    newUpdateUserProfile,

    -- * Request Lenses
    updateUserProfile_displayName,
    updateUserProfile_sshPublicKey,
    updateUserProfile_emailAddress,
    updateUserProfile_userArn,

    -- * Destructuring the Response
    UpdateUserProfileResponse (..),
    newUpdateUserProfileResponse,

    -- * Response Lenses
    updateUserProfileResponse_lastModifiedTimestamp,
    updateUserProfileResponse_createdTimestamp,
    updateUserProfileResponse_displayName,
    updateUserProfileResponse_sshPublicKey,
    updateUserProfileResponse_emailAddress,
    updateUserProfileResponse_httpStatus,
    updateUserProfileResponse_userArn,
  )
where

import Amazonka.CodeStar.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | The name that is displayed as the friendly name for the user in AWS
    -- CodeStar.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The SSH public key associated with the user in AWS CodeStar. If a
    -- project owner allows the user remote access to project resources, this
    -- public key will be used along with the user\'s private key for SSH
    -- access.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The email address that is displayed as part of the user\'s profile in
    -- AWS CodeStar.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name that will be displayed as the friendly name for the user in AWS
    -- CodeStar.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'updateUserProfile_displayName' - The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
--
-- 'sshPublicKey', 'updateUserProfile_sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
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
    { displayName = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      userArn = pUserArn_
    }

-- | The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
updateUserProfile_displayName :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Text)
updateUserProfile_displayName = Lens.lens (\UpdateUserProfile' {displayName} -> displayName) (\s@UpdateUserProfile' {} a -> s {displayName = a} :: UpdateUserProfile) Prelude.. Lens.mapping Data._Sensitive

-- | The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
updateUserProfile_sshPublicKey :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Text)
updateUserProfile_sshPublicKey = Lens.lens (\UpdateUserProfile' {sshPublicKey} -> sshPublicKey) (\s@UpdateUserProfile' {} a -> s {sshPublicKey = a} :: UpdateUserProfile)

-- | The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
updateUserProfile_emailAddress :: Lens.Lens' UpdateUserProfile (Prelude.Maybe Prelude.Text)
updateUserProfile_emailAddress = Lens.lens (\UpdateUserProfile' {emailAddress} -> emailAddress) (\s@UpdateUserProfile' {} a -> s {emailAddress = a} :: UpdateUserProfile) Prelude.. Lens.mapping Data._Sensitive

-- | The name that will be displayed as the friendly name for the user in AWS
-- CodeStar.
updateUserProfile_userArn :: Lens.Lens' UpdateUserProfile Prelude.Text
updateUserProfile_userArn = Lens.lens (\UpdateUserProfile' {userArn} -> userArn) (\s@UpdateUserProfile' {} a -> s {userArn = a} :: UpdateUserProfile)

instance Core.AWSRequest UpdateUserProfile where
  type
    AWSResponse UpdateUserProfile =
      UpdateUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserProfileResponse'
            Prelude.<$> (x Data..?> "lastModifiedTimestamp")
            Prelude.<*> (x Data..?> "createdTimestamp")
            Prelude.<*> (x Data..?> "displayName")
            Prelude.<*> (x Data..?> "sshPublicKey")
            Prelude.<*> (x Data..?> "emailAddress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "userArn")
      )

instance Prelude.Hashable UpdateUserProfile where
  hashWithSalt _salt UpdateUserProfile' {..} =
    _salt `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` sshPublicKey
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` userArn

instance Prelude.NFData UpdateUserProfile where
  rnf UpdateUserProfile' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf sshPublicKey
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf userArn

instance Data.ToHeaders UpdateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeStar_20170419.UpdateUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("displayName" Data..=) Prelude.<$> displayName,
            ("sshPublicKey" Data..=) Prelude.<$> sshPublicKey,
            ("emailAddress" Data..=) Prelude.<$> emailAddress,
            Prelude.Just ("userArn" Data..= userArn)
          ]
      )

instance Data.ToPath UpdateUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { -- | The date the user profile was last modified, in timestamp format.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The date the user profile was created, in timestamp format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name that is displayed as the friendly name for the user in AWS
    -- CodeStar.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The SSH public key associated with the user in AWS CodeStar. This is the
    -- public portion of the public\/private keypair the user can use to access
    -- project resources if a project owner allows the user remote access to
    -- those resources.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The email address that is displayed as part of the user\'s profile in
    -- AWS CodeStar.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimestamp', 'updateUserProfileResponse_lastModifiedTimestamp' - The date the user profile was last modified, in timestamp format.
--
-- 'createdTimestamp', 'updateUserProfileResponse_createdTimestamp' - The date the user profile was created, in timestamp format.
--
-- 'displayName', 'updateUserProfileResponse_displayName' - The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
--
-- 'sshPublicKey', 'updateUserProfileResponse_sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. This is the
-- public portion of the public\/private keypair the user can use to access
-- project resources if a project owner allows the user remote access to
-- those resources.
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
    { lastModifiedTimestamp =
        Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      displayName = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      userArn = pUserArn_
    }

-- | The date the user profile was last modified, in timestamp format.
updateUserProfileResponse_lastModifiedTimestamp :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
updateUserProfileResponse_lastModifiedTimestamp = Lens.lens (\UpdateUserProfileResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@UpdateUserProfileResponse' {} a -> s {lastModifiedTimestamp = a} :: UpdateUserProfileResponse) Prelude.. Lens.mapping Data._Time

-- | The date the user profile was created, in timestamp format.
updateUserProfileResponse_createdTimestamp :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
updateUserProfileResponse_createdTimestamp = Lens.lens (\UpdateUserProfileResponse' {createdTimestamp} -> createdTimestamp) (\s@UpdateUserProfileResponse' {} a -> s {createdTimestamp = a} :: UpdateUserProfileResponse) Prelude.. Lens.mapping Data._Time

-- | The name that is displayed as the friendly name for the user in AWS
-- CodeStar.
updateUserProfileResponse_displayName :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.Text)
updateUserProfileResponse_displayName = Lens.lens (\UpdateUserProfileResponse' {displayName} -> displayName) (\s@UpdateUserProfileResponse' {} a -> s {displayName = a} :: UpdateUserProfileResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The SSH public key associated with the user in AWS CodeStar. This is the
-- public portion of the public\/private keypair the user can use to access
-- project resources if a project owner allows the user remote access to
-- those resources.
updateUserProfileResponse_sshPublicKey :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.Text)
updateUserProfileResponse_sshPublicKey = Lens.lens (\UpdateUserProfileResponse' {sshPublicKey} -> sshPublicKey) (\s@UpdateUserProfileResponse' {} a -> s {sshPublicKey = a} :: UpdateUserProfileResponse)

-- | The email address that is displayed as part of the user\'s profile in
-- AWS CodeStar.
updateUserProfileResponse_emailAddress :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.Text)
updateUserProfileResponse_emailAddress = Lens.lens (\UpdateUserProfileResponse' {emailAddress} -> emailAddress) (\s@UpdateUserProfileResponse' {} a -> s {emailAddress = a} :: UpdateUserProfileResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
updateUserProfileResponse_httpStatus :: Lens.Lens' UpdateUserProfileResponse Prelude.Int
updateUserProfileResponse_httpStatus = Lens.lens (\UpdateUserProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateUserProfileResponse' {} a -> s {httpStatus = a} :: UpdateUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user in IAM.
updateUserProfileResponse_userArn :: Lens.Lens' UpdateUserProfileResponse Prelude.Text
updateUserProfileResponse_userArn = Lens.lens (\UpdateUserProfileResponse' {userArn} -> userArn) (\s@UpdateUserProfileResponse' {} a -> s {userArn = a} :: UpdateUserProfileResponse)

instance Prelude.NFData UpdateUserProfileResponse where
  rnf UpdateUserProfileResponse' {..} =
    Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf sshPublicKey
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf userArn
