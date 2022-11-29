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
-- Module      : Amazonka.SageMaker.CreateUserProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user profile. A user profile represents a single user within a
-- domain, and is the main way to reference a \"person\" for the purposes
-- of sharing, reporting, and other user-oriented features. This entity is
-- created when a user onboards to Amazon SageMaker Studio. If an
-- administrator invites a person by email or imports them from IAM
-- Identity Center, a user profile is automatically created. A user profile
-- is the primary holder of settings for an individual user and has a
-- reference to the user\'s private Amazon Elastic File System (EFS) home
-- directory.
module Amazonka.SageMaker.CreateUserProfile
  ( -- * Creating a Request
    CreateUserProfile (..),
    newCreateUserProfile,

    -- * Request Lenses
    createUserProfile_tags,
    createUserProfile_singleSignOnUserValue,
    createUserProfile_userSettings,
    createUserProfile_singleSignOnUserIdentifier,
    createUserProfile_domainId,
    createUserProfile_userProfileName,

    -- * Destructuring the Response
    CreateUserProfileResponse (..),
    newCreateUserProfileResponse,

    -- * Response Lenses
    createUserProfileResponse_userProfileArn,
    createUserProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | Each tag consists of a key and an optional value. Tag keys must be
    -- unique per resource.
    --
    -- Tags that you specify for the User Profile are also added to all Apps
    -- that the User Profile launches.
    tags :: Prelude.Maybe [Tag],
    -- | The username of the associated Amazon Web Services Single Sign-On User
    -- for this UserProfile. If the Domain\'s AuthMode is IAM Identity Center,
    -- this field is required, and must match a valid username of a user in
    -- your directory. If the Domain\'s AuthMode is not IAM Identity Center,
    -- this field cannot be specified.
    singleSignOnUserValue :: Prelude.Maybe Prelude.Text,
    -- | A collection of settings.
    userSettings :: Prelude.Maybe UserSettings,
    -- | A specifier for the type of value specified in SingleSignOnUserValue.
    -- Currently, the only supported value is \"UserName\". If the Domain\'s
    -- AuthMode is IAM Identity Center, this field is required. If the
    -- Domain\'s AuthMode is not IAM Identity Center, this field cannot be
    -- specified.
    singleSignOnUserIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ID of the associated Domain.
    domainId :: Prelude.Text,
    -- | A name for the UserProfile. This value is not case sensitive.
    userProfileName :: Prelude.Text
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
-- 'tags', 'createUserProfile_tags' - Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
--
-- Tags that you specify for the User Profile are also added to all Apps
-- that the User Profile launches.
--
-- 'singleSignOnUserValue', 'createUserProfile_singleSignOnUserValue' - The username of the associated Amazon Web Services Single Sign-On User
-- for this UserProfile. If the Domain\'s AuthMode is IAM Identity Center,
-- this field is required, and must match a valid username of a user in
-- your directory. If the Domain\'s AuthMode is not IAM Identity Center,
-- this field cannot be specified.
--
-- 'userSettings', 'createUserProfile_userSettings' - A collection of settings.
--
-- 'singleSignOnUserIdentifier', 'createUserProfile_singleSignOnUserIdentifier' - A specifier for the type of value specified in SingleSignOnUserValue.
-- Currently, the only supported value is \"UserName\". If the Domain\'s
-- AuthMode is IAM Identity Center, this field is required. If the
-- Domain\'s AuthMode is not IAM Identity Center, this field cannot be
-- specified.
--
-- 'domainId', 'createUserProfile_domainId' - The ID of the associated Domain.
--
-- 'userProfileName', 'createUserProfile_userProfileName' - A name for the UserProfile. This value is not case sensitive.
newCreateUserProfile ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'userProfileName'
  Prelude.Text ->
  CreateUserProfile
newCreateUserProfile pDomainId_ pUserProfileName_ =
  CreateUserProfile'
    { tags = Prelude.Nothing,
      singleSignOnUserValue = Prelude.Nothing,
      userSettings = Prelude.Nothing,
      singleSignOnUserIdentifier = Prelude.Nothing,
      domainId = pDomainId_,
      userProfileName = pUserProfileName_
    }

-- | Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
--
-- Tags that you specify for the User Profile are also added to all Apps
-- that the User Profile launches.
createUserProfile_tags :: Lens.Lens' CreateUserProfile (Prelude.Maybe [Tag])
createUserProfile_tags = Lens.lens (\CreateUserProfile' {tags} -> tags) (\s@CreateUserProfile' {} a -> s {tags = a} :: CreateUserProfile) Prelude.. Lens.mapping Lens.coerced

-- | The username of the associated Amazon Web Services Single Sign-On User
-- for this UserProfile. If the Domain\'s AuthMode is IAM Identity Center,
-- this field is required, and must match a valid username of a user in
-- your directory. If the Domain\'s AuthMode is not IAM Identity Center,
-- this field cannot be specified.
createUserProfile_singleSignOnUserValue :: Lens.Lens' CreateUserProfile (Prelude.Maybe Prelude.Text)
createUserProfile_singleSignOnUserValue = Lens.lens (\CreateUserProfile' {singleSignOnUserValue} -> singleSignOnUserValue) (\s@CreateUserProfile' {} a -> s {singleSignOnUserValue = a} :: CreateUserProfile)

-- | A collection of settings.
createUserProfile_userSettings :: Lens.Lens' CreateUserProfile (Prelude.Maybe UserSettings)
createUserProfile_userSettings = Lens.lens (\CreateUserProfile' {userSettings} -> userSettings) (\s@CreateUserProfile' {} a -> s {userSettings = a} :: CreateUserProfile)

-- | A specifier for the type of value specified in SingleSignOnUserValue.
-- Currently, the only supported value is \"UserName\". If the Domain\'s
-- AuthMode is IAM Identity Center, this field is required. If the
-- Domain\'s AuthMode is not IAM Identity Center, this field cannot be
-- specified.
createUserProfile_singleSignOnUserIdentifier :: Lens.Lens' CreateUserProfile (Prelude.Maybe Prelude.Text)
createUserProfile_singleSignOnUserIdentifier = Lens.lens (\CreateUserProfile' {singleSignOnUserIdentifier} -> singleSignOnUserIdentifier) (\s@CreateUserProfile' {} a -> s {singleSignOnUserIdentifier = a} :: CreateUserProfile)

-- | The ID of the associated Domain.
createUserProfile_domainId :: Lens.Lens' CreateUserProfile Prelude.Text
createUserProfile_domainId = Lens.lens (\CreateUserProfile' {domainId} -> domainId) (\s@CreateUserProfile' {} a -> s {domainId = a} :: CreateUserProfile)

-- | A name for the UserProfile. This value is not case sensitive.
createUserProfile_userProfileName :: Lens.Lens' CreateUserProfile Prelude.Text
createUserProfile_userProfileName = Lens.lens (\CreateUserProfile' {userProfileName} -> userProfileName) (\s@CreateUserProfile' {} a -> s {userProfileName = a} :: CreateUserProfile)

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
            Prelude.<$> (x Core..?> "UserProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserProfile where
  hashWithSalt _salt CreateUserProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` singleSignOnUserValue
      `Prelude.hashWithSalt` userSettings
      `Prelude.hashWithSalt` singleSignOnUserIdentifier
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` userProfileName

instance Prelude.NFData CreateUserProfile where
  rnf CreateUserProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf singleSignOnUserValue
      `Prelude.seq` Prelude.rnf userSettings
      `Prelude.seq` Prelude.rnf singleSignOnUserIdentifier
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf userProfileName

instance Core.ToHeaders CreateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUserProfile where
  toJSON CreateUserProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("SingleSignOnUserValue" Core..=)
              Prelude.<$> singleSignOnUserValue,
            ("UserSettings" Core..=) Prelude.<$> userSettings,
            ("SingleSignOnUserIdentifier" Core..=)
              Prelude.<$> singleSignOnUserIdentifier,
            Prelude.Just ("DomainId" Core..= domainId),
            Prelude.Just
              ("UserProfileName" Core..= userProfileName)
          ]
      )

instance Core.ToPath CreateUserProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Prelude.Maybe Prelude.Text,
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
-- 'userProfileArn', 'createUserProfileResponse_userProfileArn' - The user profile Amazon Resource Name (ARN).
--
-- 'httpStatus', 'createUserProfileResponse_httpStatus' - The response's http status code.
newCreateUserProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserProfileResponse
newCreateUserProfileResponse pHttpStatus_ =
  CreateUserProfileResponse'
    { userProfileArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user profile Amazon Resource Name (ARN).
createUserProfileResponse_userProfileArn :: Lens.Lens' CreateUserProfileResponse (Prelude.Maybe Prelude.Text)
createUserProfileResponse_userProfileArn = Lens.lens (\CreateUserProfileResponse' {userProfileArn} -> userProfileArn) (\s@CreateUserProfileResponse' {} a -> s {userProfileArn = a} :: CreateUserProfileResponse)

-- | The response's http status code.
createUserProfileResponse_httpStatus :: Lens.Lens' CreateUserProfileResponse Prelude.Int
createUserProfileResponse_httpStatus = Lens.lens (\CreateUserProfileResponse' {httpStatus} -> httpStatus) (\s@CreateUserProfileResponse' {} a -> s {httpStatus = a} :: CreateUserProfileResponse)

instance Prelude.NFData CreateUserProfileResponse where
  rnf CreateUserProfileResponse' {..} =
    Prelude.rnf userProfileArn
      `Prelude.seq` Prelude.rnf httpStatus
