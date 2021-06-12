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
-- Module      : Network.AWS.SageMaker.CreateUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user profile. A user profile represents a single user within a
-- domain, and is the main way to reference a \"person\" for the purposes
-- of sharing, reporting, and other user-oriented features. This entity is
-- created when a user onboards to Amazon SageMaker Studio. If an
-- administrator invites a person by email or imports them from SSO, a user
-- profile is automatically created. A user profile is the primary holder
-- of settings for an individual user and has a reference to the user\'s
-- private Amazon Elastic File System (EFS) home directory.
module Network.AWS.SageMaker.CreateUserProfile
  ( -- * Creating a Request
    CreateUserProfile (..),
    newCreateUserProfile,

    -- * Request Lenses
    createUserProfile_userSettings,
    createUserProfile_tags,
    createUserProfile_singleSignOnUserIdentifier,
    createUserProfile_singleSignOnUserValue,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | A collection of settings.
    userSettings :: Core.Maybe UserSettings,
    -- | Each tag consists of a key and an optional value. Tag keys must be
    -- unique per resource.
    tags :: Core.Maybe [Tag],
    -- | A specifier for the type of value specified in SingleSignOnUserValue.
    -- Currently, the only supported value is \"UserName\". If the Domain\'s
    -- AuthMode is SSO, this field is required. If the Domain\'s AuthMode is
    -- not SSO, this field cannot be specified.
    singleSignOnUserIdentifier :: Core.Maybe Core.Text,
    -- | The username of the associated AWS Single Sign-On User for this
    -- UserProfile. If the Domain\'s AuthMode is SSO, this field is required,
    -- and must match a valid username of a user in your directory. If the
    -- Domain\'s AuthMode is not SSO, this field cannot be specified.
    singleSignOnUserValue :: Core.Maybe Core.Text,
    -- | The ID of the associated Domain.
    domainId :: Core.Text,
    -- | A name for the UserProfile.
    userProfileName :: Core.Text
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
-- 'userSettings', 'createUserProfile_userSettings' - A collection of settings.
--
-- 'tags', 'createUserProfile_tags' - Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
--
-- 'singleSignOnUserIdentifier', 'createUserProfile_singleSignOnUserIdentifier' - A specifier for the type of value specified in SingleSignOnUserValue.
-- Currently, the only supported value is \"UserName\". If the Domain\'s
-- AuthMode is SSO, this field is required. If the Domain\'s AuthMode is
-- not SSO, this field cannot be specified.
--
-- 'singleSignOnUserValue', 'createUserProfile_singleSignOnUserValue' - The username of the associated AWS Single Sign-On User for this
-- UserProfile. If the Domain\'s AuthMode is SSO, this field is required,
-- and must match a valid username of a user in your directory. If the
-- Domain\'s AuthMode is not SSO, this field cannot be specified.
--
-- 'domainId', 'createUserProfile_domainId' - The ID of the associated Domain.
--
-- 'userProfileName', 'createUserProfile_userProfileName' - A name for the UserProfile.
newCreateUserProfile ::
  -- | 'domainId'
  Core.Text ->
  -- | 'userProfileName'
  Core.Text ->
  CreateUserProfile
newCreateUserProfile pDomainId_ pUserProfileName_ =
  CreateUserProfile'
    { userSettings = Core.Nothing,
      tags = Core.Nothing,
      singleSignOnUserIdentifier = Core.Nothing,
      singleSignOnUserValue = Core.Nothing,
      domainId = pDomainId_,
      userProfileName = pUserProfileName_
    }

-- | A collection of settings.
createUserProfile_userSettings :: Lens.Lens' CreateUserProfile (Core.Maybe UserSettings)
createUserProfile_userSettings = Lens.lens (\CreateUserProfile' {userSettings} -> userSettings) (\s@CreateUserProfile' {} a -> s {userSettings = a} :: CreateUserProfile)

-- | Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
createUserProfile_tags :: Lens.Lens' CreateUserProfile (Core.Maybe [Tag])
createUserProfile_tags = Lens.lens (\CreateUserProfile' {tags} -> tags) (\s@CreateUserProfile' {} a -> s {tags = a} :: CreateUserProfile) Core.. Lens.mapping Lens._Coerce

-- | A specifier for the type of value specified in SingleSignOnUserValue.
-- Currently, the only supported value is \"UserName\". If the Domain\'s
-- AuthMode is SSO, this field is required. If the Domain\'s AuthMode is
-- not SSO, this field cannot be specified.
createUserProfile_singleSignOnUserIdentifier :: Lens.Lens' CreateUserProfile (Core.Maybe Core.Text)
createUserProfile_singleSignOnUserIdentifier = Lens.lens (\CreateUserProfile' {singleSignOnUserIdentifier} -> singleSignOnUserIdentifier) (\s@CreateUserProfile' {} a -> s {singleSignOnUserIdentifier = a} :: CreateUserProfile)

-- | The username of the associated AWS Single Sign-On User for this
-- UserProfile. If the Domain\'s AuthMode is SSO, this field is required,
-- and must match a valid username of a user in your directory. If the
-- Domain\'s AuthMode is not SSO, this field cannot be specified.
createUserProfile_singleSignOnUserValue :: Lens.Lens' CreateUserProfile (Core.Maybe Core.Text)
createUserProfile_singleSignOnUserValue = Lens.lens (\CreateUserProfile' {singleSignOnUserValue} -> singleSignOnUserValue) (\s@CreateUserProfile' {} a -> s {singleSignOnUserValue = a} :: CreateUserProfile)

-- | The ID of the associated Domain.
createUserProfile_domainId :: Lens.Lens' CreateUserProfile Core.Text
createUserProfile_domainId = Lens.lens (\CreateUserProfile' {domainId} -> domainId) (\s@CreateUserProfile' {} a -> s {domainId = a} :: CreateUserProfile)

-- | A name for the UserProfile.
createUserProfile_userProfileName :: Lens.Lens' CreateUserProfile Core.Text
createUserProfile_userProfileName = Lens.lens (\CreateUserProfile' {userProfileName} -> userProfileName) (\s@CreateUserProfile' {} a -> s {userProfileName = a} :: CreateUserProfile)

instance Core.AWSRequest CreateUserProfile where
  type
    AWSResponse CreateUserProfile =
      CreateUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Core.<$> (x Core..?> "UserProfileArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUserProfile

instance Core.NFData CreateUserProfile

instance Core.ToHeaders CreateUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateUserProfile" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUserProfile where
  toJSON CreateUserProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UserSettings" Core..=) Core.<$> userSettings,
            ("Tags" Core..=) Core.<$> tags,
            ("SingleSignOnUserIdentifier" Core..=)
              Core.<$> singleSignOnUserIdentifier,
            ("SingleSignOnUserValue" Core..=)
              Core.<$> singleSignOnUserValue,
            Core.Just ("DomainId" Core..= domainId),
            Core.Just
              ("UserProfileName" Core..= userProfileName)
          ]
      )

instance Core.ToPath CreateUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery CreateUserProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Core.Maybe Core.Text,
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
-- 'userProfileArn', 'createUserProfileResponse_userProfileArn' - The user profile Amazon Resource Name (ARN).
--
-- 'httpStatus', 'createUserProfileResponse_httpStatus' - The response's http status code.
newCreateUserProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserProfileResponse
newCreateUserProfileResponse pHttpStatus_ =
  CreateUserProfileResponse'
    { userProfileArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user profile Amazon Resource Name (ARN).
createUserProfileResponse_userProfileArn :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.Text)
createUserProfileResponse_userProfileArn = Lens.lens (\CreateUserProfileResponse' {userProfileArn} -> userProfileArn) (\s@CreateUserProfileResponse' {} a -> s {userProfileArn = a} :: CreateUserProfileResponse)

-- | The response's http status code.
createUserProfileResponse_httpStatus :: Lens.Lens' CreateUserProfileResponse Core.Int
createUserProfileResponse_httpStatus = Lens.lens (\CreateUserProfileResponse' {httpStatus} -> httpStatus) (\s@CreateUserProfileResponse' {} a -> s {httpStatus = a} :: CreateUserProfileResponse)

instance Core.NFData CreateUserProfileResponse
