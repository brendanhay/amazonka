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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | A collection of settings.
    userSettings :: Prelude.Maybe UserSettings,
    -- | Each tag consists of a key and an optional value. Tag keys must be
    -- unique per resource.
    tags :: Prelude.Maybe [Tag],
    -- | A specifier for the type of value specified in SingleSignOnUserValue.
    -- Currently, the only supported value is \"UserName\". If the Domain\'s
    -- AuthMode is SSO, this field is required. If the Domain\'s AuthMode is
    -- not SSO, this field cannot be specified.
    singleSignOnUserIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The username of the associated AWS Single Sign-On User for this
    -- UserProfile. If the Domain\'s AuthMode is SSO, this field is required,
    -- and must match a valid username of a user in your directory. If the
    -- Domain\'s AuthMode is not SSO, this field cannot be specified.
    singleSignOnUserValue :: Prelude.Maybe Prelude.Text,
    -- | The ID of the associated Domain.
    domainId :: Prelude.Text,
    -- | A name for the UserProfile.
    userProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'userProfileName'
  Prelude.Text ->
  CreateUserProfile
newCreateUserProfile pDomainId_ pUserProfileName_ =
  CreateUserProfile'
    { userSettings = Prelude.Nothing,
      tags = Prelude.Nothing,
      singleSignOnUserIdentifier = Prelude.Nothing,
      singleSignOnUserValue = Prelude.Nothing,
      domainId = pDomainId_,
      userProfileName = pUserProfileName_
    }

-- | A collection of settings.
createUserProfile_userSettings :: Lens.Lens' CreateUserProfile (Prelude.Maybe UserSettings)
createUserProfile_userSettings = Lens.lens (\CreateUserProfile' {userSettings} -> userSettings) (\s@CreateUserProfile' {} a -> s {userSettings = a} :: CreateUserProfile)

-- | Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
createUserProfile_tags :: Lens.Lens' CreateUserProfile (Prelude.Maybe [Tag])
createUserProfile_tags = Lens.lens (\CreateUserProfile' {tags} -> tags) (\s@CreateUserProfile' {} a -> s {tags = a} :: CreateUserProfile) Prelude.. Lens.mapping Prelude._Coerce

-- | A specifier for the type of value specified in SingleSignOnUserValue.
-- Currently, the only supported value is \"UserName\". If the Domain\'s
-- AuthMode is SSO, this field is required. If the Domain\'s AuthMode is
-- not SSO, this field cannot be specified.
createUserProfile_singleSignOnUserIdentifier :: Lens.Lens' CreateUserProfile (Prelude.Maybe Prelude.Text)
createUserProfile_singleSignOnUserIdentifier = Lens.lens (\CreateUserProfile' {singleSignOnUserIdentifier} -> singleSignOnUserIdentifier) (\s@CreateUserProfile' {} a -> s {singleSignOnUserIdentifier = a} :: CreateUserProfile)

-- | The username of the associated AWS Single Sign-On User for this
-- UserProfile. If the Domain\'s AuthMode is SSO, this field is required,
-- and must match a valid username of a user in your directory. If the
-- Domain\'s AuthMode is not SSO, this field cannot be specified.
createUserProfile_singleSignOnUserValue :: Lens.Lens' CreateUserProfile (Prelude.Maybe Prelude.Text)
createUserProfile_singleSignOnUserValue = Lens.lens (\CreateUserProfile' {singleSignOnUserValue} -> singleSignOnUserValue) (\s@CreateUserProfile' {} a -> s {singleSignOnUserValue = a} :: CreateUserProfile)

-- | The ID of the associated Domain.
createUserProfile_domainId :: Lens.Lens' CreateUserProfile Prelude.Text
createUserProfile_domainId = Lens.lens (\CreateUserProfile' {domainId} -> domainId) (\s@CreateUserProfile' {} a -> s {domainId = a} :: CreateUserProfile)

-- | A name for the UserProfile.
createUserProfile_userProfileName :: Lens.Lens' CreateUserProfile Prelude.Text
createUserProfile_userProfileName = Lens.lens (\CreateUserProfile' {userProfileName} -> userProfileName) (\s@CreateUserProfile' {} a -> s {userProfileName = a} :: CreateUserProfile)

instance Prelude.AWSRequest CreateUserProfile where
  type Rs CreateUserProfile = CreateUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Prelude.<$> (x Prelude..?> "UserProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserProfile

instance Prelude.NFData CreateUserProfile

instance Prelude.ToHeaders CreateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateUserProfile" ::
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
          [ ("UserSettings" Prelude..=)
              Prelude.<$> userSettings,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("SingleSignOnUserIdentifier" Prelude..=)
              Prelude.<$> singleSignOnUserIdentifier,
            ("SingleSignOnUserValue" Prelude..=)
              Prelude.<$> singleSignOnUserValue,
            Prelude.Just ("DomainId" Prelude..= domainId),
            Prelude.Just
              ("UserProfileName" Prelude..= userProfileName)
          ]
      )

instance Prelude.ToPath CreateUserProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateUserProfileResponse
