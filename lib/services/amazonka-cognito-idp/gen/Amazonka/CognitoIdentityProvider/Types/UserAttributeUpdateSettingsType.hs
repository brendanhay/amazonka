{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserAttributeUpdateSettingsType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserAttributeUpdateSettingsType where

import Amazonka.CognitoIdentityProvider.Types.VerifiedAttributeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for updates to user attributes. These settings include the
-- property @AttributesRequireVerificationBeforeUpdate@, a user-pool
-- setting that tells Amazon Cognito how to handle changes to the value of
-- your users\' email address and phone number attributes. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html#user-pool-settings-verifications-verify-attribute-updates Verifying updates to email addresses and phone numbers>.
--
-- /See:/ 'newUserAttributeUpdateSettingsType' smart constructor.
data UserAttributeUpdateSettingsType = UserAttributeUpdateSettingsType'
  { -- | Requires that your user verifies their email address, phone number, or
    -- both before Amazon Cognito updates the value of that attribute. When you
    -- update a user attribute that has this option activated, Amazon Cognito
    -- sends a verification message to the new phone number or email address.
    -- Amazon Cognito doesn’t change the value of the attribute until your user
    -- responds to the verification message and confirms the new value.
    --
    -- You can verify an updated email address or phone number with a
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerifyUserAttribute.html VerifyUserAttribute>
    -- API request. You can also call the
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateUserAttributes.html UpdateUserAttributes>
    -- or
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>
    -- API and set @email_verified@ or @phone_number_verified@ to true.
    --
    -- When @AttributesRequireVerificationBeforeUpdate@ is false, your user
    -- pool doesn\'t require that your users verify attribute changes before
    -- Amazon Cognito updates them. In a user pool where
    -- @AttributesRequireVerificationBeforeUpdate@ is false, API operations
    -- that change attribute values can immediately update a user’s @email@ or
    -- @phone_number@ attribute.
    attributesRequireVerificationBeforeUpdate :: Prelude.Maybe [VerifiedAttributeType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserAttributeUpdateSettingsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributesRequireVerificationBeforeUpdate', 'userAttributeUpdateSettingsType_attributesRequireVerificationBeforeUpdate' - Requires that your user verifies their email address, phone number, or
-- both before Amazon Cognito updates the value of that attribute. When you
-- update a user attribute that has this option activated, Amazon Cognito
-- sends a verification message to the new phone number or email address.
-- Amazon Cognito doesn’t change the value of the attribute until your user
-- responds to the verification message and confirms the new value.
--
-- You can verify an updated email address or phone number with a
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerifyUserAttribute.html VerifyUserAttribute>
-- API request. You can also call the
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateUserAttributes.html UpdateUserAttributes>
-- or
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>
-- API and set @email_verified@ or @phone_number_verified@ to true.
--
-- When @AttributesRequireVerificationBeforeUpdate@ is false, your user
-- pool doesn\'t require that your users verify attribute changes before
-- Amazon Cognito updates them. In a user pool where
-- @AttributesRequireVerificationBeforeUpdate@ is false, API operations
-- that change attribute values can immediately update a user’s @email@ or
-- @phone_number@ attribute.
newUserAttributeUpdateSettingsType ::
  UserAttributeUpdateSettingsType
newUserAttributeUpdateSettingsType =
  UserAttributeUpdateSettingsType'
    { attributesRequireVerificationBeforeUpdate =
        Prelude.Nothing
    }

-- | Requires that your user verifies their email address, phone number, or
-- both before Amazon Cognito updates the value of that attribute. When you
-- update a user attribute that has this option activated, Amazon Cognito
-- sends a verification message to the new phone number or email address.
-- Amazon Cognito doesn’t change the value of the attribute until your user
-- responds to the verification message and confirms the new value.
--
-- You can verify an updated email address or phone number with a
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerifyUserAttribute.html VerifyUserAttribute>
-- API request. You can also call the
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateUserAttributes.html UpdateUserAttributes>
-- or
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>
-- API and set @email_verified@ or @phone_number_verified@ to true.
--
-- When @AttributesRequireVerificationBeforeUpdate@ is false, your user
-- pool doesn\'t require that your users verify attribute changes before
-- Amazon Cognito updates them. In a user pool where
-- @AttributesRequireVerificationBeforeUpdate@ is false, API operations
-- that change attribute values can immediately update a user’s @email@ or
-- @phone_number@ attribute.
userAttributeUpdateSettingsType_attributesRequireVerificationBeforeUpdate :: Lens.Lens' UserAttributeUpdateSettingsType (Prelude.Maybe [VerifiedAttributeType])
userAttributeUpdateSettingsType_attributesRequireVerificationBeforeUpdate = Lens.lens (\UserAttributeUpdateSettingsType' {attributesRequireVerificationBeforeUpdate} -> attributesRequireVerificationBeforeUpdate) (\s@UserAttributeUpdateSettingsType' {} a -> s {attributesRequireVerificationBeforeUpdate = a} :: UserAttributeUpdateSettingsType) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    UserAttributeUpdateSettingsType
  where
  parseJSON =
    Data.withObject
      "UserAttributeUpdateSettingsType"
      ( \x ->
          UserAttributeUpdateSettingsType'
            Prelude.<$> ( x
                            Data..:? "AttributesRequireVerificationBeforeUpdate"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    UserAttributeUpdateSettingsType
  where
  hashWithSalt
    _salt
    UserAttributeUpdateSettingsType' {..} =
      _salt
        `Prelude.hashWithSalt` attributesRequireVerificationBeforeUpdate

instance
  Prelude.NFData
    UserAttributeUpdateSettingsType
  where
  rnf UserAttributeUpdateSettingsType' {..} =
    Prelude.rnf
      attributesRequireVerificationBeforeUpdate

instance Data.ToJSON UserAttributeUpdateSettingsType where
  toJSON UserAttributeUpdateSettingsType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ( "AttributesRequireVerificationBeforeUpdate"
                Data..=
            )
              Prelude.<$> attributesRequireVerificationBeforeUpdate
          ]
      )
