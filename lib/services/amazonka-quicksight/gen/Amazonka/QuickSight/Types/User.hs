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
-- Module      : Amazonka.QuickSight.Types.User
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.IdentityType
import Amazonka.QuickSight.Types.UserRole

-- | A registered user of Amazon QuickSight.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The principal ID of the user.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The type of supported external login provider that provides identity to
    -- let the user federate into Amazon QuickSight with an associated IAM
    -- role. The type can be one of the following.
    --
    -- -   @COGNITO@: Amazon Cognito. The provider URL is
    --     cognito-identity.amazonaws.com.
    --
    -- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider.
    externalLoginFederationProviderType :: Prelude.Maybe Prelude.Text,
    -- | The active status of user. When you create an Amazon QuickSight user
    -- that’s not an IAM user or an Active Directory user, that user is
    -- inactive until they sign in and provide a password.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The user\'s email address.
    email :: Prelude.Maybe Prelude.Text,
    -- | The user\'s user name. This value is required if you are registering a
    -- user that will be managed in Amazon QuickSight. In the output, the value
    -- for @UserName@ is @N\/A@ when the value for @IdentityType@ is @IAM@ and
    -- the corresponding IAM user is deleted.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The URL of the external login provider.
    externalLoginFederationProviderUrl :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight role for the user. The user role can be one of the
    -- following:.
    --
    -- -   @READER@: A user who has read-only access to dashboards.
    --
    -- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
    --     and dashboards.
    --
    -- -   @ADMIN@: A user who is an author, who can also manage Amazon Amazon
    --     QuickSight settings.
    --
    -- -   @RESTRICTED_READER@: This role isn\'t currently available for use.
    --
    -- -   @RESTRICTED_AUTHOR@: This role isn\'t currently available for use.
    role' :: Prelude.Maybe UserRole,
    -- | The identity ID for the user in the external login provider.
    externalLoginId :: Prelude.Maybe Prelude.Text,
    -- | The type of identity authentication used by the user.
    identityType :: Prelude.Maybe IdentityType,
    -- | The custom permissions profile associated with this user.
    customPermissionsName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'user_principalId' - The principal ID of the user.
--
-- 'externalLoginFederationProviderType', 'user_externalLoginFederationProviderType' - The type of supported external login provider that provides identity to
-- let the user federate into Amazon QuickSight with an associated IAM
-- role. The type can be one of the following.
--
-- -   @COGNITO@: Amazon Cognito. The provider URL is
--     cognito-identity.amazonaws.com.
--
-- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider.
--
-- 'active', 'user_active' - The active status of user. When you create an Amazon QuickSight user
-- that’s not an IAM user or an Active Directory user, that user is
-- inactive until they sign in and provide a password.
--
-- 'email', 'user_email' - The user\'s email address.
--
-- 'userName', 'user_userName' - The user\'s user name. This value is required if you are registering a
-- user that will be managed in Amazon QuickSight. In the output, the value
-- for @UserName@ is @N\/A@ when the value for @IdentityType@ is @IAM@ and
-- the corresponding IAM user is deleted.
--
-- 'externalLoginFederationProviderUrl', 'user_externalLoginFederationProviderUrl' - The URL of the external login provider.
--
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) for the user.
--
-- 'role'', 'user_role' - The Amazon QuickSight role for the user. The user role can be one of the
-- following:.
--
-- -   @READER@: A user who has read-only access to dashboards.
--
-- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
--     and dashboards.
--
-- -   @ADMIN@: A user who is an author, who can also manage Amazon Amazon
--     QuickSight settings.
--
-- -   @RESTRICTED_READER@: This role isn\'t currently available for use.
--
-- -   @RESTRICTED_AUTHOR@: This role isn\'t currently available for use.
--
-- 'externalLoginId', 'user_externalLoginId' - The identity ID for the user in the external login provider.
--
-- 'identityType', 'user_identityType' - The type of identity authentication used by the user.
--
-- 'customPermissionsName', 'user_customPermissionsName' - The custom permissions profile associated with this user.
newUser ::
  User
newUser =
  User'
    { principalId = Prelude.Nothing,
      externalLoginFederationProviderType =
        Prelude.Nothing,
      active = Prelude.Nothing,
      email = Prelude.Nothing,
      userName = Prelude.Nothing,
      externalLoginFederationProviderUrl = Prelude.Nothing,
      arn = Prelude.Nothing,
      role' = Prelude.Nothing,
      externalLoginId = Prelude.Nothing,
      identityType = Prelude.Nothing,
      customPermissionsName = Prelude.Nothing
    }

-- | The principal ID of the user.
user_principalId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_principalId = Lens.lens (\User' {principalId} -> principalId) (\s@User' {} a -> s {principalId = a} :: User)

-- | The type of supported external login provider that provides identity to
-- let the user federate into Amazon QuickSight with an associated IAM
-- role. The type can be one of the following.
--
-- -   @COGNITO@: Amazon Cognito. The provider URL is
--     cognito-identity.amazonaws.com.
--
-- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider.
user_externalLoginFederationProviderType :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_externalLoginFederationProviderType = Lens.lens (\User' {externalLoginFederationProviderType} -> externalLoginFederationProviderType) (\s@User' {} a -> s {externalLoginFederationProviderType = a} :: User)

-- | The active status of user. When you create an Amazon QuickSight user
-- that’s not an IAM user or an Active Directory user, that user is
-- inactive until they sign in and provide a password.
user_active :: Lens.Lens' User (Prelude.Maybe Prelude.Bool)
user_active = Lens.lens (\User' {active} -> active) (\s@User' {} a -> s {active = a} :: User)

-- | The user\'s email address.
user_email :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_email = Lens.lens (\User' {email} -> email) (\s@User' {} a -> s {email = a} :: User)

-- | The user\'s user name. This value is required if you are registering a
-- user that will be managed in Amazon QuickSight. In the output, the value
-- for @UserName@ is @N\/A@ when the value for @IdentityType@ is @IAM@ and
-- the corresponding IAM user is deleted.
user_userName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User)

-- | The URL of the external login provider.
user_externalLoginFederationProviderUrl :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_externalLoginFederationProviderUrl = Lens.lens (\User' {externalLoginFederationProviderUrl} -> externalLoginFederationProviderUrl) (\s@User' {} a -> s {externalLoginFederationProviderUrl = a} :: User)

-- | The Amazon Resource Name (ARN) for the user.
user_arn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | The Amazon QuickSight role for the user. The user role can be one of the
-- following:.
--
-- -   @READER@: A user who has read-only access to dashboards.
--
-- -   @AUTHOR@: A user who can create data sources, datasets, analyses,
--     and dashboards.
--
-- -   @ADMIN@: A user who is an author, who can also manage Amazon Amazon
--     QuickSight settings.
--
-- -   @RESTRICTED_READER@: This role isn\'t currently available for use.
--
-- -   @RESTRICTED_AUTHOR@: This role isn\'t currently available for use.
user_role :: Lens.Lens' User (Prelude.Maybe UserRole)
user_role = Lens.lens (\User' {role'} -> role') (\s@User' {} a -> s {role' = a} :: User)

-- | The identity ID for the user in the external login provider.
user_externalLoginId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_externalLoginId = Lens.lens (\User' {externalLoginId} -> externalLoginId) (\s@User' {} a -> s {externalLoginId = a} :: User)

-- | The type of identity authentication used by the user.
user_identityType :: Lens.Lens' User (Prelude.Maybe IdentityType)
user_identityType = Lens.lens (\User' {identityType} -> identityType) (\s@User' {} a -> s {identityType = a} :: User)

-- | The custom permissions profile associated with this user.
user_customPermissionsName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_customPermissionsName = Lens.lens (\User' {customPermissionsName} -> customPermissionsName) (\s@User' {} a -> s {customPermissionsName = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "PrincipalId")
            Prelude.<*> (x Data..:? "ExternalLoginFederationProviderType")
            Prelude.<*> (x Data..:? "Active")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "UserName")
            Prelude.<*> (x Data..:? "ExternalLoginFederationProviderUrl")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "ExternalLoginId")
            Prelude.<*> (x Data..:? "IdentityType")
            Prelude.<*> (x Data..:? "CustomPermissionsName")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` externalLoginFederationProviderType
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` externalLoginFederationProviderUrl
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` externalLoginId
      `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` customPermissionsName

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf externalLoginFederationProviderType
      `Prelude.seq` Prelude.rnf active
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf externalLoginFederationProviderUrl
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf externalLoginId
      `Prelude.seq` Prelude.rnf identityType
      `Prelude.seq` Prelude.rnf customPermissionsName
