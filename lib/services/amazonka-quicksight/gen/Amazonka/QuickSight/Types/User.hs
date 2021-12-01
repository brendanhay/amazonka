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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.IdentityType
import Amazonka.QuickSight.Types.UserRole

-- | A registered user of Amazon QuickSight.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The user\'s email address.
    email :: Prelude.Maybe Prelude.Text,
    -- | The principal ID of the user.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The user\'s user name.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The type of identity authentication used by the user.
    identityType :: Prelude.Maybe IdentityType,
    -- | The URL of the external login provider.
    externalLoginFederationProviderUrl :: Prelude.Maybe Prelude.Text,
    -- | The custom permissions profile associated with this user.
    customPermissionsName :: Prelude.Maybe Prelude.Text,
    -- | The active status of user. When you create an Amazon QuickSight user
    -- that’s not an IAM user or an Active Directory user, that user is
    -- inactive until they sign in and provide a password.
    active :: Prelude.Maybe Prelude.Bool,
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
    -- | The type of supported external login provider that provides identity to
    -- let the user federate into Amazon QuickSight with an associated IAMrole.
    -- The type can be one of the following.
    --
    -- -   @COGNITO@: Amazon Cognito. The provider URL is
    --     cognito-identity.amazonaws.com.
    --
    -- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider.
    externalLoginFederationProviderType :: Prelude.Maybe Prelude.Text,
    -- | The identity ID for the user in the external login provider.
    externalLoginId :: Prelude.Maybe Prelude.Text
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
-- 'email', 'user_email' - The user\'s email address.
--
-- 'principalId', 'user_principalId' - The principal ID of the user.
--
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) for the user.
--
-- 'userName', 'user_userName' - The user\'s user name.
--
-- 'identityType', 'user_identityType' - The type of identity authentication used by the user.
--
-- 'externalLoginFederationProviderUrl', 'user_externalLoginFederationProviderUrl' - The URL of the external login provider.
--
-- 'customPermissionsName', 'user_customPermissionsName' - The custom permissions profile associated with this user.
--
-- 'active', 'user_active' - The active status of user. When you create an Amazon QuickSight user
-- that’s not an IAM user or an Active Directory user, that user is
-- inactive until they sign in and provide a password.
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
-- 'externalLoginFederationProviderType', 'user_externalLoginFederationProviderType' - The type of supported external login provider that provides identity to
-- let the user federate into Amazon QuickSight with an associated IAMrole.
-- The type can be one of the following.
--
-- -   @COGNITO@: Amazon Cognito. The provider URL is
--     cognito-identity.amazonaws.com.
--
-- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider.
--
-- 'externalLoginId', 'user_externalLoginId' - The identity ID for the user in the external login provider.
newUser ::
  User
newUser =
  User'
    { email = Prelude.Nothing,
      principalId = Prelude.Nothing,
      arn = Prelude.Nothing,
      userName = Prelude.Nothing,
      identityType = Prelude.Nothing,
      externalLoginFederationProviderUrl = Prelude.Nothing,
      customPermissionsName = Prelude.Nothing,
      active = Prelude.Nothing,
      role' = Prelude.Nothing,
      externalLoginFederationProviderType =
        Prelude.Nothing,
      externalLoginId = Prelude.Nothing
    }

-- | The user\'s email address.
user_email :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_email = Lens.lens (\User' {email} -> email) (\s@User' {} a -> s {email = a} :: User)

-- | The principal ID of the user.
user_principalId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_principalId = Lens.lens (\User' {principalId} -> principalId) (\s@User' {} a -> s {principalId = a} :: User)

-- | The Amazon Resource Name (ARN) for the user.
user_arn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | The user\'s user name.
user_userName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User)

-- | The type of identity authentication used by the user.
user_identityType :: Lens.Lens' User (Prelude.Maybe IdentityType)
user_identityType = Lens.lens (\User' {identityType} -> identityType) (\s@User' {} a -> s {identityType = a} :: User)

-- | The URL of the external login provider.
user_externalLoginFederationProviderUrl :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_externalLoginFederationProviderUrl = Lens.lens (\User' {externalLoginFederationProviderUrl} -> externalLoginFederationProviderUrl) (\s@User' {} a -> s {externalLoginFederationProviderUrl = a} :: User)

-- | The custom permissions profile associated with this user.
user_customPermissionsName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_customPermissionsName = Lens.lens (\User' {customPermissionsName} -> customPermissionsName) (\s@User' {} a -> s {customPermissionsName = a} :: User)

-- | The active status of user. When you create an Amazon QuickSight user
-- that’s not an IAM user or an Active Directory user, that user is
-- inactive until they sign in and provide a password.
user_active :: Lens.Lens' User (Prelude.Maybe Prelude.Bool)
user_active = Lens.lens (\User' {active} -> active) (\s@User' {} a -> s {active = a} :: User)

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

-- | The type of supported external login provider that provides identity to
-- let the user federate into Amazon QuickSight with an associated IAMrole.
-- The type can be one of the following.
--
-- -   @COGNITO@: Amazon Cognito. The provider URL is
--     cognito-identity.amazonaws.com.
--
-- -   @CUSTOM_OIDC@: Custom OpenID Connect (OIDC) provider.
user_externalLoginFederationProviderType :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_externalLoginFederationProviderType = Lens.lens (\User' {externalLoginFederationProviderType} -> externalLoginFederationProviderType) (\s@User' {} a -> s {externalLoginFederationProviderType = a} :: User)

-- | The identity ID for the user in the external login provider.
user_externalLoginId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_externalLoginId = Lens.lens (\User' {externalLoginId} -> externalLoginId) (\s@User' {} a -> s {externalLoginId = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Core..:? "Email")
            Prelude.<*> (x Core..:? "PrincipalId")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "UserName")
            Prelude.<*> (x Core..:? "IdentityType")
            Prelude.<*> (x Core..:? "ExternalLoginFederationProviderUrl")
            Prelude.<*> (x Core..:? "CustomPermissionsName")
            Prelude.<*> (x Core..:? "Active")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "ExternalLoginFederationProviderType")
            Prelude.<*> (x Core..:? "ExternalLoginId")
      )

instance Prelude.Hashable User where
  hashWithSalt salt' User' {..} =
    salt' `Prelude.hashWithSalt` externalLoginId
      `Prelude.hashWithSalt` externalLoginFederationProviderType
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` customPermissionsName
      `Prelude.hashWithSalt` externalLoginFederationProviderUrl
      `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` email

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf email
      `Prelude.seq` Prelude.rnf externalLoginId
      `Prelude.seq` Prelude.rnf externalLoginFederationProviderType
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf active
      `Prelude.seq` Prelude.rnf customPermissionsName
      `Prelude.seq` Prelude.rnf externalLoginFederationProviderUrl
      `Prelude.seq` Prelude.rnf identityType
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf principalId
