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
-- Module      : Amazonka.IAM.UpdateAccountPasswordPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the password policy settings for the Amazon Web Services
-- account.
--
-- This operation does not support partial updates. No parameters are
-- required, but if you do not specify a parameter, that parameter\'s value
-- reverts to its default value. See the __Request Parameters__ section for
-- each parameter\'s default value. Also note that some parameters do not
-- allow the default parameter to be explicitly set. Instead, to invoke the
-- default value, do not include that parameter when you invoke the
-- operation.
--
-- For more information about using a password policy, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM password policy>
-- in the /IAM User Guide/.
module Amazonka.IAM.UpdateAccountPasswordPolicy
  ( -- * Creating a Request
    UpdateAccountPasswordPolicy (..),
    newUpdateAccountPasswordPolicy,

    -- * Request Lenses
    updateAccountPasswordPolicy_allowUsersToChangePassword,
    updateAccountPasswordPolicy_hardExpiry,
    updateAccountPasswordPolicy_maxPasswordAge,
    updateAccountPasswordPolicy_minimumPasswordLength,
    updateAccountPasswordPolicy_passwordReusePrevention,
    updateAccountPasswordPolicy_requireLowercaseCharacters,
    updateAccountPasswordPolicy_requireNumbers,
    updateAccountPasswordPolicy_requireSymbols,
    updateAccountPasswordPolicy_requireUppercaseCharacters,

    -- * Destructuring the Response
    UpdateAccountPasswordPolicyResponse (..),
    newUpdateAccountPasswordPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccountPasswordPolicy' smart constructor.
data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy'
  { -- | Allows all IAM users in your account to use the Amazon Web Services
    -- Management Console to change their own passwords. For more information,
    -- see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_passwords_enable-user-change.html Permitting IAM users to change their own passwords>
    -- in the /IAM User Guide/.
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @false@. The result is that IAM users in the
    -- account do not automatically have permissions to change their own
    -- password.
    allowUsersToChangePassword :: Prelude.Maybe Prelude.Bool,
    -- | Prevents IAM users who are accessing the account via the Amazon Web
    -- Services Management Console from setting a new console password after
    -- their password has expired. The IAM user cannot access the console until
    -- an administrator resets the password.
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @false@. The result is that IAM users can
    -- change their passwords after they expire and continue to sign in as the
    -- user.
    --
    -- In the Amazon Web Services Management Console, the custom password
    -- policy option __Allow users to change their own password__ gives IAM
    -- users permissions to @iam:ChangePassword@ for only their user and to the
    -- @iam:GetAccountPasswordPolicy@ action. This option does not attach a
    -- permissions policy to each user, rather the permissions are applied at
    -- the account-level for all users by IAM. IAM users with
    -- @iam:ChangePassword@ permission and active access keys can reset their
    -- own expired console password using the CLI or API.
    hardExpiry :: Prelude.Maybe Prelude.Bool,
    -- | The number of days that an IAM user password is valid.
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @0@. The result is that IAM user passwords
    -- never expire.
    maxPasswordAge :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of characters allowed in an IAM user password.
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @6@.
    minimumPasswordLength :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the number of previous passwords that IAM users are prevented
    -- from reusing.
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @0@. The result is that IAM users are not
    -- prevented from reusing previous passwords.
    passwordReusePrevention :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether IAM user passwords must contain at least one lowercase
    -- character from the ISO basic Latin alphabet (a to z).
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @false@. The result is that passwords do not
    -- require at least one lowercase character.
    requireLowercaseCharacters :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one numeric
    -- character (0 to 9).
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @false@. The result is that passwords do not
    -- require at least one numeric character.
    requireNumbers :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one of the
    -- following non-alphanumeric characters:
    --
    -- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @false@. The result is that passwords do not
    -- require at least one symbol character.
    requireSymbols :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one uppercase
    -- character from the ISO basic Latin alphabet (A to Z).
    --
    -- If you do not specify a value for this parameter, then the operation
    -- uses the default value of @false@. The result is that passwords do not
    -- require at least one uppercase character.
    requireUppercaseCharacters :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountPasswordPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowUsersToChangePassword', 'updateAccountPasswordPolicy_allowUsersToChangePassword' - Allows all IAM users in your account to use the Amazon Web Services
-- Management Console to change their own passwords. For more information,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_passwords_enable-user-change.html Permitting IAM users to change their own passwords>
-- in the /IAM User Guide/.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that IAM users in the
-- account do not automatically have permissions to change their own
-- password.
--
-- 'hardExpiry', 'updateAccountPasswordPolicy_hardExpiry' - Prevents IAM users who are accessing the account via the Amazon Web
-- Services Management Console from setting a new console password after
-- their password has expired. The IAM user cannot access the console until
-- an administrator resets the password.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that IAM users can
-- change their passwords after they expire and continue to sign in as the
-- user.
--
-- In the Amazon Web Services Management Console, the custom password
-- policy option __Allow users to change their own password__ gives IAM
-- users permissions to @iam:ChangePassword@ for only their user and to the
-- @iam:GetAccountPasswordPolicy@ action. This option does not attach a
-- permissions policy to each user, rather the permissions are applied at
-- the account-level for all users by IAM. IAM users with
-- @iam:ChangePassword@ permission and active access keys can reset their
-- own expired console password using the CLI or API.
--
-- 'maxPasswordAge', 'updateAccountPasswordPolicy_maxPasswordAge' - The number of days that an IAM user password is valid.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @0@. The result is that IAM user passwords
-- never expire.
--
-- 'minimumPasswordLength', 'updateAccountPasswordPolicy_minimumPasswordLength' - The minimum number of characters allowed in an IAM user password.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @6@.
--
-- 'passwordReusePrevention', 'updateAccountPasswordPolicy_passwordReusePrevention' - Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @0@. The result is that IAM users are not
-- prevented from reusing previous passwords.
--
-- 'requireLowercaseCharacters', 'updateAccountPasswordPolicy_requireLowercaseCharacters' - Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z).
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that passwords do not
-- require at least one lowercase character.
--
-- 'requireNumbers', 'updateAccountPasswordPolicy_requireNumbers' - Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that passwords do not
-- require at least one numeric character.
--
-- 'requireSymbols', 'updateAccountPasswordPolicy_requireSymbols' - Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters:
--
-- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that passwords do not
-- require at least one symbol character.
--
-- 'requireUppercaseCharacters', 'updateAccountPasswordPolicy_requireUppercaseCharacters' - Specifies whether IAM user passwords must contain at least one uppercase
-- character from the ISO basic Latin alphabet (A to Z).
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that passwords do not
-- require at least one uppercase character.
newUpdateAccountPasswordPolicy ::
  UpdateAccountPasswordPolicy
newUpdateAccountPasswordPolicy =
  UpdateAccountPasswordPolicy'
    { allowUsersToChangePassword =
        Prelude.Nothing,
      hardExpiry = Prelude.Nothing,
      maxPasswordAge = Prelude.Nothing,
      minimumPasswordLength = Prelude.Nothing,
      passwordReusePrevention = Prelude.Nothing,
      requireLowercaseCharacters = Prelude.Nothing,
      requireNumbers = Prelude.Nothing,
      requireSymbols = Prelude.Nothing,
      requireUppercaseCharacters = Prelude.Nothing
    }

-- | Allows all IAM users in your account to use the Amazon Web Services
-- Management Console to change their own passwords. For more information,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_passwords_enable-user-change.html Permitting IAM users to change their own passwords>
-- in the /IAM User Guide/.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that IAM users in the
-- account do not automatically have permissions to change their own
-- password.
updateAccountPasswordPolicy_allowUsersToChangePassword :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Bool)
updateAccountPasswordPolicy_allowUsersToChangePassword = Lens.lens (\UpdateAccountPasswordPolicy' {allowUsersToChangePassword} -> allowUsersToChangePassword) (\s@UpdateAccountPasswordPolicy' {} a -> s {allowUsersToChangePassword = a} :: UpdateAccountPasswordPolicy)

-- | Prevents IAM users who are accessing the account via the Amazon Web
-- Services Management Console from setting a new console password after
-- their password has expired. The IAM user cannot access the console until
-- an administrator resets the password.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that IAM users can
-- change their passwords after they expire and continue to sign in as the
-- user.
--
-- In the Amazon Web Services Management Console, the custom password
-- policy option __Allow users to change their own password__ gives IAM
-- users permissions to @iam:ChangePassword@ for only their user and to the
-- @iam:GetAccountPasswordPolicy@ action. This option does not attach a
-- permissions policy to each user, rather the permissions are applied at
-- the account-level for all users by IAM. IAM users with
-- @iam:ChangePassword@ permission and active access keys can reset their
-- own expired console password using the CLI or API.
updateAccountPasswordPolicy_hardExpiry :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Bool)
updateAccountPasswordPolicy_hardExpiry = Lens.lens (\UpdateAccountPasswordPolicy' {hardExpiry} -> hardExpiry) (\s@UpdateAccountPasswordPolicy' {} a -> s {hardExpiry = a} :: UpdateAccountPasswordPolicy)

-- | The number of days that an IAM user password is valid.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @0@. The result is that IAM user passwords
-- never expire.
updateAccountPasswordPolicy_maxPasswordAge :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Natural)
updateAccountPasswordPolicy_maxPasswordAge = Lens.lens (\UpdateAccountPasswordPolicy' {maxPasswordAge} -> maxPasswordAge) (\s@UpdateAccountPasswordPolicy' {} a -> s {maxPasswordAge = a} :: UpdateAccountPasswordPolicy)

-- | The minimum number of characters allowed in an IAM user password.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @6@.
updateAccountPasswordPolicy_minimumPasswordLength :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Natural)
updateAccountPasswordPolicy_minimumPasswordLength = Lens.lens (\UpdateAccountPasswordPolicy' {minimumPasswordLength} -> minimumPasswordLength) (\s@UpdateAccountPasswordPolicy' {} a -> s {minimumPasswordLength = a} :: UpdateAccountPasswordPolicy)

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @0@. The result is that IAM users are not
-- prevented from reusing previous passwords.
updateAccountPasswordPolicy_passwordReusePrevention :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Natural)
updateAccountPasswordPolicy_passwordReusePrevention = Lens.lens (\UpdateAccountPasswordPolicy' {passwordReusePrevention} -> passwordReusePrevention) (\s@UpdateAccountPasswordPolicy' {} a -> s {passwordReusePrevention = a} :: UpdateAccountPasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z).
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that passwords do not
-- require at least one lowercase character.
updateAccountPasswordPolicy_requireLowercaseCharacters :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Bool)
updateAccountPasswordPolicy_requireLowercaseCharacters = Lens.lens (\UpdateAccountPasswordPolicy' {requireLowercaseCharacters} -> requireLowercaseCharacters) (\s@UpdateAccountPasswordPolicy' {} a -> s {requireLowercaseCharacters = a} :: UpdateAccountPasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that passwords do not
-- require at least one numeric character.
updateAccountPasswordPolicy_requireNumbers :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Bool)
updateAccountPasswordPolicy_requireNumbers = Lens.lens (\UpdateAccountPasswordPolicy' {requireNumbers} -> requireNumbers) (\s@UpdateAccountPasswordPolicy' {} a -> s {requireNumbers = a} :: UpdateAccountPasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters:
--
-- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that passwords do not
-- require at least one symbol character.
updateAccountPasswordPolicy_requireSymbols :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Bool)
updateAccountPasswordPolicy_requireSymbols = Lens.lens (\UpdateAccountPasswordPolicy' {requireSymbols} -> requireSymbols) (\s@UpdateAccountPasswordPolicy' {} a -> s {requireSymbols = a} :: UpdateAccountPasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character from the ISO basic Latin alphabet (A to Z).
--
-- If you do not specify a value for this parameter, then the operation
-- uses the default value of @false@. The result is that passwords do not
-- require at least one uppercase character.
updateAccountPasswordPolicy_requireUppercaseCharacters :: Lens.Lens' UpdateAccountPasswordPolicy (Prelude.Maybe Prelude.Bool)
updateAccountPasswordPolicy_requireUppercaseCharacters = Lens.lens (\UpdateAccountPasswordPolicy' {requireUppercaseCharacters} -> requireUppercaseCharacters) (\s@UpdateAccountPasswordPolicy' {} a -> s {requireUppercaseCharacters = a} :: UpdateAccountPasswordPolicy)

instance Core.AWSRequest UpdateAccountPasswordPolicy where
  type
    AWSResponse UpdateAccountPasswordPolicy =
      UpdateAccountPasswordPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      UpdateAccountPasswordPolicyResponse'

instance Prelude.Hashable UpdateAccountPasswordPolicy where
  hashWithSalt _salt UpdateAccountPasswordPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` allowUsersToChangePassword
      `Prelude.hashWithSalt` hardExpiry
      `Prelude.hashWithSalt` maxPasswordAge
      `Prelude.hashWithSalt` minimumPasswordLength
      `Prelude.hashWithSalt` passwordReusePrevention
      `Prelude.hashWithSalt` requireLowercaseCharacters
      `Prelude.hashWithSalt` requireNumbers
      `Prelude.hashWithSalt` requireSymbols
      `Prelude.hashWithSalt` requireUppercaseCharacters

instance Prelude.NFData UpdateAccountPasswordPolicy where
  rnf UpdateAccountPasswordPolicy' {..} =
    Prelude.rnf allowUsersToChangePassword
      `Prelude.seq` Prelude.rnf hardExpiry
      `Prelude.seq` Prelude.rnf maxPasswordAge
      `Prelude.seq` Prelude.rnf minimumPasswordLength
      `Prelude.seq` Prelude.rnf passwordReusePrevention
      `Prelude.seq` Prelude.rnf requireLowercaseCharacters
      `Prelude.seq` Prelude.rnf requireNumbers
      `Prelude.seq` Prelude.rnf requireSymbols
      `Prelude.seq` Prelude.rnf requireUppercaseCharacters

instance Data.ToHeaders UpdateAccountPasswordPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateAccountPasswordPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAccountPasswordPolicy where
  toQuery UpdateAccountPasswordPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "UpdateAccountPasswordPolicy" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "AllowUsersToChangePassword"
          Data.=: allowUsersToChangePassword,
        "HardExpiry" Data.=: hardExpiry,
        "MaxPasswordAge" Data.=: maxPasswordAge,
        "MinimumPasswordLength"
          Data.=: minimumPasswordLength,
        "PasswordReusePrevention"
          Data.=: passwordReusePrevention,
        "RequireLowercaseCharacters"
          Data.=: requireLowercaseCharacters,
        "RequireNumbers" Data.=: requireNumbers,
        "RequireSymbols" Data.=: requireSymbols,
        "RequireUppercaseCharacters"
          Data.=: requireUppercaseCharacters
      ]

-- | /See:/ 'newUpdateAccountPasswordPolicyResponse' smart constructor.
data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountPasswordPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAccountPasswordPolicyResponse ::
  UpdateAccountPasswordPolicyResponse
newUpdateAccountPasswordPolicyResponse =
  UpdateAccountPasswordPolicyResponse'

instance
  Prelude.NFData
    UpdateAccountPasswordPolicyResponse
  where
  rnf _ = ()
