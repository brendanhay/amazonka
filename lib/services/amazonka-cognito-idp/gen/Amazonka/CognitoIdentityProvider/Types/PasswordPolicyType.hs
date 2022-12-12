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
-- Module      : Amazonka.CognitoIdentityProvider.Types.PasswordPolicyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.PasswordPolicyType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The password policy type.
--
-- /See:/ 'newPasswordPolicyType' smart constructor.
data PasswordPolicyType = PasswordPolicyType'
  { -- | The minimum length of the password in the policy that you have set. This
    -- value can\'t be less than 6.
    minimumLength :: Prelude.Maybe Prelude.Natural,
    -- | In the password policy that you have set, refers to whether you have
    -- required users to use at least one lowercase letter in their password.
    requireLowercase :: Prelude.Maybe Prelude.Bool,
    -- | In the password policy that you have set, refers to whether you have
    -- required users to use at least one number in their password.
    requireNumbers :: Prelude.Maybe Prelude.Bool,
    -- | In the password policy that you have set, refers to whether you have
    -- required users to use at least one symbol in their password.
    requireSymbols :: Prelude.Maybe Prelude.Bool,
    -- | In the password policy that you have set, refers to whether you have
    -- required users to use at least one uppercase letter in their password.
    requireUppercase :: Prelude.Maybe Prelude.Bool,
    -- | The number of days a temporary password is valid in the password policy.
    -- If the user doesn\'t sign in during this time, an administrator must
    -- reset their password.
    --
    -- When you set @TemporaryPasswordValidityDays@ for a user pool, you can no
    -- longer set a value for the legacy @UnusedAccountValidityDays@ parameter
    -- in that user pool.
    temporaryPasswordValidityDays :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PasswordPolicyType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumLength', 'passwordPolicyType_minimumLength' - The minimum length of the password in the policy that you have set. This
-- value can\'t be less than 6.
--
-- 'requireLowercase', 'passwordPolicyType_requireLowercase' - In the password policy that you have set, refers to whether you have
-- required users to use at least one lowercase letter in their password.
--
-- 'requireNumbers', 'passwordPolicyType_requireNumbers' - In the password policy that you have set, refers to whether you have
-- required users to use at least one number in their password.
--
-- 'requireSymbols', 'passwordPolicyType_requireSymbols' - In the password policy that you have set, refers to whether you have
-- required users to use at least one symbol in their password.
--
-- 'requireUppercase', 'passwordPolicyType_requireUppercase' - In the password policy that you have set, refers to whether you have
-- required users to use at least one uppercase letter in their password.
--
-- 'temporaryPasswordValidityDays', 'passwordPolicyType_temporaryPasswordValidityDays' - The number of days a temporary password is valid in the password policy.
-- If the user doesn\'t sign in during this time, an administrator must
-- reset their password.
--
-- When you set @TemporaryPasswordValidityDays@ for a user pool, you can no
-- longer set a value for the legacy @UnusedAccountValidityDays@ parameter
-- in that user pool.
newPasswordPolicyType ::
  PasswordPolicyType
newPasswordPolicyType =
  PasswordPolicyType'
    { minimumLength =
        Prelude.Nothing,
      requireLowercase = Prelude.Nothing,
      requireNumbers = Prelude.Nothing,
      requireSymbols = Prelude.Nothing,
      requireUppercase = Prelude.Nothing,
      temporaryPasswordValidityDays = Prelude.Nothing
    }

-- | The minimum length of the password in the policy that you have set. This
-- value can\'t be less than 6.
passwordPolicyType_minimumLength :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Natural)
passwordPolicyType_minimumLength = Lens.lens (\PasswordPolicyType' {minimumLength} -> minimumLength) (\s@PasswordPolicyType' {} a -> s {minimumLength = a} :: PasswordPolicyType)

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one lowercase letter in their password.
passwordPolicyType_requireLowercase :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Bool)
passwordPolicyType_requireLowercase = Lens.lens (\PasswordPolicyType' {requireLowercase} -> requireLowercase) (\s@PasswordPolicyType' {} a -> s {requireLowercase = a} :: PasswordPolicyType)

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one number in their password.
passwordPolicyType_requireNumbers :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Bool)
passwordPolicyType_requireNumbers = Lens.lens (\PasswordPolicyType' {requireNumbers} -> requireNumbers) (\s@PasswordPolicyType' {} a -> s {requireNumbers = a} :: PasswordPolicyType)

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one symbol in their password.
passwordPolicyType_requireSymbols :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Bool)
passwordPolicyType_requireSymbols = Lens.lens (\PasswordPolicyType' {requireSymbols} -> requireSymbols) (\s@PasswordPolicyType' {} a -> s {requireSymbols = a} :: PasswordPolicyType)

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one uppercase letter in their password.
passwordPolicyType_requireUppercase :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Bool)
passwordPolicyType_requireUppercase = Lens.lens (\PasswordPolicyType' {requireUppercase} -> requireUppercase) (\s@PasswordPolicyType' {} a -> s {requireUppercase = a} :: PasswordPolicyType)

-- | The number of days a temporary password is valid in the password policy.
-- If the user doesn\'t sign in during this time, an administrator must
-- reset their password.
--
-- When you set @TemporaryPasswordValidityDays@ for a user pool, you can no
-- longer set a value for the legacy @UnusedAccountValidityDays@ parameter
-- in that user pool.
passwordPolicyType_temporaryPasswordValidityDays :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Natural)
passwordPolicyType_temporaryPasswordValidityDays = Lens.lens (\PasswordPolicyType' {temporaryPasswordValidityDays} -> temporaryPasswordValidityDays) (\s@PasswordPolicyType' {} a -> s {temporaryPasswordValidityDays = a} :: PasswordPolicyType)

instance Data.FromJSON PasswordPolicyType where
  parseJSON =
    Data.withObject
      "PasswordPolicyType"
      ( \x ->
          PasswordPolicyType'
            Prelude.<$> (x Data..:? "MinimumLength")
            Prelude.<*> (x Data..:? "RequireLowercase")
            Prelude.<*> (x Data..:? "RequireNumbers")
            Prelude.<*> (x Data..:? "RequireSymbols")
            Prelude.<*> (x Data..:? "RequireUppercase")
            Prelude.<*> (x Data..:? "TemporaryPasswordValidityDays")
      )

instance Prelude.Hashable PasswordPolicyType where
  hashWithSalt _salt PasswordPolicyType' {..} =
    _salt `Prelude.hashWithSalt` minimumLength
      `Prelude.hashWithSalt` requireLowercase
      `Prelude.hashWithSalt` requireNumbers
      `Prelude.hashWithSalt` requireSymbols
      `Prelude.hashWithSalt` requireUppercase
      `Prelude.hashWithSalt` temporaryPasswordValidityDays

instance Prelude.NFData PasswordPolicyType where
  rnf PasswordPolicyType' {..} =
    Prelude.rnf minimumLength
      `Prelude.seq` Prelude.rnf requireLowercase
      `Prelude.seq` Prelude.rnf requireNumbers
      `Prelude.seq` Prelude.rnf requireSymbols
      `Prelude.seq` Prelude.rnf requireUppercase
      `Prelude.seq` Prelude.rnf temporaryPasswordValidityDays

instance Data.ToJSON PasswordPolicyType where
  toJSON PasswordPolicyType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MinimumLength" Data..=) Prelude.<$> minimumLength,
            ("RequireLowercase" Data..=)
              Prelude.<$> requireLowercase,
            ("RequireNumbers" Data..=)
              Prelude.<$> requireNumbers,
            ("RequireSymbols" Data..=)
              Prelude.<$> requireSymbols,
            ("RequireUppercase" Data..=)
              Prelude.<$> requireUppercase,
            ("TemporaryPasswordValidityDays" Data..=)
              Prelude.<$> temporaryPasswordValidityDays
          ]
      )
