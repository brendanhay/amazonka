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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The password policy type.
--
-- /See:/ 'newPasswordPolicyType' smart constructor.
data PasswordPolicyType = PasswordPolicyType'
  { -- | In the password policy you have set, refers to the number of days a
    -- temporary password is valid. If the user does not sign-in during this
    -- time, their password will need to be reset by an administrator.
    --
    -- When you set @TemporaryPasswordValidityDays@ for a user pool, you will
    -- no longer be able to set the deprecated @UnusedAccountValidityDays@
    -- value for that user pool.
    temporaryPasswordValidityDays :: Prelude.Maybe Prelude.Natural,
    -- | In the password policy that you have set, refers to whether you have
    -- required users to use at least one uppercase letter in their password.
    requireUppercase :: Prelude.Maybe Prelude.Bool,
    -- | The minimum length of the password policy that you have set. Cannot be
    -- less than 6.
    minimumLength :: Prelude.Maybe Prelude.Natural,
    -- | In the password policy that you have set, refers to whether you have
    -- required users to use at least one symbol in their password.
    requireSymbols :: Prelude.Maybe Prelude.Bool,
    -- | In the password policy that you have set, refers to whether you have
    -- required users to use at least one number in their password.
    requireNumbers :: Prelude.Maybe Prelude.Bool,
    -- | In the password policy that you have set, refers to whether you have
    -- required users to use at least one lowercase letter in their password.
    requireLowercase :: Prelude.Maybe Prelude.Bool
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
-- 'temporaryPasswordValidityDays', 'passwordPolicyType_temporaryPasswordValidityDays' - In the password policy you have set, refers to the number of days a
-- temporary password is valid. If the user does not sign-in during this
-- time, their password will need to be reset by an administrator.
--
-- When you set @TemporaryPasswordValidityDays@ for a user pool, you will
-- no longer be able to set the deprecated @UnusedAccountValidityDays@
-- value for that user pool.
--
-- 'requireUppercase', 'passwordPolicyType_requireUppercase' - In the password policy that you have set, refers to whether you have
-- required users to use at least one uppercase letter in their password.
--
-- 'minimumLength', 'passwordPolicyType_minimumLength' - The minimum length of the password policy that you have set. Cannot be
-- less than 6.
--
-- 'requireSymbols', 'passwordPolicyType_requireSymbols' - In the password policy that you have set, refers to whether you have
-- required users to use at least one symbol in their password.
--
-- 'requireNumbers', 'passwordPolicyType_requireNumbers' - In the password policy that you have set, refers to whether you have
-- required users to use at least one number in their password.
--
-- 'requireLowercase', 'passwordPolicyType_requireLowercase' - In the password policy that you have set, refers to whether you have
-- required users to use at least one lowercase letter in their password.
newPasswordPolicyType ::
  PasswordPolicyType
newPasswordPolicyType =
  PasswordPolicyType'
    { temporaryPasswordValidityDays =
        Prelude.Nothing,
      requireUppercase = Prelude.Nothing,
      minimumLength = Prelude.Nothing,
      requireSymbols = Prelude.Nothing,
      requireNumbers = Prelude.Nothing,
      requireLowercase = Prelude.Nothing
    }

-- | In the password policy you have set, refers to the number of days a
-- temporary password is valid. If the user does not sign-in during this
-- time, their password will need to be reset by an administrator.
--
-- When you set @TemporaryPasswordValidityDays@ for a user pool, you will
-- no longer be able to set the deprecated @UnusedAccountValidityDays@
-- value for that user pool.
passwordPolicyType_temporaryPasswordValidityDays :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Natural)
passwordPolicyType_temporaryPasswordValidityDays = Lens.lens (\PasswordPolicyType' {temporaryPasswordValidityDays} -> temporaryPasswordValidityDays) (\s@PasswordPolicyType' {} a -> s {temporaryPasswordValidityDays = a} :: PasswordPolicyType)

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one uppercase letter in their password.
passwordPolicyType_requireUppercase :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Bool)
passwordPolicyType_requireUppercase = Lens.lens (\PasswordPolicyType' {requireUppercase} -> requireUppercase) (\s@PasswordPolicyType' {} a -> s {requireUppercase = a} :: PasswordPolicyType)

-- | The minimum length of the password policy that you have set. Cannot be
-- less than 6.
passwordPolicyType_minimumLength :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Natural)
passwordPolicyType_minimumLength = Lens.lens (\PasswordPolicyType' {minimumLength} -> minimumLength) (\s@PasswordPolicyType' {} a -> s {minimumLength = a} :: PasswordPolicyType)

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one symbol in their password.
passwordPolicyType_requireSymbols :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Bool)
passwordPolicyType_requireSymbols = Lens.lens (\PasswordPolicyType' {requireSymbols} -> requireSymbols) (\s@PasswordPolicyType' {} a -> s {requireSymbols = a} :: PasswordPolicyType)

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one number in their password.
passwordPolicyType_requireNumbers :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Bool)
passwordPolicyType_requireNumbers = Lens.lens (\PasswordPolicyType' {requireNumbers} -> requireNumbers) (\s@PasswordPolicyType' {} a -> s {requireNumbers = a} :: PasswordPolicyType)

-- | In the password policy that you have set, refers to whether you have
-- required users to use at least one lowercase letter in their password.
passwordPolicyType_requireLowercase :: Lens.Lens' PasswordPolicyType (Prelude.Maybe Prelude.Bool)
passwordPolicyType_requireLowercase = Lens.lens (\PasswordPolicyType' {requireLowercase} -> requireLowercase) (\s@PasswordPolicyType' {} a -> s {requireLowercase = a} :: PasswordPolicyType)

instance Core.FromJSON PasswordPolicyType where
  parseJSON =
    Core.withObject
      "PasswordPolicyType"
      ( \x ->
          PasswordPolicyType'
            Prelude.<$> (x Core..:? "TemporaryPasswordValidityDays")
            Prelude.<*> (x Core..:? "RequireUppercase")
            Prelude.<*> (x Core..:? "MinimumLength")
            Prelude.<*> (x Core..:? "RequireSymbols")
            Prelude.<*> (x Core..:? "RequireNumbers")
            Prelude.<*> (x Core..:? "RequireLowercase")
      )

instance Prelude.Hashable PasswordPolicyType

instance Prelude.NFData PasswordPolicyType

instance Core.ToJSON PasswordPolicyType where
  toJSON PasswordPolicyType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TemporaryPasswordValidityDays" Core..=)
              Prelude.<$> temporaryPasswordValidityDays,
            ("RequireUppercase" Core..=)
              Prelude.<$> requireUppercase,
            ("MinimumLength" Core..=) Prelude.<$> minimumLength,
            ("RequireSymbols" Core..=)
              Prelude.<$> requireSymbols,
            ("RequireNumbers" Core..=)
              Prelude.<$> requireNumbers,
            ("RequireLowercase" Core..=)
              Prelude.<$> requireLowercase
          ]
      )
