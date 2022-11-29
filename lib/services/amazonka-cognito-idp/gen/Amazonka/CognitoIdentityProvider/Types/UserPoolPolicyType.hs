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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserPoolPolicyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserPoolPolicyType where

import Amazonka.CognitoIdentityProvider.Types.PasswordPolicyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The policy associated with a user pool.
--
-- /See:/ 'newUserPoolPolicyType' smart constructor.
data UserPoolPolicyType = UserPoolPolicyType'
  { -- | The password policy.
    passwordPolicy :: Prelude.Maybe PasswordPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPoolPolicyType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordPolicy', 'userPoolPolicyType_passwordPolicy' - The password policy.
newUserPoolPolicyType ::
  UserPoolPolicyType
newUserPoolPolicyType =
  UserPoolPolicyType'
    { passwordPolicy =
        Prelude.Nothing
    }

-- | The password policy.
userPoolPolicyType_passwordPolicy :: Lens.Lens' UserPoolPolicyType (Prelude.Maybe PasswordPolicyType)
userPoolPolicyType_passwordPolicy = Lens.lens (\UserPoolPolicyType' {passwordPolicy} -> passwordPolicy) (\s@UserPoolPolicyType' {} a -> s {passwordPolicy = a} :: UserPoolPolicyType)

instance Core.FromJSON UserPoolPolicyType where
  parseJSON =
    Core.withObject
      "UserPoolPolicyType"
      ( \x ->
          UserPoolPolicyType'
            Prelude.<$> (x Core..:? "PasswordPolicy")
      )

instance Prelude.Hashable UserPoolPolicyType where
  hashWithSalt _salt UserPoolPolicyType' {..} =
    _salt `Prelude.hashWithSalt` passwordPolicy

instance Prelude.NFData UserPoolPolicyType where
  rnf UserPoolPolicyType' {..} =
    Prelude.rnf passwordPolicy

instance Core.ToJSON UserPoolPolicyType where
  toJSON UserPoolPolicyType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PasswordPolicy" Core..=)
              Prelude.<$> passwordPolicy
          ]
      )
