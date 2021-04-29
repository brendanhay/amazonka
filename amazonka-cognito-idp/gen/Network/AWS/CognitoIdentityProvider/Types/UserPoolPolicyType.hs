{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType where

import Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The policy associated with a user pool.
--
-- /See:/ 'newUserPoolPolicyType' smart constructor.
data UserPoolPolicyType = UserPoolPolicyType'
  { -- | The password policy.
    passwordPolicy :: Prelude.Maybe PasswordPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON UserPoolPolicyType where
  parseJSON =
    Prelude.withObject
      "UserPoolPolicyType"
      ( \x ->
          UserPoolPolicyType'
            Prelude.<$> (x Prelude..:? "PasswordPolicy")
      )

instance Prelude.Hashable UserPoolPolicyType

instance Prelude.NFData UserPoolPolicyType

instance Prelude.ToJSON UserPoolPolicyType where
  toJSON UserPoolPolicyType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PasswordPolicy" Prelude..=)
              Prelude.<$> passwordPolicy
          ]
      )
