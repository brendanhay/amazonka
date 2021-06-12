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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The policy associated with a user pool.
--
-- /See:/ 'newUserPoolPolicyType' smart constructor.
data UserPoolPolicyType = UserPoolPolicyType'
  { -- | The password policy.
    passwordPolicy :: Core.Maybe PasswordPolicyType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  UserPoolPolicyType' {passwordPolicy = Core.Nothing}

-- | The password policy.
userPoolPolicyType_passwordPolicy :: Lens.Lens' UserPoolPolicyType (Core.Maybe PasswordPolicyType)
userPoolPolicyType_passwordPolicy = Lens.lens (\UserPoolPolicyType' {passwordPolicy} -> passwordPolicy) (\s@UserPoolPolicyType' {} a -> s {passwordPolicy = a} :: UserPoolPolicyType)

instance Core.FromJSON UserPoolPolicyType where
  parseJSON =
    Core.withObject
      "UserPoolPolicyType"
      ( \x ->
          UserPoolPolicyType'
            Core.<$> (x Core..:? "PasswordPolicy")
      )

instance Core.Hashable UserPoolPolicyType

instance Core.NFData UserPoolPolicyType

instance Core.ToJSON UserPoolPolicyType where
  toJSON UserPoolPolicyType' {..} =
    Core.object
      ( Core.catMaybes
          [("PasswordPolicy" Core..=) Core.<$> passwordPolicy]
      )
