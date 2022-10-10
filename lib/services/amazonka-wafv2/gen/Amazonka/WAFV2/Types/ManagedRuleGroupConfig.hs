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
-- Module      : Amazonka.WAFV2.Types.ManagedRuleGroupConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ManagedRuleGroupConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.PasswordField
import Amazonka.WAFV2.Types.PayloadType
import Amazonka.WAFV2.Types.UsernameField

-- | Additional information that\'s used by a managed rule group. Most
-- managed rule groups don\'t require this.
--
-- Use this for the account takeover prevention managed rule group
-- @AWSManagedRulesATPRuleSet@, to provide information about the sign-in
-- page of your application.
--
-- You can provide multiple individual @ManagedRuleGroupConfig@ objects for
-- any rule group configuration, for example @UsernameField@ and
-- @PasswordField@. The configuration that you provide depends on the needs
-- of the managed rule group. For the ATP managed rule group, you provide
-- the following individual configuration objects: @LoginPath@,
-- @PasswordField@, @PayloadType@ and @UsernameField@.
--
-- For example specifications, see the examples section of CreateWebACL.
--
-- /See:/ 'newManagedRuleGroupConfig' smart constructor.
data ManagedRuleGroupConfig = ManagedRuleGroupConfig'
  { -- | Details about your login page password field.
    passwordField :: Prelude.Maybe PasswordField,
    -- | The path of the login endpoint for your application. For example, for
    -- the URL @https:\/\/example.com\/web\/login@, you would provide the path
    -- @\/web\/login@.
    loginPath :: Prelude.Maybe Prelude.Text,
    -- | The payload type for your login endpoint, either JSON or form encoded.
    payloadType :: Prelude.Maybe PayloadType,
    -- | Details about your login page username field.
    usernameField :: Prelude.Maybe UsernameField
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRuleGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordField', 'managedRuleGroupConfig_passwordField' - Details about your login page password field.
--
-- 'loginPath', 'managedRuleGroupConfig_loginPath' - The path of the login endpoint for your application. For example, for
-- the URL @https:\/\/example.com\/web\/login@, you would provide the path
-- @\/web\/login@.
--
-- 'payloadType', 'managedRuleGroupConfig_payloadType' - The payload type for your login endpoint, either JSON or form encoded.
--
-- 'usernameField', 'managedRuleGroupConfig_usernameField' - Details about your login page username field.
newManagedRuleGroupConfig ::
  ManagedRuleGroupConfig
newManagedRuleGroupConfig =
  ManagedRuleGroupConfig'
    { passwordField =
        Prelude.Nothing,
      loginPath = Prelude.Nothing,
      payloadType = Prelude.Nothing,
      usernameField = Prelude.Nothing
    }

-- | Details about your login page password field.
managedRuleGroupConfig_passwordField :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe PasswordField)
managedRuleGroupConfig_passwordField = Lens.lens (\ManagedRuleGroupConfig' {passwordField} -> passwordField) (\s@ManagedRuleGroupConfig' {} a -> s {passwordField = a} :: ManagedRuleGroupConfig)

-- | The path of the login endpoint for your application. For example, for
-- the URL @https:\/\/example.com\/web\/login@, you would provide the path
-- @\/web\/login@.
managedRuleGroupConfig_loginPath :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe Prelude.Text)
managedRuleGroupConfig_loginPath = Lens.lens (\ManagedRuleGroupConfig' {loginPath} -> loginPath) (\s@ManagedRuleGroupConfig' {} a -> s {loginPath = a} :: ManagedRuleGroupConfig)

-- | The payload type for your login endpoint, either JSON or form encoded.
managedRuleGroupConfig_payloadType :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe PayloadType)
managedRuleGroupConfig_payloadType = Lens.lens (\ManagedRuleGroupConfig' {payloadType} -> payloadType) (\s@ManagedRuleGroupConfig' {} a -> s {payloadType = a} :: ManagedRuleGroupConfig)

-- | Details about your login page username field.
managedRuleGroupConfig_usernameField :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe UsernameField)
managedRuleGroupConfig_usernameField = Lens.lens (\ManagedRuleGroupConfig' {usernameField} -> usernameField) (\s@ManagedRuleGroupConfig' {} a -> s {usernameField = a} :: ManagedRuleGroupConfig)

instance Core.FromJSON ManagedRuleGroupConfig where
  parseJSON =
    Core.withObject
      "ManagedRuleGroupConfig"
      ( \x ->
          ManagedRuleGroupConfig'
            Prelude.<$> (x Core..:? "PasswordField")
            Prelude.<*> (x Core..:? "LoginPath")
            Prelude.<*> (x Core..:? "PayloadType")
            Prelude.<*> (x Core..:? "UsernameField")
      )

instance Prelude.Hashable ManagedRuleGroupConfig where
  hashWithSalt _salt ManagedRuleGroupConfig' {..} =
    _salt `Prelude.hashWithSalt` passwordField
      `Prelude.hashWithSalt` loginPath
      `Prelude.hashWithSalt` payloadType
      `Prelude.hashWithSalt` usernameField

instance Prelude.NFData ManagedRuleGroupConfig where
  rnf ManagedRuleGroupConfig' {..} =
    Prelude.rnf passwordField
      `Prelude.seq` Prelude.rnf loginPath
      `Prelude.seq` Prelude.rnf payloadType
      `Prelude.seq` Prelude.rnf usernameField

instance Core.ToJSON ManagedRuleGroupConfig where
  toJSON ManagedRuleGroupConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PasswordField" Core..=) Prelude.<$> passwordField,
            ("LoginPath" Core..=) Prelude.<$> loginPath,
            ("PayloadType" Core..=) Prelude.<$> payloadType,
            ("UsernameField" Core..=) Prelude.<$> usernameField
          ]
      )
