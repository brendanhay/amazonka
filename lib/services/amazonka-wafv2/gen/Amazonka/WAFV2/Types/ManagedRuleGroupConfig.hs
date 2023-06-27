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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ManagedRuleGroupConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.AWSManagedRulesACFPRuleSet
import Amazonka.WAFV2.Types.AWSManagedRulesATPRuleSet
import Amazonka.WAFV2.Types.AWSManagedRulesBotControlRuleSet
import Amazonka.WAFV2.Types.PasswordField
import Amazonka.WAFV2.Types.PayloadType
import Amazonka.WAFV2.Types.UsernameField

-- | Additional information that\'s used by a managed rule group. Many
-- managed rule groups don\'t require this.
--
-- The rule groups used for intelligent threat mitigation require
-- additional configuration:
--
-- -   Use the @AWSManagedRulesACFPRuleSet@ configuration object to
--     configure the account creation fraud prevention managed rule group.
--     The configuration includes the registration and sign-up pages of
--     your application and the locations in the account creation request
--     payload of data, such as the user email and phone number fields.
--
-- -   Use the @AWSManagedRulesATPRuleSet@ configuration object to
--     configure the account takeover prevention managed rule group. The
--     configuration includes the sign-in page of your application and the
--     locations in the login request payload of data such as the username
--     and password.
--
-- -   Use the @AWSManagedRulesBotControlRuleSet@ configuration object to
--     configure the protection level that you want the Bot Control rule
--     group to use.
--
-- For example specifications, see the examples section of CreateWebACL.
--
-- /See:/ 'newManagedRuleGroupConfig' smart constructor.
data ManagedRuleGroupConfig = ManagedRuleGroupConfig'
  { -- | Additional configuration for using the account creation fraud prevention
    -- (ACFP) managed rule group, @AWSManagedRulesACFPRuleSet@. Use this to
    -- provide account creation request information to the rule group. For web
    -- ACLs that protect CloudFront distributions, use this to also provide the
    -- information about how your distribution responds to account creation
    -- requests.
    --
    -- For information about using the ACFP managed rule group, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-acfp.html WAF Fraud Control account creation fraud prevention (ACFP) rule group>
    -- and
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-acfp.html WAF Fraud Control account creation fraud prevention (ACFP)>
    -- in the /WAF Developer Guide/.
    aWSManagedRulesACFPRuleSet :: Prelude.Maybe AWSManagedRulesACFPRuleSet,
    -- | Additional configuration for using the account takeover prevention (ATP)
    -- managed rule group, @AWSManagedRulesATPRuleSet@. Use this to provide
    -- login request information to the rule group. For web ACLs that protect
    -- CloudFront distributions, use this to also provide the information about
    -- how your distribution responds to login requests.
    --
    -- This configuration replaces the individual configuration fields in
    -- @ManagedRuleGroupConfig@ and provides additional feature configuration.
    --
    -- For information about using the ATP managed rule group, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-atp.html WAF Fraud Control account takeover prevention (ATP) rule group>
    -- and
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-atp.html WAF Fraud Control account takeover prevention (ATP)>
    -- in the /WAF Developer Guide/.
    aWSManagedRulesATPRuleSet :: Prelude.Maybe AWSManagedRulesATPRuleSet,
    -- | Additional configuration for using the Bot Control managed rule group.
    -- Use this to specify the inspection level that you want to use. For
    -- information about using the Bot Control managed rule group, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-bot.html WAF Bot Control rule group>
    -- and
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-bot-control.html WAF Bot Control>
    -- in the /WAF Developer Guide/.
    aWSManagedRulesBotControlRuleSet :: Prelude.Maybe AWSManagedRulesBotControlRuleSet,
    -- | Instead of this setting, provide your configuration under
    -- @AWSManagedRulesATPRuleSet@.
    loginPath :: Prelude.Maybe Prelude.Text,
    -- | Instead of this setting, provide your configuration under the request
    -- inspection configuration for @AWSManagedRulesATPRuleSet@ or
    -- @AWSManagedRulesACFPRuleSet@.
    passwordField :: Prelude.Maybe PasswordField,
    -- | Instead of this setting, provide your configuration under the request
    -- inspection configuration for @AWSManagedRulesATPRuleSet@ or
    -- @AWSManagedRulesACFPRuleSet@.
    payloadType :: Prelude.Maybe PayloadType,
    -- | Instead of this setting, provide your configuration under the request
    -- inspection configuration for @AWSManagedRulesATPRuleSet@ or
    -- @AWSManagedRulesACFPRuleSet@.
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
-- 'aWSManagedRulesACFPRuleSet', 'managedRuleGroupConfig_aWSManagedRulesACFPRuleSet' - Additional configuration for using the account creation fraud prevention
-- (ACFP) managed rule group, @AWSManagedRulesACFPRuleSet@. Use this to
-- provide account creation request information to the rule group. For web
-- ACLs that protect CloudFront distributions, use this to also provide the
-- information about how your distribution responds to account creation
-- requests.
--
-- For information about using the ACFP managed rule group, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-acfp.html WAF Fraud Control account creation fraud prevention (ACFP) rule group>
-- and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-acfp.html WAF Fraud Control account creation fraud prevention (ACFP)>
-- in the /WAF Developer Guide/.
--
-- 'aWSManagedRulesATPRuleSet', 'managedRuleGroupConfig_aWSManagedRulesATPRuleSet' - Additional configuration for using the account takeover prevention (ATP)
-- managed rule group, @AWSManagedRulesATPRuleSet@. Use this to provide
-- login request information to the rule group. For web ACLs that protect
-- CloudFront distributions, use this to also provide the information about
-- how your distribution responds to login requests.
--
-- This configuration replaces the individual configuration fields in
-- @ManagedRuleGroupConfig@ and provides additional feature configuration.
--
-- For information about using the ATP managed rule group, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-atp.html WAF Fraud Control account takeover prevention (ATP) rule group>
-- and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-atp.html WAF Fraud Control account takeover prevention (ATP)>
-- in the /WAF Developer Guide/.
--
-- 'aWSManagedRulesBotControlRuleSet', 'managedRuleGroupConfig_aWSManagedRulesBotControlRuleSet' - Additional configuration for using the Bot Control managed rule group.
-- Use this to specify the inspection level that you want to use. For
-- information about using the Bot Control managed rule group, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-bot.html WAF Bot Control rule group>
-- and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-bot-control.html WAF Bot Control>
-- in the /WAF Developer Guide/.
--
-- 'loginPath', 'managedRuleGroupConfig_loginPath' - Instead of this setting, provide your configuration under
-- @AWSManagedRulesATPRuleSet@.
--
-- 'passwordField', 'managedRuleGroupConfig_passwordField' - Instead of this setting, provide your configuration under the request
-- inspection configuration for @AWSManagedRulesATPRuleSet@ or
-- @AWSManagedRulesACFPRuleSet@.
--
-- 'payloadType', 'managedRuleGroupConfig_payloadType' - Instead of this setting, provide your configuration under the request
-- inspection configuration for @AWSManagedRulesATPRuleSet@ or
-- @AWSManagedRulesACFPRuleSet@.
--
-- 'usernameField', 'managedRuleGroupConfig_usernameField' - Instead of this setting, provide your configuration under the request
-- inspection configuration for @AWSManagedRulesATPRuleSet@ or
-- @AWSManagedRulesACFPRuleSet@.
newManagedRuleGroupConfig ::
  ManagedRuleGroupConfig
newManagedRuleGroupConfig =
  ManagedRuleGroupConfig'
    { aWSManagedRulesACFPRuleSet =
        Prelude.Nothing,
      aWSManagedRulesATPRuleSet = Prelude.Nothing,
      aWSManagedRulesBotControlRuleSet = Prelude.Nothing,
      loginPath = Prelude.Nothing,
      passwordField = Prelude.Nothing,
      payloadType = Prelude.Nothing,
      usernameField = Prelude.Nothing
    }

-- | Additional configuration for using the account creation fraud prevention
-- (ACFP) managed rule group, @AWSManagedRulesACFPRuleSet@. Use this to
-- provide account creation request information to the rule group. For web
-- ACLs that protect CloudFront distributions, use this to also provide the
-- information about how your distribution responds to account creation
-- requests.
--
-- For information about using the ACFP managed rule group, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-acfp.html WAF Fraud Control account creation fraud prevention (ACFP) rule group>
-- and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-acfp.html WAF Fraud Control account creation fraud prevention (ACFP)>
-- in the /WAF Developer Guide/.
managedRuleGroupConfig_aWSManagedRulesACFPRuleSet :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe AWSManagedRulesACFPRuleSet)
managedRuleGroupConfig_aWSManagedRulesACFPRuleSet = Lens.lens (\ManagedRuleGroupConfig' {aWSManagedRulesACFPRuleSet} -> aWSManagedRulesACFPRuleSet) (\s@ManagedRuleGroupConfig' {} a -> s {aWSManagedRulesACFPRuleSet = a} :: ManagedRuleGroupConfig)

-- | Additional configuration for using the account takeover prevention (ATP)
-- managed rule group, @AWSManagedRulesATPRuleSet@. Use this to provide
-- login request information to the rule group. For web ACLs that protect
-- CloudFront distributions, use this to also provide the information about
-- how your distribution responds to login requests.
--
-- This configuration replaces the individual configuration fields in
-- @ManagedRuleGroupConfig@ and provides additional feature configuration.
--
-- For information about using the ATP managed rule group, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-atp.html WAF Fraud Control account takeover prevention (ATP) rule group>
-- and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-atp.html WAF Fraud Control account takeover prevention (ATP)>
-- in the /WAF Developer Guide/.
managedRuleGroupConfig_aWSManagedRulesATPRuleSet :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe AWSManagedRulesATPRuleSet)
managedRuleGroupConfig_aWSManagedRulesATPRuleSet = Lens.lens (\ManagedRuleGroupConfig' {aWSManagedRulesATPRuleSet} -> aWSManagedRulesATPRuleSet) (\s@ManagedRuleGroupConfig' {} a -> s {aWSManagedRulesATPRuleSet = a} :: ManagedRuleGroupConfig)

-- | Additional configuration for using the Bot Control managed rule group.
-- Use this to specify the inspection level that you want to use. For
-- information about using the Bot Control managed rule group, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-bot.html WAF Bot Control rule group>
-- and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-bot-control.html WAF Bot Control>
-- in the /WAF Developer Guide/.
managedRuleGroupConfig_aWSManagedRulesBotControlRuleSet :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe AWSManagedRulesBotControlRuleSet)
managedRuleGroupConfig_aWSManagedRulesBotControlRuleSet = Lens.lens (\ManagedRuleGroupConfig' {aWSManagedRulesBotControlRuleSet} -> aWSManagedRulesBotControlRuleSet) (\s@ManagedRuleGroupConfig' {} a -> s {aWSManagedRulesBotControlRuleSet = a} :: ManagedRuleGroupConfig)

-- | Instead of this setting, provide your configuration under
-- @AWSManagedRulesATPRuleSet@.
managedRuleGroupConfig_loginPath :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe Prelude.Text)
managedRuleGroupConfig_loginPath = Lens.lens (\ManagedRuleGroupConfig' {loginPath} -> loginPath) (\s@ManagedRuleGroupConfig' {} a -> s {loginPath = a} :: ManagedRuleGroupConfig)

-- | Instead of this setting, provide your configuration under the request
-- inspection configuration for @AWSManagedRulesATPRuleSet@ or
-- @AWSManagedRulesACFPRuleSet@.
managedRuleGroupConfig_passwordField :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe PasswordField)
managedRuleGroupConfig_passwordField = Lens.lens (\ManagedRuleGroupConfig' {passwordField} -> passwordField) (\s@ManagedRuleGroupConfig' {} a -> s {passwordField = a} :: ManagedRuleGroupConfig)

-- | Instead of this setting, provide your configuration under the request
-- inspection configuration for @AWSManagedRulesATPRuleSet@ or
-- @AWSManagedRulesACFPRuleSet@.
managedRuleGroupConfig_payloadType :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe PayloadType)
managedRuleGroupConfig_payloadType = Lens.lens (\ManagedRuleGroupConfig' {payloadType} -> payloadType) (\s@ManagedRuleGroupConfig' {} a -> s {payloadType = a} :: ManagedRuleGroupConfig)

-- | Instead of this setting, provide your configuration under the request
-- inspection configuration for @AWSManagedRulesATPRuleSet@ or
-- @AWSManagedRulesACFPRuleSet@.
managedRuleGroupConfig_usernameField :: Lens.Lens' ManagedRuleGroupConfig (Prelude.Maybe UsernameField)
managedRuleGroupConfig_usernameField = Lens.lens (\ManagedRuleGroupConfig' {usernameField} -> usernameField) (\s@ManagedRuleGroupConfig' {} a -> s {usernameField = a} :: ManagedRuleGroupConfig)

instance Data.FromJSON ManagedRuleGroupConfig where
  parseJSON =
    Data.withObject
      "ManagedRuleGroupConfig"
      ( \x ->
          ManagedRuleGroupConfig'
            Prelude.<$> (x Data..:? "AWSManagedRulesACFPRuleSet")
            Prelude.<*> (x Data..:? "AWSManagedRulesATPRuleSet")
            Prelude.<*> (x Data..:? "AWSManagedRulesBotControlRuleSet")
            Prelude.<*> (x Data..:? "LoginPath")
            Prelude.<*> (x Data..:? "PasswordField")
            Prelude.<*> (x Data..:? "PayloadType")
            Prelude.<*> (x Data..:? "UsernameField")
      )

instance Prelude.Hashable ManagedRuleGroupConfig where
  hashWithSalt _salt ManagedRuleGroupConfig' {..} =
    _salt
      `Prelude.hashWithSalt` aWSManagedRulesACFPRuleSet
      `Prelude.hashWithSalt` aWSManagedRulesATPRuleSet
      `Prelude.hashWithSalt` aWSManagedRulesBotControlRuleSet
      `Prelude.hashWithSalt` loginPath
      `Prelude.hashWithSalt` passwordField
      `Prelude.hashWithSalt` payloadType
      `Prelude.hashWithSalt` usernameField

instance Prelude.NFData ManagedRuleGroupConfig where
  rnf ManagedRuleGroupConfig' {..} =
    Prelude.rnf aWSManagedRulesACFPRuleSet
      `Prelude.seq` Prelude.rnf aWSManagedRulesATPRuleSet
      `Prelude.seq` Prelude.rnf aWSManagedRulesBotControlRuleSet
      `Prelude.seq` Prelude.rnf loginPath
      `Prelude.seq` Prelude.rnf passwordField
      `Prelude.seq` Prelude.rnf payloadType
      `Prelude.seq` Prelude.rnf usernameField

instance Data.ToJSON ManagedRuleGroupConfig where
  toJSON ManagedRuleGroupConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AWSManagedRulesACFPRuleSet" Data..=)
              Prelude.<$> aWSManagedRulesACFPRuleSet,
            ("AWSManagedRulesATPRuleSet" Data..=)
              Prelude.<$> aWSManagedRulesATPRuleSet,
            ("AWSManagedRulesBotControlRuleSet" Data..=)
              Prelude.<$> aWSManagedRulesBotControlRuleSet,
            ("LoginPath" Data..=) Prelude.<$> loginPath,
            ("PasswordField" Data..=) Prelude.<$> passwordField,
            ("PayloadType" Data..=) Prelude.<$> payloadType,
            ("UsernameField" Data..=) Prelude.<$> usernameField
          ]
      )
