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
-- Module      : Amazonka.WAFV2.Types.Rule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Rule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CaptchaConfig
import Amazonka.WAFV2.Types.ChallengeConfig
import Amazonka.WAFV2.Types.Label
import Amazonka.WAFV2.Types.OverrideAction
import Amazonka.WAFV2.Types.RuleAction
import Amazonka.WAFV2.Types.Statement
import Amazonka.WAFV2.Types.VisibilityConfig

-- | A single rule, which you can use in a WebACL or RuleGroup to identify
-- web requests that you want to allow, block, or count. Each rule includes
-- one top-level Statement that WAF uses to identify matching web requests,
-- and parameters that govern how WAF handles them.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | The action that WAF should take on a web request when it matches the
    -- rule statement. Settings at the web ACL level can override the rule
    -- action setting.
    --
    -- This is used only for rules whose statements do not reference a rule
    -- group. Rule statements that reference a rule group include
    -- @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
    --
    -- You must specify either this @Action@ setting or the rule
    -- @OverrideAction@ setting, but not both:
    --
    -- -   If the rule statement does not reference a rule group, use this rule
    --     action setting and not the rule override action setting.
    --
    -- -   If the rule statement references a rule group, use the override
    --     action setting and not this action setting.
    action :: Prelude.Maybe RuleAction,
    -- | Specifies how WAF should handle @CAPTCHA@ evaluations. If you don\'t
    -- specify this, WAF uses the @CAPTCHA@ configuration that\'s defined for
    -- the web ACL.
    captchaConfig :: Prelude.Maybe CaptchaConfig,
    -- | Specifies how WAF should handle @Challenge@ evaluations. If you don\'t
    -- specify this, WAF uses the challenge configuration that\'s defined for
    -- the web ACL.
    challengeConfig :: Prelude.Maybe ChallengeConfig,
    -- | The action to use in the place of the action that results from the rule
    -- group evaluation. Set the override action to none to leave the result of
    -- the rule group alone. Set it to count to override the result to count
    -- only.
    --
    -- You can only use this for rule statements that reference a rule group,
    -- like @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
    --
    -- This option is usually set to none. It does not affect how the rules in
    -- the rule group are evaluated. If you want the rules in the rule group to
    -- only count matches, do not use this and instead use the rule action
    -- override option, with @Count@ action, in your rule group reference
    -- statement settings.
    overrideAction :: Prelude.Maybe OverrideAction,
    -- | Labels to apply to web requests that match the rule match statement. WAF
    -- applies fully qualified labels to matching web requests. A fully
    -- qualified label is the concatenation of a label namespace and a rule
    -- label. The rule\'s rule group or web ACL defines the label namespace.
    --
    -- Rules that run after this rule in the web ACL can match against these
    -- labels using a @LabelMatchStatement@.
    --
    -- For each label, provide a case-sensitive string containing optional
    -- namespaces and a label name, according to the following guidelines:
    --
    -- -   Separate each component of the label with a colon.
    --
    -- -   Each namespace or name can have up to 128 characters.
    --
    -- -   You can specify up to 5 namespaces in a label.
    --
    -- -   Don\'t use the following reserved words in your label specification:
    --     @aws@, @waf@, @managed@, @rulegroup@, @webacl@, @regexpatternset@,
    --     or @ipset@.
    --
    -- For example, @myLabelName@ or @nameSpace1:nameSpace2:myLabelName@.
    ruleLabels :: Prelude.Maybe [Label],
    -- | The name of the rule. You can\'t change the name of a @Rule@ after you
    -- create it.
    name :: Prelude.Text,
    -- | If you define more than one @Rule@ in a @WebACL@, WAF evaluates each
    -- request against the @Rules@ in order based on the value of @Priority@.
    -- WAF processes rules with lower priority first. The priorities don\'t
    -- need to be consecutive, but they must all be different.
    priority :: Prelude.Natural,
    -- | The WAF processing statement for the rule, for example
    -- ByteMatchStatement or SizeConstraintStatement.
    statement :: Statement,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: VisibilityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'rule_action' - The action that WAF should take on a web request when it matches the
-- rule statement. Settings at the web ACL level can override the rule
-- action setting.
--
-- This is used only for rules whose statements do not reference a rule
-- group. Rule statements that reference a rule group include
-- @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
--
-- You must specify either this @Action@ setting or the rule
-- @OverrideAction@ setting, but not both:
--
-- -   If the rule statement does not reference a rule group, use this rule
--     action setting and not the rule override action setting.
--
-- -   If the rule statement references a rule group, use the override
--     action setting and not this action setting.
--
-- 'captchaConfig', 'rule_captchaConfig' - Specifies how WAF should handle @CAPTCHA@ evaluations. If you don\'t
-- specify this, WAF uses the @CAPTCHA@ configuration that\'s defined for
-- the web ACL.
--
-- 'challengeConfig', 'rule_challengeConfig' - Specifies how WAF should handle @Challenge@ evaluations. If you don\'t
-- specify this, WAF uses the challenge configuration that\'s defined for
-- the web ACL.
--
-- 'overrideAction', 'rule_overrideAction' - The action to use in the place of the action that results from the rule
-- group evaluation. Set the override action to none to leave the result of
-- the rule group alone. Set it to count to override the result to count
-- only.
--
-- You can only use this for rule statements that reference a rule group,
-- like @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
--
-- This option is usually set to none. It does not affect how the rules in
-- the rule group are evaluated. If you want the rules in the rule group to
-- only count matches, do not use this and instead use the rule action
-- override option, with @Count@ action, in your rule group reference
-- statement settings.
--
-- 'ruleLabels', 'rule_ruleLabels' - Labels to apply to web requests that match the rule match statement. WAF
-- applies fully qualified labels to matching web requests. A fully
-- qualified label is the concatenation of a label namespace and a rule
-- label. The rule\'s rule group or web ACL defines the label namespace.
--
-- Rules that run after this rule in the web ACL can match against these
-- labels using a @LabelMatchStatement@.
--
-- For each label, provide a case-sensitive string containing optional
-- namespaces and a label name, according to the following guidelines:
--
-- -   Separate each component of the label with a colon.
--
-- -   Each namespace or name can have up to 128 characters.
--
-- -   You can specify up to 5 namespaces in a label.
--
-- -   Don\'t use the following reserved words in your label specification:
--     @aws@, @waf@, @managed@, @rulegroup@, @webacl@, @regexpatternset@,
--     or @ipset@.
--
-- For example, @myLabelName@ or @nameSpace1:nameSpace2:myLabelName@.
--
-- 'name', 'rule_name' - The name of the rule. You can\'t change the name of a @Rule@ after you
-- create it.
--
-- 'priority', 'rule_priority' - If you define more than one @Rule@ in a @WebACL@, WAF evaluates each
-- request against the @Rules@ in order based on the value of @Priority@.
-- WAF processes rules with lower priority first. The priorities don\'t
-- need to be consecutive, but they must all be different.
--
-- 'statement', 'rule_statement' - The WAF processing statement for the rule, for example
-- ByteMatchStatement or SizeConstraintStatement.
--
-- 'visibilityConfig', 'rule_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Natural ->
  -- | 'statement'
  Statement ->
  -- | 'visibilityConfig'
  VisibilityConfig ->
  Rule
newRule
  pName_
  pPriority_
  pStatement_
  pVisibilityConfig_ =
    Rule'
      { action = Prelude.Nothing,
        captchaConfig = Prelude.Nothing,
        challengeConfig = Prelude.Nothing,
        overrideAction = Prelude.Nothing,
        ruleLabels = Prelude.Nothing,
        name = pName_,
        priority = pPriority_,
        statement = pStatement_,
        visibilityConfig = pVisibilityConfig_
      }

-- | The action that WAF should take on a web request when it matches the
-- rule statement. Settings at the web ACL level can override the rule
-- action setting.
--
-- This is used only for rules whose statements do not reference a rule
-- group. Rule statements that reference a rule group include
-- @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
--
-- You must specify either this @Action@ setting or the rule
-- @OverrideAction@ setting, but not both:
--
-- -   If the rule statement does not reference a rule group, use this rule
--     action setting and not the rule override action setting.
--
-- -   If the rule statement references a rule group, use the override
--     action setting and not this action setting.
rule_action :: Lens.Lens' Rule (Prelude.Maybe RuleAction)
rule_action = Lens.lens (\Rule' {action} -> action) (\s@Rule' {} a -> s {action = a} :: Rule)

-- | Specifies how WAF should handle @CAPTCHA@ evaluations. If you don\'t
-- specify this, WAF uses the @CAPTCHA@ configuration that\'s defined for
-- the web ACL.
rule_captchaConfig :: Lens.Lens' Rule (Prelude.Maybe CaptchaConfig)
rule_captchaConfig = Lens.lens (\Rule' {captchaConfig} -> captchaConfig) (\s@Rule' {} a -> s {captchaConfig = a} :: Rule)

-- | Specifies how WAF should handle @Challenge@ evaluations. If you don\'t
-- specify this, WAF uses the challenge configuration that\'s defined for
-- the web ACL.
rule_challengeConfig :: Lens.Lens' Rule (Prelude.Maybe ChallengeConfig)
rule_challengeConfig = Lens.lens (\Rule' {challengeConfig} -> challengeConfig) (\s@Rule' {} a -> s {challengeConfig = a} :: Rule)

-- | The action to use in the place of the action that results from the rule
-- group evaluation. Set the override action to none to leave the result of
-- the rule group alone. Set it to count to override the result to count
-- only.
--
-- You can only use this for rule statements that reference a rule group,
-- like @RuleGroupReferenceStatement@ and @ManagedRuleGroupStatement@.
--
-- This option is usually set to none. It does not affect how the rules in
-- the rule group are evaluated. If you want the rules in the rule group to
-- only count matches, do not use this and instead use the rule action
-- override option, with @Count@ action, in your rule group reference
-- statement settings.
rule_overrideAction :: Lens.Lens' Rule (Prelude.Maybe OverrideAction)
rule_overrideAction = Lens.lens (\Rule' {overrideAction} -> overrideAction) (\s@Rule' {} a -> s {overrideAction = a} :: Rule)

-- | Labels to apply to web requests that match the rule match statement. WAF
-- applies fully qualified labels to matching web requests. A fully
-- qualified label is the concatenation of a label namespace and a rule
-- label. The rule\'s rule group or web ACL defines the label namespace.
--
-- Rules that run after this rule in the web ACL can match against these
-- labels using a @LabelMatchStatement@.
--
-- For each label, provide a case-sensitive string containing optional
-- namespaces and a label name, according to the following guidelines:
--
-- -   Separate each component of the label with a colon.
--
-- -   Each namespace or name can have up to 128 characters.
--
-- -   You can specify up to 5 namespaces in a label.
--
-- -   Don\'t use the following reserved words in your label specification:
--     @aws@, @waf@, @managed@, @rulegroup@, @webacl@, @regexpatternset@,
--     or @ipset@.
--
-- For example, @myLabelName@ or @nameSpace1:nameSpace2:myLabelName@.
rule_ruleLabels :: Lens.Lens' Rule (Prelude.Maybe [Label])
rule_ruleLabels = Lens.lens (\Rule' {ruleLabels} -> ruleLabels) (\s@Rule' {} a -> s {ruleLabels = a} :: Rule) Prelude.. Lens.mapping Lens.coerced

-- | The name of the rule. You can\'t change the name of a @Rule@ after you
-- create it.
rule_name :: Lens.Lens' Rule Prelude.Text
rule_name = Lens.lens (\Rule' {name} -> name) (\s@Rule' {} a -> s {name = a} :: Rule)

-- | If you define more than one @Rule@ in a @WebACL@, WAF evaluates each
-- request against the @Rules@ in order based on the value of @Priority@.
-- WAF processes rules with lower priority first. The priorities don\'t
-- need to be consecutive, but they must all be different.
rule_priority :: Lens.Lens' Rule Prelude.Natural
rule_priority = Lens.lens (\Rule' {priority} -> priority) (\s@Rule' {} a -> s {priority = a} :: Rule)

-- | The WAF processing statement for the rule, for example
-- ByteMatchStatement or SizeConstraintStatement.
rule_statement :: Lens.Lens' Rule Statement
rule_statement = Lens.lens (\Rule' {statement} -> statement) (\s@Rule' {} a -> s {statement = a} :: Rule)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
rule_visibilityConfig :: Lens.Lens' Rule VisibilityConfig
rule_visibilityConfig = Lens.lens (\Rule' {visibilityConfig} -> visibilityConfig) (\s@Rule' {} a -> s {visibilityConfig = a} :: Rule)

instance Data.FromJSON Rule where
  parseJSON =
    Data.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "CaptchaConfig")
            Prelude.<*> (x Data..:? "ChallengeConfig")
            Prelude.<*> (x Data..:? "OverrideAction")
            Prelude.<*> (x Data..:? "RuleLabels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Priority")
            Prelude.<*> (x Data..: "Statement")
            Prelude.<*> (x Data..: "VisibilityConfig")
      )

instance Prelude.Hashable Rule where
  hashWithSalt _salt Rule' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` captchaConfig
      `Prelude.hashWithSalt` challengeConfig
      `Prelude.hashWithSalt` overrideAction
      `Prelude.hashWithSalt` ruleLabels
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` statement
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData Rule where
  rnf Rule' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf captchaConfig
      `Prelude.seq` Prelude.rnf challengeConfig
      `Prelude.seq` Prelude.rnf overrideAction
      `Prelude.seq` Prelude.rnf ruleLabels
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf statement
      `Prelude.seq` Prelude.rnf visibilityConfig

instance Data.ToJSON Rule where
  toJSON Rule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("CaptchaConfig" Data..=) Prelude.<$> captchaConfig,
            ("ChallengeConfig" Data..=)
              Prelude.<$> challengeConfig,
            ("OverrideAction" Data..=)
              Prelude.<$> overrideAction,
            ("RuleLabels" Data..=) Prelude.<$> ruleLabels,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Priority" Data..= priority),
            Prelude.Just ("Statement" Data..= statement),
            Prelude.Just
              ("VisibilityConfig" Data..= visibilityConfig)
          ]
      )
