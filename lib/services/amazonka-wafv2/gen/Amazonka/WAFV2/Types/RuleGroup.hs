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
-- Module      : Amazonka.WAFV2.Types.RuleGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RuleGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CustomResponseBody
import Amazonka.WAFV2.Types.LabelSummary
import Amazonka.WAFV2.Types.Rule
import Amazonka.WAFV2.Types.VisibilityConfig

-- | A rule group defines a collection of rules to inspect and control web
-- requests that you can use in a WebACL. When you create a rule group, you
-- define an immutable capacity limit. If you update a rule group, you must
-- stay within the capacity. This allows others to reuse the rule group
-- with confidence in its capacity requirements.
--
-- /See:/ 'newRuleGroup' smart constructor.
data RuleGroup = RuleGroup'
  { -- | The labels that one or more rules in this rule group add to matching web
    -- requests. These labels are defined in the @RuleLabels@ for a Rule.
    availableLabels :: Prelude.Maybe [LabelSummary],
    -- | The labels that one or more rules in this rule group match against in
    -- label match statements. These labels are defined in a
    -- @LabelMatchStatement@ specification, in the Statement definition of a
    -- rule.
    consumedLabels :: Prelude.Maybe [LabelSummary],
    -- | A map of custom response keys and content bodies. When you create a rule
    -- with a block action, you can send a custom response to the web request.
    -- You define these for the rule group, and then use them in the rules that
    -- you define in the rule group.
    --
    -- For information about customizing web requests and responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    --
    -- For information about the limits on count and size for custom request
    -- and response settings, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    customResponseBodies :: Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody),
    -- | A description of the rule group that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The label namespace prefix for this rule group. All labels added by
    -- rules in this rule group have this prefix.
    --
    -- -   The syntax for the label namespace prefix for your rule groups is
    --     the following:
    --
    --     @awswaf:\<account ID>:rulegroup:\<rule group name>:@
    --
    -- -   When a rule with a label matches a web request, WAF adds the fully
    --     qualified label to the request. A fully qualified label is made up
    --     of the label namespace from the rule group or web ACL where the rule
    --     is defined and the label from the rule, separated by a colon:
    --
    --     @\<label namespace>:\<label from rule>@
    labelNamespace :: Prelude.Maybe Prelude.Text,
    -- | The Rule statements used to identify the web requests that you want to
    -- allow, block, or count. Each rule includes one top-level statement that
    -- WAF uses to identify matching web requests, and parameters that govern
    -- how WAF handles them.
    rules :: Prelude.Maybe [Rule],
    -- | The name of the rule group. You cannot change the name of a rule group
    -- after you create it.
    name :: Prelude.Text,
    -- | A unique identifier for the rule group. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Text,
    -- | The web ACL capacity units (WCUs) required for this rule group.
    --
    -- When you create your own rule group, you define this, and you cannot
    -- change it after creation. When you add or modify the rules in a rule
    -- group, WAF enforces this limit. You can check the capacity for a set of
    -- rules using CheckCapacity.
    --
    -- WAF uses WCUs to calculate and control the operating resources that are
    -- used to run your rules, rule groups, and web ACLs. WAF calculates
    -- capacity differently for each rule type, to reflect the relative cost of
    -- each rule. Simple rules that cost little to run use fewer WCUs than more
    -- complex rules that use more processing power. Rule group capacity is
    -- fixed at creation, which helps users plan their web ACL WCU usage when
    -- they use a rule group. The WCU limit for web ACLs is 1,500.
    capacity :: Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Text,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: VisibilityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availableLabels', 'ruleGroup_availableLabels' - The labels that one or more rules in this rule group add to matching web
-- requests. These labels are defined in the @RuleLabels@ for a Rule.
--
-- 'consumedLabels', 'ruleGroup_consumedLabels' - The labels that one or more rules in this rule group match against in
-- label match statements. These labels are defined in a
-- @LabelMatchStatement@ specification, in the Statement definition of a
-- rule.
--
-- 'customResponseBodies', 'ruleGroup_customResponseBodies' - A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the rule group, and then use them in the rules that
-- you define in the rule group.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- 'description', 'ruleGroup_description' - A description of the rule group that helps with identification.
--
-- 'labelNamespace', 'ruleGroup_labelNamespace' - The label namespace prefix for this rule group. All labels added by
-- rules in this rule group have this prefix.
--
-- -   The syntax for the label namespace prefix for your rule groups is
--     the following:
--
--     @awswaf:\<account ID>:rulegroup:\<rule group name>:@
--
-- -   When a rule with a label matches a web request, WAF adds the fully
--     qualified label to the request. A fully qualified label is made up
--     of the label namespace from the rule group or web ACL where the rule
--     is defined and the label from the rule, separated by a colon:
--
--     @\<label namespace>:\<label from rule>@
--
-- 'rules', 'ruleGroup_rules' - The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
--
-- 'name', 'ruleGroup_name' - The name of the rule group. You cannot change the name of a rule group
-- after you create it.
--
-- 'id', 'ruleGroup_id' - A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'capacity', 'ruleGroup_capacity' - The web ACL capacity units (WCUs) required for this rule group.
--
-- When you create your own rule group, you define this, and you cannot
-- change it after creation. When you add or modify the rules in a rule
-- group, WAF enforces this limit. You can check the capacity for a set of
-- rules using CheckCapacity.
--
-- WAF uses WCUs to calculate and control the operating resources that are
-- used to run your rules, rule groups, and web ACLs. WAF calculates
-- capacity differently for each rule type, to reflect the relative cost of
-- each rule. Simple rules that cost little to run use fewer WCUs than more
-- complex rules that use more processing power. Rule group capacity is
-- fixed at creation, which helps users plan their web ACL WCU usage when
-- they use a rule group. The WCU limit for web ACLs is 1,500.
--
-- 'arn', 'ruleGroup_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'visibilityConfig', 'ruleGroup_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newRuleGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'capacity'
  Prelude.Natural ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'visibilityConfig'
  VisibilityConfig ->
  RuleGroup
newRuleGroup
  pName_
  pId_
  pCapacity_
  pARN_
  pVisibilityConfig_ =
    RuleGroup'
      { availableLabels = Prelude.Nothing,
        consumedLabels = Prelude.Nothing,
        customResponseBodies = Prelude.Nothing,
        description = Prelude.Nothing,
        labelNamespace = Prelude.Nothing,
        rules = Prelude.Nothing,
        name = pName_,
        id = pId_,
        capacity = pCapacity_,
        arn = pARN_,
        visibilityConfig = pVisibilityConfig_
      }

-- | The labels that one or more rules in this rule group add to matching web
-- requests. These labels are defined in the @RuleLabels@ for a Rule.
ruleGroup_availableLabels :: Lens.Lens' RuleGroup (Prelude.Maybe [LabelSummary])
ruleGroup_availableLabels = Lens.lens (\RuleGroup' {availableLabels} -> availableLabels) (\s@RuleGroup' {} a -> s {availableLabels = a} :: RuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | The labels that one or more rules in this rule group match against in
-- label match statements. These labels are defined in a
-- @LabelMatchStatement@ specification, in the Statement definition of a
-- rule.
ruleGroup_consumedLabels :: Lens.Lens' RuleGroup (Prelude.Maybe [LabelSummary])
ruleGroup_consumedLabels = Lens.lens (\RuleGroup' {consumedLabels} -> consumedLabels) (\s@RuleGroup' {} a -> s {consumedLabels = a} :: RuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the rule group, and then use them in the rules that
-- you define in the rule group.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
ruleGroup_customResponseBodies :: Lens.Lens' RuleGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody))
ruleGroup_customResponseBodies = Lens.lens (\RuleGroup' {customResponseBodies} -> customResponseBodies) (\s@RuleGroup' {} a -> s {customResponseBodies = a} :: RuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | A description of the rule group that helps with identification.
ruleGroup_description :: Lens.Lens' RuleGroup (Prelude.Maybe Prelude.Text)
ruleGroup_description = Lens.lens (\RuleGroup' {description} -> description) (\s@RuleGroup' {} a -> s {description = a} :: RuleGroup)

-- | The label namespace prefix for this rule group. All labels added by
-- rules in this rule group have this prefix.
--
-- -   The syntax for the label namespace prefix for your rule groups is
--     the following:
--
--     @awswaf:\<account ID>:rulegroup:\<rule group name>:@
--
-- -   When a rule with a label matches a web request, WAF adds the fully
--     qualified label to the request. A fully qualified label is made up
--     of the label namespace from the rule group or web ACL where the rule
--     is defined and the label from the rule, separated by a colon:
--
--     @\<label namespace>:\<label from rule>@
ruleGroup_labelNamespace :: Lens.Lens' RuleGroup (Prelude.Maybe Prelude.Text)
ruleGroup_labelNamespace = Lens.lens (\RuleGroup' {labelNamespace} -> labelNamespace) (\s@RuleGroup' {} a -> s {labelNamespace = a} :: RuleGroup)

-- | The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
ruleGroup_rules :: Lens.Lens' RuleGroup (Prelude.Maybe [Rule])
ruleGroup_rules = Lens.lens (\RuleGroup' {rules} -> rules) (\s@RuleGroup' {} a -> s {rules = a} :: RuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the rule group. You cannot change the name of a rule group
-- after you create it.
ruleGroup_name :: Lens.Lens' RuleGroup Prelude.Text
ruleGroup_name = Lens.lens (\RuleGroup' {name} -> name) (\s@RuleGroup' {} a -> s {name = a} :: RuleGroup)

-- | A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
ruleGroup_id :: Lens.Lens' RuleGroup Prelude.Text
ruleGroup_id = Lens.lens (\RuleGroup' {id} -> id) (\s@RuleGroup' {} a -> s {id = a} :: RuleGroup)

-- | The web ACL capacity units (WCUs) required for this rule group.
--
-- When you create your own rule group, you define this, and you cannot
-- change it after creation. When you add or modify the rules in a rule
-- group, WAF enforces this limit. You can check the capacity for a set of
-- rules using CheckCapacity.
--
-- WAF uses WCUs to calculate and control the operating resources that are
-- used to run your rules, rule groups, and web ACLs. WAF calculates
-- capacity differently for each rule type, to reflect the relative cost of
-- each rule. Simple rules that cost little to run use fewer WCUs than more
-- complex rules that use more processing power. Rule group capacity is
-- fixed at creation, which helps users plan their web ACL WCU usage when
-- they use a rule group. The WCU limit for web ACLs is 1,500.
ruleGroup_capacity :: Lens.Lens' RuleGroup Prelude.Natural
ruleGroup_capacity = Lens.lens (\RuleGroup' {capacity} -> capacity) (\s@RuleGroup' {} a -> s {capacity = a} :: RuleGroup)

-- | The Amazon Resource Name (ARN) of the entity.
ruleGroup_arn :: Lens.Lens' RuleGroup Prelude.Text
ruleGroup_arn = Lens.lens (\RuleGroup' {arn} -> arn) (\s@RuleGroup' {} a -> s {arn = a} :: RuleGroup)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
ruleGroup_visibilityConfig :: Lens.Lens' RuleGroup VisibilityConfig
ruleGroup_visibilityConfig = Lens.lens (\RuleGroup' {visibilityConfig} -> visibilityConfig) (\s@RuleGroup' {} a -> s {visibilityConfig = a} :: RuleGroup)

instance Data.FromJSON RuleGroup where
  parseJSON =
    Data.withObject
      "RuleGroup"
      ( \x ->
          RuleGroup'
            Prelude.<$> ( x Data..:? "AvailableLabels"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ConsumedLabels" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "CustomResponseBodies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LabelNamespace")
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Capacity")
            Prelude.<*> (x Data..: "ARN")
            Prelude.<*> (x Data..: "VisibilityConfig")
      )

instance Prelude.Hashable RuleGroup where
  hashWithSalt _salt RuleGroup' {..} =
    _salt `Prelude.hashWithSalt` availableLabels
      `Prelude.hashWithSalt` consumedLabels
      `Prelude.hashWithSalt` customResponseBodies
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` labelNamespace
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData RuleGroup where
  rnf RuleGroup' {..} =
    Prelude.rnf availableLabels
      `Prelude.seq` Prelude.rnf consumedLabels
      `Prelude.seq` Prelude.rnf customResponseBodies
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf labelNamespace
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf visibilityConfig
