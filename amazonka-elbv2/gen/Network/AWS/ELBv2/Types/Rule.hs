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
-- Module      : Network.AWS.ELBv2.Types.Rule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Rule where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.RuleCondition
import qualified Network.AWS.Lens as Lens

-- | Information about a rule.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | Indicates whether this is the default rule.
    isDefault :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Core.Maybe Core.Text,
    -- | The actions. Each rule must include exactly one of the following types
    -- of actions: @forward@, @redirect@, or @fixed-response@, and it must be
    -- the last action to be performed.
    actions :: Core.Maybe [Action],
    -- | The priority.
    priority :: Core.Maybe Core.Text,
    -- | The conditions. Each rule can include zero or one of the following
    -- conditions: @http-request-method@, @host-header@, @path-pattern@, and
    -- @source-ip@, and zero or more of the following conditions: @http-header@
    -- and @query-string@.
    conditions :: Core.Maybe [RuleCondition]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefault', 'rule_isDefault' - Indicates whether this is the default rule.
--
-- 'ruleArn', 'rule_ruleArn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'actions', 'rule_actions' - The actions. Each rule must include exactly one of the following types
-- of actions: @forward@, @redirect@, or @fixed-response@, and it must be
-- the last action to be performed.
--
-- 'priority', 'rule_priority' - The priority.
--
-- 'conditions', 'rule_conditions' - The conditions. Each rule can include zero or one of the following
-- conditions: @http-request-method@, @host-header@, @path-pattern@, and
-- @source-ip@, and zero or more of the following conditions: @http-header@
-- and @query-string@.
newRule ::
  Rule
newRule =
  Rule'
    { isDefault = Core.Nothing,
      ruleArn = Core.Nothing,
      actions = Core.Nothing,
      priority = Core.Nothing,
      conditions = Core.Nothing
    }

-- | Indicates whether this is the default rule.
rule_isDefault :: Lens.Lens' Rule (Core.Maybe Core.Bool)
rule_isDefault = Lens.lens (\Rule' {isDefault} -> isDefault) (\s@Rule' {} a -> s {isDefault = a} :: Rule)

-- | The Amazon Resource Name (ARN) of the rule.
rule_ruleArn :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_ruleArn = Lens.lens (\Rule' {ruleArn} -> ruleArn) (\s@Rule' {} a -> s {ruleArn = a} :: Rule)

-- | The actions. Each rule must include exactly one of the following types
-- of actions: @forward@, @redirect@, or @fixed-response@, and it must be
-- the last action to be performed.
rule_actions :: Lens.Lens' Rule (Core.Maybe [Action])
rule_actions = Lens.lens (\Rule' {actions} -> actions) (\s@Rule' {} a -> s {actions = a} :: Rule) Core.. Lens.mapping Lens._Coerce

-- | The priority.
rule_priority :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_priority = Lens.lens (\Rule' {priority} -> priority) (\s@Rule' {} a -> s {priority = a} :: Rule)

-- | The conditions. Each rule can include zero or one of the following
-- conditions: @http-request-method@, @host-header@, @path-pattern@, and
-- @source-ip@, and zero or more of the following conditions: @http-header@
-- and @query-string@.
rule_conditions :: Lens.Lens' Rule (Core.Maybe [RuleCondition])
rule_conditions = Lens.lens (\Rule' {conditions} -> conditions) (\s@Rule' {} a -> s {conditions = a} :: Rule) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML Rule where
  parseXML x =
    Rule'
      Core.<$> (x Core..@? "IsDefault")
      Core.<*> (x Core..@? "RuleArn")
      Core.<*> ( x Core..@? "Actions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Priority")
      Core.<*> ( x Core..@? "Conditions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable Rule

instance Core.NFData Rule
