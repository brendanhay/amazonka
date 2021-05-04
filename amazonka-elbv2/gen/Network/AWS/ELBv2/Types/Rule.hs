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
-- Module      : Network.AWS.ELBv2.Types.Rule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Rule where

import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.RuleCondition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a rule.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | Indicates whether this is the default rule.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Prelude.Maybe Prelude.Text,
    -- | The actions. Each rule must include exactly one of the following types
    -- of actions: @forward@, @redirect@, or @fixed-response@, and it must be
    -- the last action to be performed.
    actions :: Prelude.Maybe [Action],
    -- | The priority.
    priority :: Prelude.Maybe Prelude.Text,
    -- | The conditions. Each rule can include zero or one of the following
    -- conditions: @http-request-method@, @host-header@, @path-pattern@, and
    -- @source-ip@, and zero or more of the following conditions: @http-header@
    -- and @query-string@.
    conditions :: Prelude.Maybe [RuleCondition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { isDefault = Prelude.Nothing,
      ruleArn = Prelude.Nothing,
      actions = Prelude.Nothing,
      priority = Prelude.Nothing,
      conditions = Prelude.Nothing
    }

-- | Indicates whether this is the default rule.
rule_isDefault :: Lens.Lens' Rule (Prelude.Maybe Prelude.Bool)
rule_isDefault = Lens.lens (\Rule' {isDefault} -> isDefault) (\s@Rule' {} a -> s {isDefault = a} :: Rule)

-- | The Amazon Resource Name (ARN) of the rule.
rule_ruleArn :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_ruleArn = Lens.lens (\Rule' {ruleArn} -> ruleArn) (\s@Rule' {} a -> s {ruleArn = a} :: Rule)

-- | The actions. Each rule must include exactly one of the following types
-- of actions: @forward@, @redirect@, or @fixed-response@, and it must be
-- the last action to be performed.
rule_actions :: Lens.Lens' Rule (Prelude.Maybe [Action])
rule_actions = Lens.lens (\Rule' {actions} -> actions) (\s@Rule' {} a -> s {actions = a} :: Rule) Prelude.. Lens.mapping Prelude._Coerce

-- | The priority.
rule_priority :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_priority = Lens.lens (\Rule' {priority} -> priority) (\s@Rule' {} a -> s {priority = a} :: Rule)

-- | The conditions. Each rule can include zero or one of the following
-- conditions: @http-request-method@, @host-header@, @path-pattern@, and
-- @source-ip@, and zero or more of the following conditions: @http-header@
-- and @query-string@.
rule_conditions :: Lens.Lens' Rule (Prelude.Maybe [RuleCondition])
rule_conditions = Lens.lens (\Rule' {conditions} -> conditions) (\s@Rule' {} a -> s {conditions = a} :: Rule) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML Rule where
  parseXML x =
    Rule'
      Prelude.<$> (x Prelude..@? "IsDefault")
      Prelude.<*> (x Prelude..@? "RuleArn")
      Prelude.<*> ( x Prelude..@? "Actions" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "Priority")
      Prelude.<*> ( x Prelude..@? "Conditions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable Rule

instance Prelude.NFData Rule
