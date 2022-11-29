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
-- Module      : Amazonka.NetworkFirewall.Types.StatelessRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatelessRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkFirewall.Types.RuleDefinition
import qualified Amazonka.Prelude as Prelude

-- | A single stateless rule. This is used in StatelessRulesAndCustomActions.
--
-- /See:/ 'newStatelessRule' smart constructor.
data StatelessRule = StatelessRule'
  { -- | Defines the stateless 5-tuple packet inspection criteria and the action
    -- to take on a packet that matches the criteria.
    ruleDefinition :: RuleDefinition,
    -- | Indicates the order in which to run this rule relative to all of the
    -- rules that are defined for a stateless rule group. Network Firewall
    -- evaluates the rules in a rule group starting with the lowest priority
    -- setting. You must ensure that the priority settings are unique for the
    -- rule group.
    --
    -- Each stateless rule group uses exactly one
    -- @StatelessRulesAndCustomActions@ object, and each
    -- @StatelessRulesAndCustomActions@ contains exactly one @StatelessRules@
    -- object. To ensure unique priority settings for your rule groups, set
    -- unique priorities for the stateless rules that you define inside any
    -- single @StatelessRules@ object.
    --
    -- You can change the priority settings of your rules at any time. To make
    -- it easier to insert rules later, number them so there\'s a wide range in
    -- between, for example use 100, 200, and so on.
    priority :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatelessRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleDefinition', 'statelessRule_ruleDefinition' - Defines the stateless 5-tuple packet inspection criteria and the action
-- to take on a packet that matches the criteria.
--
-- 'priority', 'statelessRule_priority' - Indicates the order in which to run this rule relative to all of the
-- rules that are defined for a stateless rule group. Network Firewall
-- evaluates the rules in a rule group starting with the lowest priority
-- setting. You must ensure that the priority settings are unique for the
-- rule group.
--
-- Each stateless rule group uses exactly one
-- @StatelessRulesAndCustomActions@ object, and each
-- @StatelessRulesAndCustomActions@ contains exactly one @StatelessRules@
-- object. To ensure unique priority settings for your rule groups, set
-- unique priorities for the stateless rules that you define inside any
-- single @StatelessRules@ object.
--
-- You can change the priority settings of your rules at any time. To make
-- it easier to insert rules later, number them so there\'s a wide range in
-- between, for example use 100, 200, and so on.
newStatelessRule ::
  -- | 'ruleDefinition'
  RuleDefinition ->
  -- | 'priority'
  Prelude.Natural ->
  StatelessRule
newStatelessRule pRuleDefinition_ pPriority_ =
  StatelessRule'
    { ruleDefinition = pRuleDefinition_,
      priority = pPriority_
    }

-- | Defines the stateless 5-tuple packet inspection criteria and the action
-- to take on a packet that matches the criteria.
statelessRule_ruleDefinition :: Lens.Lens' StatelessRule RuleDefinition
statelessRule_ruleDefinition = Lens.lens (\StatelessRule' {ruleDefinition} -> ruleDefinition) (\s@StatelessRule' {} a -> s {ruleDefinition = a} :: StatelessRule)

-- | Indicates the order in which to run this rule relative to all of the
-- rules that are defined for a stateless rule group. Network Firewall
-- evaluates the rules in a rule group starting with the lowest priority
-- setting. You must ensure that the priority settings are unique for the
-- rule group.
--
-- Each stateless rule group uses exactly one
-- @StatelessRulesAndCustomActions@ object, and each
-- @StatelessRulesAndCustomActions@ contains exactly one @StatelessRules@
-- object. To ensure unique priority settings for your rule groups, set
-- unique priorities for the stateless rules that you define inside any
-- single @StatelessRules@ object.
--
-- You can change the priority settings of your rules at any time. To make
-- it easier to insert rules later, number them so there\'s a wide range in
-- between, for example use 100, 200, and so on.
statelessRule_priority :: Lens.Lens' StatelessRule Prelude.Natural
statelessRule_priority = Lens.lens (\StatelessRule' {priority} -> priority) (\s@StatelessRule' {} a -> s {priority = a} :: StatelessRule)

instance Core.FromJSON StatelessRule where
  parseJSON =
    Core.withObject
      "StatelessRule"
      ( \x ->
          StatelessRule'
            Prelude.<$> (x Core..: "RuleDefinition")
            Prelude.<*> (x Core..: "Priority")
      )

instance Prelude.Hashable StatelessRule where
  hashWithSalt _salt StatelessRule' {..} =
    _salt `Prelude.hashWithSalt` ruleDefinition
      `Prelude.hashWithSalt` priority

instance Prelude.NFData StatelessRule where
  rnf StatelessRule' {..} =
    Prelude.rnf ruleDefinition
      `Prelude.seq` Prelude.rnf priority

instance Core.ToJSON StatelessRule where
  toJSON StatelessRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RuleDefinition" Core..= ruleDefinition),
            Prelude.Just ("Priority" Core..= priority)
          ]
      )
