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
-- Module      : Amazonka.NetworkFirewall.Types.RuleDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.MatchAttributes
import qualified Amazonka.Prelude as Prelude

-- | The inspection criteria and action for a single stateless rule. Network
-- Firewall inspects each packet for the specified matching criteria. When
-- a packet matches the criteria, Network Firewall performs the rule\'s
-- actions on the packet.
--
-- /See:/ 'newRuleDefinition' smart constructor.
data RuleDefinition = RuleDefinition'
  { -- | Criteria for Network Firewall to use to inspect an individual packet in
    -- stateless rule inspection. Each match attributes set can include one or
    -- more items such as IP address, CIDR range, port number, protocol, and
    -- TCP flags.
    matchAttributes :: MatchAttributes,
    -- | The actions to take on a packet that matches one of the stateless rule
    -- definition\'s match attributes. You must specify a standard action and
    -- you can add custom actions.
    --
    -- Network Firewall only forwards a packet for stateful rule inspection if
    -- you specify @aws:forward_to_sfe@ for a rule that the packet matches, or
    -- if the packet doesn\'t match any stateless rule and you specify
    -- @aws:forward_to_sfe@ for the @StatelessDefaultActions@ setting for the
    -- FirewallPolicy.
    --
    -- For every rule, you must specify exactly one of the following standard
    -- actions.
    --
    -- -   __aws:pass__ - Discontinues all inspection of the packet and permits
    --     it to go to its intended destination.
    --
    -- -   __aws:drop__ - Discontinues all inspection of the packet and blocks
    --     it from going to its intended destination.
    --
    -- -   __aws:forward_to_sfe__ - Discontinues stateless inspection of the
    --     packet and forwards it to the stateful rule engine for inspection.
    --
    -- Additionally, you can specify a custom action. To do this, you define a
    -- custom action by name and type, then provide the name you\'ve assigned
    -- to the action in this @Actions@ setting. For information about the
    -- options, see CustomAction.
    --
    -- To provide more than one action in this setting, separate the settings
    -- with a comma. For example, if you have a custom @PublishMetrics@ action
    -- that you\'ve named @MyMetricsAction@, then you could specify the
    -- standard action @aws:pass@ and the custom action with
    -- @[“aws:pass”, “MyMetricsAction”]@.
    actions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchAttributes', 'ruleDefinition_matchAttributes' - Criteria for Network Firewall to use to inspect an individual packet in
-- stateless rule inspection. Each match attributes set can include one or
-- more items such as IP address, CIDR range, port number, protocol, and
-- TCP flags.
--
-- 'actions', 'ruleDefinition_actions' - The actions to take on a packet that matches one of the stateless rule
-- definition\'s match attributes. You must specify a standard action and
-- you can add custom actions.
--
-- Network Firewall only forwards a packet for stateful rule inspection if
-- you specify @aws:forward_to_sfe@ for a rule that the packet matches, or
-- if the packet doesn\'t match any stateless rule and you specify
-- @aws:forward_to_sfe@ for the @StatelessDefaultActions@ setting for the
-- FirewallPolicy.
--
-- For every rule, you must specify exactly one of the following standard
-- actions.
--
-- -   __aws:pass__ - Discontinues all inspection of the packet and permits
--     it to go to its intended destination.
--
-- -   __aws:drop__ - Discontinues all inspection of the packet and blocks
--     it from going to its intended destination.
--
-- -   __aws:forward_to_sfe__ - Discontinues stateless inspection of the
--     packet and forwards it to the stateful rule engine for inspection.
--
-- Additionally, you can specify a custom action. To do this, you define a
-- custom action by name and type, then provide the name you\'ve assigned
-- to the action in this @Actions@ setting. For information about the
-- options, see CustomAction.
--
-- To provide more than one action in this setting, separate the settings
-- with a comma. For example, if you have a custom @PublishMetrics@ action
-- that you\'ve named @MyMetricsAction@, then you could specify the
-- standard action @aws:pass@ and the custom action with
-- @[“aws:pass”, “MyMetricsAction”]@.
newRuleDefinition ::
  -- | 'matchAttributes'
  MatchAttributes ->
  RuleDefinition
newRuleDefinition pMatchAttributes_ =
  RuleDefinition'
    { matchAttributes =
        pMatchAttributes_,
      actions = Prelude.mempty
    }

-- | Criteria for Network Firewall to use to inspect an individual packet in
-- stateless rule inspection. Each match attributes set can include one or
-- more items such as IP address, CIDR range, port number, protocol, and
-- TCP flags.
ruleDefinition_matchAttributes :: Lens.Lens' RuleDefinition MatchAttributes
ruleDefinition_matchAttributes = Lens.lens (\RuleDefinition' {matchAttributes} -> matchAttributes) (\s@RuleDefinition' {} a -> s {matchAttributes = a} :: RuleDefinition)

-- | The actions to take on a packet that matches one of the stateless rule
-- definition\'s match attributes. You must specify a standard action and
-- you can add custom actions.
--
-- Network Firewall only forwards a packet for stateful rule inspection if
-- you specify @aws:forward_to_sfe@ for a rule that the packet matches, or
-- if the packet doesn\'t match any stateless rule and you specify
-- @aws:forward_to_sfe@ for the @StatelessDefaultActions@ setting for the
-- FirewallPolicy.
--
-- For every rule, you must specify exactly one of the following standard
-- actions.
--
-- -   __aws:pass__ - Discontinues all inspection of the packet and permits
--     it to go to its intended destination.
--
-- -   __aws:drop__ - Discontinues all inspection of the packet and blocks
--     it from going to its intended destination.
--
-- -   __aws:forward_to_sfe__ - Discontinues stateless inspection of the
--     packet and forwards it to the stateful rule engine for inspection.
--
-- Additionally, you can specify a custom action. To do this, you define a
-- custom action by name and type, then provide the name you\'ve assigned
-- to the action in this @Actions@ setting. For information about the
-- options, see CustomAction.
--
-- To provide more than one action in this setting, separate the settings
-- with a comma. For example, if you have a custom @PublishMetrics@ action
-- that you\'ve named @MyMetricsAction@, then you could specify the
-- standard action @aws:pass@ and the custom action with
-- @[“aws:pass”, “MyMetricsAction”]@.
ruleDefinition_actions :: Lens.Lens' RuleDefinition [Prelude.Text]
ruleDefinition_actions = Lens.lens (\RuleDefinition' {actions} -> actions) (\s@RuleDefinition' {} a -> s {actions = a} :: RuleDefinition) Prelude.. Lens.coerced

instance Data.FromJSON RuleDefinition where
  parseJSON =
    Data.withObject
      "RuleDefinition"
      ( \x ->
          RuleDefinition'
            Prelude.<$> (x Data..: "MatchAttributes")
            Prelude.<*> (x Data..:? "Actions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RuleDefinition where
  hashWithSalt _salt RuleDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` matchAttributes
      `Prelude.hashWithSalt` actions

instance Prelude.NFData RuleDefinition where
  rnf RuleDefinition' {..} =
    Prelude.rnf matchAttributes
      `Prelude.seq` Prelude.rnf actions

instance Data.ToJSON RuleDefinition where
  toJSON RuleDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MatchAttributes" Data..= matchAttributes),
            Prelude.Just ("Actions" Data..= actions)
          ]
      )
