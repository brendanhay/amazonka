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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributes

-- | The definition of the stateless rule.
--
-- /See:/ 'newRuleGroupSourceStatelessRuleDefinition' smart constructor.
data RuleGroupSourceStatelessRuleDefinition = RuleGroupSourceStatelessRuleDefinition'
  { -- | The actions to take on a packet that matches one of the stateless rule
    -- definition\'s match attributes. You must specify a standard action
    -- (@aws:pass@, @aws:drop@, or @aws:forward_to_sfe@). You can then add
    -- custom actions.
    actions :: Prelude.Maybe [Prelude.Text],
    -- | The criteria for Network Firewall to use to inspect an individual packet
    -- in a stateless rule inspection.
    matchAttributes :: Prelude.Maybe RuleGroupSourceStatelessRuleMatchAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatelessRuleDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'ruleGroupSourceStatelessRuleDefinition_actions' - The actions to take on a packet that matches one of the stateless rule
-- definition\'s match attributes. You must specify a standard action
-- (@aws:pass@, @aws:drop@, or @aws:forward_to_sfe@). You can then add
-- custom actions.
--
-- 'matchAttributes', 'ruleGroupSourceStatelessRuleDefinition_matchAttributes' - The criteria for Network Firewall to use to inspect an individual packet
-- in a stateless rule inspection.
newRuleGroupSourceStatelessRuleDefinition ::
  RuleGroupSourceStatelessRuleDefinition
newRuleGroupSourceStatelessRuleDefinition =
  RuleGroupSourceStatelessRuleDefinition'
    { actions =
        Prelude.Nothing,
      matchAttributes = Prelude.Nothing
    }

-- | The actions to take on a packet that matches one of the stateless rule
-- definition\'s match attributes. You must specify a standard action
-- (@aws:pass@, @aws:drop@, or @aws:forward_to_sfe@). You can then add
-- custom actions.
ruleGroupSourceStatelessRuleDefinition_actions :: Lens.Lens' RuleGroupSourceStatelessRuleDefinition (Prelude.Maybe [Prelude.Text])
ruleGroupSourceStatelessRuleDefinition_actions = Lens.lens (\RuleGroupSourceStatelessRuleDefinition' {actions} -> actions) (\s@RuleGroupSourceStatelessRuleDefinition' {} a -> s {actions = a} :: RuleGroupSourceStatelessRuleDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The criteria for Network Firewall to use to inspect an individual packet
-- in a stateless rule inspection.
ruleGroupSourceStatelessRuleDefinition_matchAttributes :: Lens.Lens' RuleGroupSourceStatelessRuleDefinition (Prelude.Maybe RuleGroupSourceStatelessRuleMatchAttributes)
ruleGroupSourceStatelessRuleDefinition_matchAttributes = Lens.lens (\RuleGroupSourceStatelessRuleDefinition' {matchAttributes} -> matchAttributes) (\s@RuleGroupSourceStatelessRuleDefinition' {} a -> s {matchAttributes = a} :: RuleGroupSourceStatelessRuleDefinition)

instance
  Data.FromJSON
    RuleGroupSourceStatelessRuleDefinition
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatelessRuleDefinition"
      ( \x ->
          RuleGroupSourceStatelessRuleDefinition'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MatchAttributes")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRuleDefinition
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRuleDefinition' {..} =
      _salt `Prelude.hashWithSalt` actions
        `Prelude.hashWithSalt` matchAttributes

instance
  Prelude.NFData
    RuleGroupSourceStatelessRuleDefinition
  where
  rnf RuleGroupSourceStatelessRuleDefinition' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf matchAttributes

instance
  Data.ToJSON
    RuleGroupSourceStatelessRuleDefinition
  where
  toJSON RuleGroupSourceStatelessRuleDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("MatchAttributes" Data..=)
              Prelude.<$> matchAttributes
          ]
      )
