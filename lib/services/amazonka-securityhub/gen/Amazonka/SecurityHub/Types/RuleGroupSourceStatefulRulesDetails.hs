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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesHeaderDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesOptionsDetails

-- | A Suricata rule specification.
--
-- /See:/ 'newRuleGroupSourceStatefulRulesDetails' smart constructor.
data RuleGroupSourceStatefulRulesDetails = RuleGroupSourceStatefulRulesDetails'
  { -- | The stateful inspection criteria for the rule.
    header :: Prelude.Maybe RuleGroupSourceStatefulRulesHeaderDetails,
    -- | Additional options for the rule.
    ruleOptions :: Prelude.Maybe [RuleGroupSourceStatefulRulesOptionsDetails],
    -- | Defines what Network Firewall should do with the packets in a traffic
    -- flow when the flow matches the stateful rule criteria.
    action :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatefulRulesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'header', 'ruleGroupSourceStatefulRulesDetails_header' - The stateful inspection criteria for the rule.
--
-- 'ruleOptions', 'ruleGroupSourceStatefulRulesDetails_ruleOptions' - Additional options for the rule.
--
-- 'action', 'ruleGroupSourceStatefulRulesDetails_action' - Defines what Network Firewall should do with the packets in a traffic
-- flow when the flow matches the stateful rule criteria.
newRuleGroupSourceStatefulRulesDetails ::
  RuleGroupSourceStatefulRulesDetails
newRuleGroupSourceStatefulRulesDetails =
  RuleGroupSourceStatefulRulesDetails'
    { header =
        Prelude.Nothing,
      ruleOptions = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The stateful inspection criteria for the rule.
ruleGroupSourceStatefulRulesDetails_header :: Lens.Lens' RuleGroupSourceStatefulRulesDetails (Prelude.Maybe RuleGroupSourceStatefulRulesHeaderDetails)
ruleGroupSourceStatefulRulesDetails_header = Lens.lens (\RuleGroupSourceStatefulRulesDetails' {header} -> header) (\s@RuleGroupSourceStatefulRulesDetails' {} a -> s {header = a} :: RuleGroupSourceStatefulRulesDetails)

-- | Additional options for the rule.
ruleGroupSourceStatefulRulesDetails_ruleOptions :: Lens.Lens' RuleGroupSourceStatefulRulesDetails (Prelude.Maybe [RuleGroupSourceStatefulRulesOptionsDetails])
ruleGroupSourceStatefulRulesDetails_ruleOptions = Lens.lens (\RuleGroupSourceStatefulRulesDetails' {ruleOptions} -> ruleOptions) (\s@RuleGroupSourceStatefulRulesDetails' {} a -> s {ruleOptions = a} :: RuleGroupSourceStatefulRulesDetails) Prelude.. Lens.mapping Lens.coerced

-- | Defines what Network Firewall should do with the packets in a traffic
-- flow when the flow matches the stateful rule criteria.
ruleGroupSourceStatefulRulesDetails_action :: Lens.Lens' RuleGroupSourceStatefulRulesDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatefulRulesDetails_action = Lens.lens (\RuleGroupSourceStatefulRulesDetails' {action} -> action) (\s@RuleGroupSourceStatefulRulesDetails' {} a -> s {action = a} :: RuleGroupSourceStatefulRulesDetails)

instance
  Core.FromJSON
    RuleGroupSourceStatefulRulesDetails
  where
  parseJSON =
    Core.withObject
      "RuleGroupSourceStatefulRulesDetails"
      ( \x ->
          RuleGroupSourceStatefulRulesDetails'
            Prelude.<$> (x Core..:? "Header")
            Prelude.<*> (x Core..:? "RuleOptions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Action")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatefulRulesDetails
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatefulRulesDetails' {..} =
      _salt `Prelude.hashWithSalt` header
        `Prelude.hashWithSalt` ruleOptions
        `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    RuleGroupSourceStatefulRulesDetails
  where
  rnf RuleGroupSourceStatefulRulesDetails' {..} =
    Prelude.rnf header
      `Prelude.seq` Prelude.rnf ruleOptions
      `Prelude.seq` Prelude.rnf action

instance
  Core.ToJSON
    RuleGroupSourceStatefulRulesDetails
  where
  toJSON RuleGroupSourceStatefulRulesDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Header" Core..=) Prelude.<$> header,
            ("RuleOptions" Core..=) Prelude.<$> ruleOptions,
            ("Action" Core..=) Prelude.<$> action
          ]
      )
