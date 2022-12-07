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
-- Module      : Amazonka.WAFV2.Types.RuleSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RuleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.RuleAction

-- | High-level information about a Rule, returned by operations like
-- DescribeManagedRuleGroup. This provides information like the ID, that
-- you can use to retrieve and manage a @RuleGroup@, and the ARN, that you
-- provide to the RuleGroupReferenceStatement to use the rule group in a
-- Rule.
--
-- /See:/ 'newRuleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { -- | The name of the rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The action that WAF should take on a web request when it matches a
    -- rule\'s statement. Settings at the web ACL level can override the rule
    -- action setting.
    action :: Prelude.Maybe RuleAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'ruleSummary_name' - The name of the rule.
--
-- 'action', 'ruleSummary_action' - The action that WAF should take on a web request when it matches a
-- rule\'s statement. Settings at the web ACL level can override the rule
-- action setting.
newRuleSummary ::
  RuleSummary
newRuleSummary =
  RuleSummary'
    { name = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The name of the rule.
ruleSummary_name :: Lens.Lens' RuleSummary (Prelude.Maybe Prelude.Text)
ruleSummary_name = Lens.lens (\RuleSummary' {name} -> name) (\s@RuleSummary' {} a -> s {name = a} :: RuleSummary)

-- | The action that WAF should take on a web request when it matches a
-- rule\'s statement. Settings at the web ACL level can override the rule
-- action setting.
ruleSummary_action :: Lens.Lens' RuleSummary (Prelude.Maybe RuleAction)
ruleSummary_action = Lens.lens (\RuleSummary' {action} -> action) (\s@RuleSummary' {} a -> s {action = a} :: RuleSummary)

instance Data.FromJSON RuleSummary where
  parseJSON =
    Data.withObject
      "RuleSummary"
      ( \x ->
          RuleSummary'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Action")
      )

instance Prelude.Hashable RuleSummary where
  hashWithSalt _salt RuleSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` action

instance Prelude.NFData RuleSummary where
  rnf RuleSummary' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf action
