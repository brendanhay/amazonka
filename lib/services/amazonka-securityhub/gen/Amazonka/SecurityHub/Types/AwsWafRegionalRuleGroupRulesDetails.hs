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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesActionDetails

-- | Provides information about the rules attached to a rule group
--
-- /See:/ 'newAwsWafRegionalRuleGroupRulesDetails' smart constructor.
data AwsWafRegionalRuleGroupRulesDetails = AwsWafRegionalRuleGroupRulesDetails'
  { -- | The type of rule in the rule group.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The ID for a rule.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | If you define more than one rule in a web ACL, WAF evaluates each
    -- request against the rules in order based on the value of @Priority@.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The action that WAF should take on a web request when it matches the
    -- criteria defined in the rule.
    action :: Prelude.Maybe AwsWafRegionalRuleGroupRulesActionDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalRuleGroupRulesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsWafRegionalRuleGroupRulesDetails_type' - The type of rule in the rule group.
--
-- 'ruleId', 'awsWafRegionalRuleGroupRulesDetails_ruleId' - The ID for a rule.
--
-- 'priority', 'awsWafRegionalRuleGroupRulesDetails_priority' - If you define more than one rule in a web ACL, WAF evaluates each
-- request against the rules in order based on the value of @Priority@.
--
-- 'action', 'awsWafRegionalRuleGroupRulesDetails_action' - The action that WAF should take on a web request when it matches the
-- criteria defined in the rule.
newAwsWafRegionalRuleGroupRulesDetails ::
  AwsWafRegionalRuleGroupRulesDetails
newAwsWafRegionalRuleGroupRulesDetails =
  AwsWafRegionalRuleGroupRulesDetails'
    { type' =
        Prelude.Nothing,
      ruleId = Prelude.Nothing,
      priority = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The type of rule in the rule group.
awsWafRegionalRuleGroupRulesDetails_type :: Lens.Lens' AwsWafRegionalRuleGroupRulesDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRuleGroupRulesDetails_type = Lens.lens (\AwsWafRegionalRuleGroupRulesDetails' {type'} -> type') (\s@AwsWafRegionalRuleGroupRulesDetails' {} a -> s {type' = a} :: AwsWafRegionalRuleGroupRulesDetails)

-- | The ID for a rule.
awsWafRegionalRuleGroupRulesDetails_ruleId :: Lens.Lens' AwsWafRegionalRuleGroupRulesDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRuleGroupRulesDetails_ruleId = Lens.lens (\AwsWafRegionalRuleGroupRulesDetails' {ruleId} -> ruleId) (\s@AwsWafRegionalRuleGroupRulesDetails' {} a -> s {ruleId = a} :: AwsWafRegionalRuleGroupRulesDetails)

-- | If you define more than one rule in a web ACL, WAF evaluates each
-- request against the rules in order based on the value of @Priority@.
awsWafRegionalRuleGroupRulesDetails_priority :: Lens.Lens' AwsWafRegionalRuleGroupRulesDetails (Prelude.Maybe Prelude.Int)
awsWafRegionalRuleGroupRulesDetails_priority = Lens.lens (\AwsWafRegionalRuleGroupRulesDetails' {priority} -> priority) (\s@AwsWafRegionalRuleGroupRulesDetails' {} a -> s {priority = a} :: AwsWafRegionalRuleGroupRulesDetails)

-- | The action that WAF should take on a web request when it matches the
-- criteria defined in the rule.
awsWafRegionalRuleGroupRulesDetails_action :: Lens.Lens' AwsWafRegionalRuleGroupRulesDetails (Prelude.Maybe AwsWafRegionalRuleGroupRulesActionDetails)
awsWafRegionalRuleGroupRulesDetails_action = Lens.lens (\AwsWafRegionalRuleGroupRulesDetails' {action} -> action) (\s@AwsWafRegionalRuleGroupRulesDetails' {} a -> s {action = a} :: AwsWafRegionalRuleGroupRulesDetails)

instance
  Core.FromJSON
    AwsWafRegionalRuleGroupRulesDetails
  where
  parseJSON =
    Core.withObject
      "AwsWafRegionalRuleGroupRulesDetails"
      ( \x ->
          AwsWafRegionalRuleGroupRulesDetails'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "RuleId")
            Prelude.<*> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "Action")
      )

instance
  Prelude.Hashable
    AwsWafRegionalRuleGroupRulesDetails
  where
  hashWithSalt
    _salt
    AwsWafRegionalRuleGroupRulesDetails' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` ruleId
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    AwsWafRegionalRuleGroupRulesDetails
  where
  rnf AwsWafRegionalRuleGroupRulesDetails' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf action

instance
  Core.ToJSON
    AwsWafRegionalRuleGroupRulesDetails
  where
  toJSON AwsWafRegionalRuleGroupRulesDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Type" Core..=) Prelude.<$> type',
            ("RuleId" Core..=) Prelude.<$> ruleId,
            ("Priority" Core..=) Prelude.<$> priority,
            ("Action" Core..=) Prelude.<$> action
          ]
      )
