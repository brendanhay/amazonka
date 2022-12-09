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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesActionDetails

-- | Provides information about the rules attached to the rule group. These
-- rules identify the web requests that you want to allow, block, or count.
--
-- /See:/ 'newAwsWafRuleGroupRulesDetails' smart constructor.
data AwsWafRuleGroupRulesDetails = AwsWafRuleGroupRulesDetails'
  { -- | Provides information about what action WAF should take on a web request
    -- when it matches the criteria defined in the rule.
    action :: Prelude.Maybe AwsWafRuleGroupRulesActionDetails,
    -- | If you define more than one rule in a web ACL, WAF evaluates each
    -- request against the rules in order based on the value of @Priority@.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The rule ID for a rule.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The type of rule.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRuleGroupRulesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'awsWafRuleGroupRulesDetails_action' - Provides information about what action WAF should take on a web request
-- when it matches the criteria defined in the rule.
--
-- 'priority', 'awsWafRuleGroupRulesDetails_priority' - If you define more than one rule in a web ACL, WAF evaluates each
-- request against the rules in order based on the value of @Priority@.
--
-- 'ruleId', 'awsWafRuleGroupRulesDetails_ruleId' - The rule ID for a rule.
--
-- 'type'', 'awsWafRuleGroupRulesDetails_type' - The type of rule.
newAwsWafRuleGroupRulesDetails ::
  AwsWafRuleGroupRulesDetails
newAwsWafRuleGroupRulesDetails =
  AwsWafRuleGroupRulesDetails'
    { action =
        Prelude.Nothing,
      priority = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Provides information about what action WAF should take on a web request
-- when it matches the criteria defined in the rule.
awsWafRuleGroupRulesDetails_action :: Lens.Lens' AwsWafRuleGroupRulesDetails (Prelude.Maybe AwsWafRuleGroupRulesActionDetails)
awsWafRuleGroupRulesDetails_action = Lens.lens (\AwsWafRuleGroupRulesDetails' {action} -> action) (\s@AwsWafRuleGroupRulesDetails' {} a -> s {action = a} :: AwsWafRuleGroupRulesDetails)

-- | If you define more than one rule in a web ACL, WAF evaluates each
-- request against the rules in order based on the value of @Priority@.
awsWafRuleGroupRulesDetails_priority :: Lens.Lens' AwsWafRuleGroupRulesDetails (Prelude.Maybe Prelude.Int)
awsWafRuleGroupRulesDetails_priority = Lens.lens (\AwsWafRuleGroupRulesDetails' {priority} -> priority) (\s@AwsWafRuleGroupRulesDetails' {} a -> s {priority = a} :: AwsWafRuleGroupRulesDetails)

-- | The rule ID for a rule.
awsWafRuleGroupRulesDetails_ruleId :: Lens.Lens' AwsWafRuleGroupRulesDetails (Prelude.Maybe Prelude.Text)
awsWafRuleGroupRulesDetails_ruleId = Lens.lens (\AwsWafRuleGroupRulesDetails' {ruleId} -> ruleId) (\s@AwsWafRuleGroupRulesDetails' {} a -> s {ruleId = a} :: AwsWafRuleGroupRulesDetails)

-- | The type of rule.
awsWafRuleGroupRulesDetails_type :: Lens.Lens' AwsWafRuleGroupRulesDetails (Prelude.Maybe Prelude.Text)
awsWafRuleGroupRulesDetails_type = Lens.lens (\AwsWafRuleGroupRulesDetails' {type'} -> type') (\s@AwsWafRuleGroupRulesDetails' {} a -> s {type' = a} :: AwsWafRuleGroupRulesDetails)

instance Data.FromJSON AwsWafRuleGroupRulesDetails where
  parseJSON =
    Data.withObject
      "AwsWafRuleGroupRulesDetails"
      ( \x ->
          AwsWafRuleGroupRulesDetails'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "RuleId")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable AwsWafRuleGroupRulesDetails where
  hashWithSalt _salt AwsWafRuleGroupRulesDetails' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AwsWafRuleGroupRulesDetails where
  rnf AwsWafRuleGroupRulesDetails' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON AwsWafRuleGroupRulesDetails where
  toJSON AwsWafRuleGroupRulesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("Priority" Data..=) Prelude.<$> priority,
            ("RuleId" Data..=) Prelude.<$> ruleId,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
