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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesActionDetails

-- | Provides information about the rules attached to a rule group
--
-- /See:/ 'newAwsWafRegionalRuleGroupRulesDetails' smart constructor.
data AwsWafRegionalRuleGroupRulesDetails = AwsWafRegionalRuleGroupRulesDetails'
  { -- | The action that WAF should take on a web request when it matches the
    -- criteria defined in the rule.
    action :: Prelude.Maybe AwsWafRegionalRuleGroupRulesActionDetails,
    -- | If you define more than one rule in a web ACL, WAF evaluates each
    -- request against the rules in order based on the value of @Priority@.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The ID for a rule.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The type of rule in the rule group.
    type' :: Prelude.Maybe Prelude.Text
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
-- 'action', 'awsWafRegionalRuleGroupRulesDetails_action' - The action that WAF should take on a web request when it matches the
-- criteria defined in the rule.
--
-- 'priority', 'awsWafRegionalRuleGroupRulesDetails_priority' - If you define more than one rule in a web ACL, WAF evaluates each
-- request against the rules in order based on the value of @Priority@.
--
-- 'ruleId', 'awsWafRegionalRuleGroupRulesDetails_ruleId' - The ID for a rule.
--
-- 'type'', 'awsWafRegionalRuleGroupRulesDetails_type' - The type of rule in the rule group.
newAwsWafRegionalRuleGroupRulesDetails ::
  AwsWafRegionalRuleGroupRulesDetails
newAwsWafRegionalRuleGroupRulesDetails =
  AwsWafRegionalRuleGroupRulesDetails'
    { action =
        Prelude.Nothing,
      priority = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The action that WAF should take on a web request when it matches the
-- criteria defined in the rule.
awsWafRegionalRuleGroupRulesDetails_action :: Lens.Lens' AwsWafRegionalRuleGroupRulesDetails (Prelude.Maybe AwsWafRegionalRuleGroupRulesActionDetails)
awsWafRegionalRuleGroupRulesDetails_action = Lens.lens (\AwsWafRegionalRuleGroupRulesDetails' {action} -> action) (\s@AwsWafRegionalRuleGroupRulesDetails' {} a -> s {action = a} :: AwsWafRegionalRuleGroupRulesDetails)

-- | If you define more than one rule in a web ACL, WAF evaluates each
-- request against the rules in order based on the value of @Priority@.
awsWafRegionalRuleGroupRulesDetails_priority :: Lens.Lens' AwsWafRegionalRuleGroupRulesDetails (Prelude.Maybe Prelude.Int)
awsWafRegionalRuleGroupRulesDetails_priority = Lens.lens (\AwsWafRegionalRuleGroupRulesDetails' {priority} -> priority) (\s@AwsWafRegionalRuleGroupRulesDetails' {} a -> s {priority = a} :: AwsWafRegionalRuleGroupRulesDetails)

-- | The ID for a rule.
awsWafRegionalRuleGroupRulesDetails_ruleId :: Lens.Lens' AwsWafRegionalRuleGroupRulesDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRuleGroupRulesDetails_ruleId = Lens.lens (\AwsWafRegionalRuleGroupRulesDetails' {ruleId} -> ruleId) (\s@AwsWafRegionalRuleGroupRulesDetails' {} a -> s {ruleId = a} :: AwsWafRegionalRuleGroupRulesDetails)

-- | The type of rule in the rule group.
awsWafRegionalRuleGroupRulesDetails_type :: Lens.Lens' AwsWafRegionalRuleGroupRulesDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRuleGroupRulesDetails_type = Lens.lens (\AwsWafRegionalRuleGroupRulesDetails' {type'} -> type') (\s@AwsWafRegionalRuleGroupRulesDetails' {} a -> s {type' = a} :: AwsWafRegionalRuleGroupRulesDetails)

instance
  Data.FromJSON
    AwsWafRegionalRuleGroupRulesDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafRegionalRuleGroupRulesDetails"
      ( \x ->
          AwsWafRegionalRuleGroupRulesDetails'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "RuleId")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRegionalRuleGroupRulesDetails
  where
  hashWithSalt
    _salt
    AwsWafRegionalRuleGroupRulesDetails' {..} =
      _salt
        `Prelude.hashWithSalt` action
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` ruleId
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRegionalRuleGroupRulesDetails
  where
  rnf AwsWafRegionalRuleGroupRulesDetails' {..} =
    Prelude.rnf action `Prelude.seq`
      Prelude.rnf priority `Prelude.seq`
        Prelude.rnf ruleId `Prelude.seq`
          Prelude.rnf type'

instance
  Data.ToJSON
    AwsWafRegionalRuleGroupRulesDetails
  where
  toJSON AwsWafRegionalRuleGroupRulesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("Priority" Data..=) Prelude.<$> priority,
            ("RuleId" Data..=) Prelude.<$> ruleId,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
