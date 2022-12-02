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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRuleGroupDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRuleGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesDetails

-- | Provides information about an WAF rule group. A rule group is a
-- collection of rules for inspecting and controlling web requests.
--
-- /See:/ 'newAwsWafRuleGroupDetails' smart constructor.
data AwsWafRuleGroupDetails = AwsWafRuleGroupDetails'
  { -- | The name of the rule group.
    name :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the rules attached to the rule group. These
    -- rules identify the web requests that you want to allow, block, or count.
    rules :: Prelude.Maybe [AwsWafRuleGroupRulesDetails],
    -- | The ID of the rule group.
    ruleGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the metrics for this rule group.
    metricName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRuleGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsWafRuleGroupDetails_name' - The name of the rule group.
--
-- 'rules', 'awsWafRuleGroupDetails_rules' - Provides information about the rules attached to the rule group. These
-- rules identify the web requests that you want to allow, block, or count.
--
-- 'ruleGroupId', 'awsWafRuleGroupDetails_ruleGroupId' - The ID of the rule group.
--
-- 'metricName', 'awsWafRuleGroupDetails_metricName' - The name of the metrics for this rule group.
newAwsWafRuleGroupDetails ::
  AwsWafRuleGroupDetails
newAwsWafRuleGroupDetails =
  AwsWafRuleGroupDetails'
    { name = Prelude.Nothing,
      rules = Prelude.Nothing,
      ruleGroupId = Prelude.Nothing,
      metricName = Prelude.Nothing
    }

-- | The name of the rule group.
awsWafRuleGroupDetails_name :: Lens.Lens' AwsWafRuleGroupDetails (Prelude.Maybe Prelude.Text)
awsWafRuleGroupDetails_name = Lens.lens (\AwsWafRuleGroupDetails' {name} -> name) (\s@AwsWafRuleGroupDetails' {} a -> s {name = a} :: AwsWafRuleGroupDetails)

-- | Provides information about the rules attached to the rule group. These
-- rules identify the web requests that you want to allow, block, or count.
awsWafRuleGroupDetails_rules :: Lens.Lens' AwsWafRuleGroupDetails (Prelude.Maybe [AwsWafRuleGroupRulesDetails])
awsWafRuleGroupDetails_rules = Lens.lens (\AwsWafRuleGroupDetails' {rules} -> rules) (\s@AwsWafRuleGroupDetails' {} a -> s {rules = a} :: AwsWafRuleGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the rule group.
awsWafRuleGroupDetails_ruleGroupId :: Lens.Lens' AwsWafRuleGroupDetails (Prelude.Maybe Prelude.Text)
awsWafRuleGroupDetails_ruleGroupId = Lens.lens (\AwsWafRuleGroupDetails' {ruleGroupId} -> ruleGroupId) (\s@AwsWafRuleGroupDetails' {} a -> s {ruleGroupId = a} :: AwsWafRuleGroupDetails)

-- | The name of the metrics for this rule group.
awsWafRuleGroupDetails_metricName :: Lens.Lens' AwsWafRuleGroupDetails (Prelude.Maybe Prelude.Text)
awsWafRuleGroupDetails_metricName = Lens.lens (\AwsWafRuleGroupDetails' {metricName} -> metricName) (\s@AwsWafRuleGroupDetails' {} a -> s {metricName = a} :: AwsWafRuleGroupDetails)

instance Data.FromJSON AwsWafRuleGroupDetails where
  parseJSON =
    Data.withObject
      "AwsWafRuleGroupDetails"
      ( \x ->
          AwsWafRuleGroupDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RuleGroupId")
            Prelude.<*> (x Data..:? "MetricName")
      )

instance Prelude.Hashable AwsWafRuleGroupDetails where
  hashWithSalt _salt AwsWafRuleGroupDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` ruleGroupId
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData AwsWafRuleGroupDetails where
  rnf AwsWafRuleGroupDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf ruleGroupId
      `Prelude.seq` Prelude.rnf metricName

instance Data.ToJSON AwsWafRuleGroupDetails where
  toJSON AwsWafRuleGroupDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Rules" Data..=) Prelude.<$> rules,
            ("RuleGroupId" Data..=) Prelude.<$> ruleGroupId,
            ("MetricName" Data..=) Prelude.<$> metricName
          ]
      )
