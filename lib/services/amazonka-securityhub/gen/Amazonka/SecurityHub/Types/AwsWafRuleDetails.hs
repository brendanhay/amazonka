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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRuleDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRuleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRulePredicateListDetails

-- | Provides information about a WAF rule. This rule specifies the web
-- requests that you want to allow, block, or count.
--
-- /See:/ 'newAwsWafRuleDetails' smart constructor.
data AwsWafRuleDetails = AwsWafRuleDetails'
  { -- | The name of the metrics for this rule.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | A descriptive name for the rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
    -- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
    -- objects that you want to add to a rule and, for each object, indicates
    -- whether you want to negate the settings.
    predicateList :: Prelude.Maybe [AwsWafRulePredicateListDetails],
    -- | The ID of the WAF rule.
    ruleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRuleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'awsWafRuleDetails_metricName' - The name of the metrics for this rule.
--
-- 'name', 'awsWafRuleDetails_name' - A descriptive name for the rule.
--
-- 'predicateList', 'awsWafRuleDetails_predicateList' - Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
-- objects that you want to add to a rule and, for each object, indicates
-- whether you want to negate the settings.
--
-- 'ruleId', 'awsWafRuleDetails_ruleId' - The ID of the WAF rule.
newAwsWafRuleDetails ::
  AwsWafRuleDetails
newAwsWafRuleDetails =
  AwsWafRuleDetails'
    { metricName = Prelude.Nothing,
      name = Prelude.Nothing,
      predicateList = Prelude.Nothing,
      ruleId = Prelude.Nothing
    }

-- | The name of the metrics for this rule.
awsWafRuleDetails_metricName :: Lens.Lens' AwsWafRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRuleDetails_metricName = Lens.lens (\AwsWafRuleDetails' {metricName} -> metricName) (\s@AwsWafRuleDetails' {} a -> s {metricName = a} :: AwsWafRuleDetails)

-- | A descriptive name for the rule.
awsWafRuleDetails_name :: Lens.Lens' AwsWafRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRuleDetails_name = Lens.lens (\AwsWafRuleDetails' {name} -> name) (\s@AwsWafRuleDetails' {} a -> s {name = a} :: AwsWafRuleDetails)

-- | Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
-- objects that you want to add to a rule and, for each object, indicates
-- whether you want to negate the settings.
awsWafRuleDetails_predicateList :: Lens.Lens' AwsWafRuleDetails (Prelude.Maybe [AwsWafRulePredicateListDetails])
awsWafRuleDetails_predicateList = Lens.lens (\AwsWafRuleDetails' {predicateList} -> predicateList) (\s@AwsWafRuleDetails' {} a -> s {predicateList = a} :: AwsWafRuleDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the WAF rule.
awsWafRuleDetails_ruleId :: Lens.Lens' AwsWafRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRuleDetails_ruleId = Lens.lens (\AwsWafRuleDetails' {ruleId} -> ruleId) (\s@AwsWafRuleDetails' {} a -> s {ruleId = a} :: AwsWafRuleDetails)

instance Data.FromJSON AwsWafRuleDetails where
  parseJSON =
    Data.withObject
      "AwsWafRuleDetails"
      ( \x ->
          AwsWafRuleDetails'
            Prelude.<$> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PredicateList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RuleId")
      )

instance Prelude.Hashable AwsWafRuleDetails where
  hashWithSalt _salt AwsWafRuleDetails' {..} =
    _salt
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` predicateList
      `Prelude.hashWithSalt` ruleId

instance Prelude.NFData AwsWafRuleDetails where
  rnf AwsWafRuleDetails' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf predicateList
      `Prelude.seq` Prelude.rnf ruleId

instance Data.ToJSON AwsWafRuleDetails where
  toJSON AwsWafRuleDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MetricName" Data..=) Prelude.<$> metricName,
            ("Name" Data..=) Prelude.<$> name,
            ("PredicateList" Data..=) Prelude.<$> predicateList,
            ("RuleId" Data..=) Prelude.<$> ruleId
          ]
      )
