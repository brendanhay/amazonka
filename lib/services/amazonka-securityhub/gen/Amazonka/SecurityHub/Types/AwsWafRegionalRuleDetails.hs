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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalRuleDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalRuleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRegionalRulePredicateListDetails

-- | Provides information about an WAF Regional rule. This rule identifies
-- the web requests that you want to allow, block, or count.
--
-- /See:/ 'newAwsWafRegionalRuleDetails' smart constructor.
data AwsWafRegionalRuleDetails = AwsWafRegionalRuleDetails'
  { -- | A name for the metrics for the rule.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | A descriptive name for the rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
    -- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
    -- objects that you want to add to a rule and, for each object, indicates
    -- whether you want to negate the settings.
    predicateList :: Prelude.Maybe [AwsWafRegionalRulePredicateListDetails],
    -- | The ID of the rule.
    ruleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalRuleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'awsWafRegionalRuleDetails_metricName' - A name for the metrics for the rule.
--
-- 'name', 'awsWafRegionalRuleDetails_name' - A descriptive name for the rule.
--
-- 'predicateList', 'awsWafRegionalRuleDetails_predicateList' - Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
-- objects that you want to add to a rule and, for each object, indicates
-- whether you want to negate the settings.
--
-- 'ruleId', 'awsWafRegionalRuleDetails_ruleId' - The ID of the rule.
newAwsWafRegionalRuleDetails ::
  AwsWafRegionalRuleDetails
newAwsWafRegionalRuleDetails =
  AwsWafRegionalRuleDetails'
    { metricName =
        Prelude.Nothing,
      name = Prelude.Nothing,
      predicateList = Prelude.Nothing,
      ruleId = Prelude.Nothing
    }

-- | A name for the metrics for the rule.
awsWafRegionalRuleDetails_metricName :: Lens.Lens' AwsWafRegionalRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRuleDetails_metricName = Lens.lens (\AwsWafRegionalRuleDetails' {metricName} -> metricName) (\s@AwsWafRegionalRuleDetails' {} a -> s {metricName = a} :: AwsWafRegionalRuleDetails)

-- | A descriptive name for the rule.
awsWafRegionalRuleDetails_name :: Lens.Lens' AwsWafRegionalRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRuleDetails_name = Lens.lens (\AwsWafRegionalRuleDetails' {name} -> name) (\s@AwsWafRegionalRuleDetails' {} a -> s {name = a} :: AwsWafRegionalRuleDetails)

-- | Specifies the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, and @SizeConstraintSet@
-- objects that you want to add to a rule and, for each object, indicates
-- whether you want to negate the settings.
awsWafRegionalRuleDetails_predicateList :: Lens.Lens' AwsWafRegionalRuleDetails (Prelude.Maybe [AwsWafRegionalRulePredicateListDetails])
awsWafRegionalRuleDetails_predicateList = Lens.lens (\AwsWafRegionalRuleDetails' {predicateList} -> predicateList) (\s@AwsWafRegionalRuleDetails' {} a -> s {predicateList = a} :: AwsWafRegionalRuleDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the rule.
awsWafRegionalRuleDetails_ruleId :: Lens.Lens' AwsWafRegionalRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRuleDetails_ruleId = Lens.lens (\AwsWafRegionalRuleDetails' {ruleId} -> ruleId) (\s@AwsWafRegionalRuleDetails' {} a -> s {ruleId = a} :: AwsWafRegionalRuleDetails)

instance Data.FromJSON AwsWafRegionalRuleDetails where
  parseJSON =
    Data.withObject
      "AwsWafRegionalRuleDetails"
      ( \x ->
          AwsWafRegionalRuleDetails'
            Prelude.<$> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PredicateList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RuleId")
      )

instance Prelude.Hashable AwsWafRegionalRuleDetails where
  hashWithSalt _salt AwsWafRegionalRuleDetails' {..} =
    _salt `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` predicateList
      `Prelude.hashWithSalt` ruleId

instance Prelude.NFData AwsWafRegionalRuleDetails where
  rnf AwsWafRegionalRuleDetails' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf predicateList
      `Prelude.seq` Prelude.rnf ruleId

instance Data.ToJSON AwsWafRegionalRuleDetails where
  toJSON AwsWafRegionalRuleDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MetricName" Data..=) Prelude.<$> metricName,
            ("Name" Data..=) Prelude.<$> name,
            ("PredicateList" Data..=) Prelude.<$> predicateList,
            ("RuleId" Data..=) Prelude.<$> ruleId
          ]
      )
