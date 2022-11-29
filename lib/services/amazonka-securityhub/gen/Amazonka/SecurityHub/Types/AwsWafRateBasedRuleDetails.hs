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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRateBasedRuleDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRateBasedRuleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRateBasedRuleMatchPredicate

-- | Details about a rate-based rule for global resources. A rate-based rule
-- provides settings to indicate when to allow, block, or count a request.
-- Rate-based rules include the number of requests that arrive over a
-- specified period of time.
--
-- /See:/ 'newAwsWafRateBasedRuleDetails' smart constructor.
data AwsWafRateBasedRuleDetails = AwsWafRateBasedRuleDetails'
  { -- | The name of the rate-based rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the rate-based rule.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The predicates to include in the rate-based rule.
    matchPredicates :: Prelude.Maybe [AwsWafRateBasedRuleMatchPredicate],
    -- | The maximum number of requests that have an identical value for the
    -- field specified in @RateKey@ that are allowed within a five-minute
    -- period. If the number of requests exceeds @RateLimit@ and the other
    -- predicates specified in the rule are met, WAF triggers the action for
    -- the rule.
    rateLimit :: Prelude.Maybe Prelude.Integer,
    -- | The name of the metrics for the rate-based rule.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The field that WAF uses to determine whether requests are likely
    -- arriving from single source and are subject to rate monitoring.
    rateKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRateBasedRuleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsWafRateBasedRuleDetails_name' - The name of the rate-based rule.
--
-- 'ruleId', 'awsWafRateBasedRuleDetails_ruleId' - The unique identifier for the rate-based rule.
--
-- 'matchPredicates', 'awsWafRateBasedRuleDetails_matchPredicates' - The predicates to include in the rate-based rule.
--
-- 'rateLimit', 'awsWafRateBasedRuleDetails_rateLimit' - The maximum number of requests that have an identical value for the
-- field specified in @RateKey@ that are allowed within a five-minute
-- period. If the number of requests exceeds @RateLimit@ and the other
-- predicates specified in the rule are met, WAF triggers the action for
-- the rule.
--
-- 'metricName', 'awsWafRateBasedRuleDetails_metricName' - The name of the metrics for the rate-based rule.
--
-- 'rateKey', 'awsWafRateBasedRuleDetails_rateKey' - The field that WAF uses to determine whether requests are likely
-- arriving from single source and are subject to rate monitoring.
newAwsWafRateBasedRuleDetails ::
  AwsWafRateBasedRuleDetails
newAwsWafRateBasedRuleDetails =
  AwsWafRateBasedRuleDetails'
    { name = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      matchPredicates = Prelude.Nothing,
      rateLimit = Prelude.Nothing,
      metricName = Prelude.Nothing,
      rateKey = Prelude.Nothing
    }

-- | The name of the rate-based rule.
awsWafRateBasedRuleDetails_name :: Lens.Lens' AwsWafRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRateBasedRuleDetails_name = Lens.lens (\AwsWafRateBasedRuleDetails' {name} -> name) (\s@AwsWafRateBasedRuleDetails' {} a -> s {name = a} :: AwsWafRateBasedRuleDetails)

-- | The unique identifier for the rate-based rule.
awsWafRateBasedRuleDetails_ruleId :: Lens.Lens' AwsWafRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRateBasedRuleDetails_ruleId = Lens.lens (\AwsWafRateBasedRuleDetails' {ruleId} -> ruleId) (\s@AwsWafRateBasedRuleDetails' {} a -> s {ruleId = a} :: AwsWafRateBasedRuleDetails)

-- | The predicates to include in the rate-based rule.
awsWafRateBasedRuleDetails_matchPredicates :: Lens.Lens' AwsWafRateBasedRuleDetails (Prelude.Maybe [AwsWafRateBasedRuleMatchPredicate])
awsWafRateBasedRuleDetails_matchPredicates = Lens.lens (\AwsWafRateBasedRuleDetails' {matchPredicates} -> matchPredicates) (\s@AwsWafRateBasedRuleDetails' {} a -> s {matchPredicates = a} :: AwsWafRateBasedRuleDetails) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of requests that have an identical value for the
-- field specified in @RateKey@ that are allowed within a five-minute
-- period. If the number of requests exceeds @RateLimit@ and the other
-- predicates specified in the rule are met, WAF triggers the action for
-- the rule.
awsWafRateBasedRuleDetails_rateLimit :: Lens.Lens' AwsWafRateBasedRuleDetails (Prelude.Maybe Prelude.Integer)
awsWafRateBasedRuleDetails_rateLimit = Lens.lens (\AwsWafRateBasedRuleDetails' {rateLimit} -> rateLimit) (\s@AwsWafRateBasedRuleDetails' {} a -> s {rateLimit = a} :: AwsWafRateBasedRuleDetails)

-- | The name of the metrics for the rate-based rule.
awsWafRateBasedRuleDetails_metricName :: Lens.Lens' AwsWafRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRateBasedRuleDetails_metricName = Lens.lens (\AwsWafRateBasedRuleDetails' {metricName} -> metricName) (\s@AwsWafRateBasedRuleDetails' {} a -> s {metricName = a} :: AwsWafRateBasedRuleDetails)

-- | The field that WAF uses to determine whether requests are likely
-- arriving from single source and are subject to rate monitoring.
awsWafRateBasedRuleDetails_rateKey :: Lens.Lens' AwsWafRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRateBasedRuleDetails_rateKey = Lens.lens (\AwsWafRateBasedRuleDetails' {rateKey} -> rateKey) (\s@AwsWafRateBasedRuleDetails' {} a -> s {rateKey = a} :: AwsWafRateBasedRuleDetails)

instance Core.FromJSON AwsWafRateBasedRuleDetails where
  parseJSON =
    Core.withObject
      "AwsWafRateBasedRuleDetails"
      ( \x ->
          AwsWafRateBasedRuleDetails'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RuleId")
            Prelude.<*> ( x Core..:? "MatchPredicates"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "RateLimit")
            Prelude.<*> (x Core..:? "MetricName")
            Prelude.<*> (x Core..:? "RateKey")
      )

instance Prelude.Hashable AwsWafRateBasedRuleDetails where
  hashWithSalt _salt AwsWafRateBasedRuleDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` matchPredicates
      `Prelude.hashWithSalt` rateLimit
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` rateKey

instance Prelude.NFData AwsWafRateBasedRuleDetails where
  rnf AwsWafRateBasedRuleDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf matchPredicates
      `Prelude.seq` Prelude.rnf rateLimit
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf rateKey

instance Core.ToJSON AwsWafRateBasedRuleDetails where
  toJSON AwsWafRateBasedRuleDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("RuleId" Core..=) Prelude.<$> ruleId,
            ("MatchPredicates" Core..=)
              Prelude.<$> matchPredicates,
            ("RateLimit" Core..=) Prelude.<$> rateLimit,
            ("MetricName" Core..=) Prelude.<$> metricName,
            ("RateKey" Core..=) Prelude.<$> rateKey
          ]
      )
