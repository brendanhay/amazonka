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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleMatchPredicate

-- | contains details about a rate-based rule for Regional resources. A
-- rate-based rule provides settings to indicate when to allow, block, or
-- count a request. Rate-based rules include the number of requests that
-- arrive over a specified period of time.
--
-- /See:/ 'newAwsWafRegionalRateBasedRuleDetails' smart constructor.
data AwsWafRegionalRateBasedRuleDetails = AwsWafRegionalRateBasedRuleDetails'
  { -- | The maximum number of requests that have an identical value for the
    -- field specified in @RateKey@ that are allowed within a five-minute
    -- period. If the number of requests exceeds @RateLimit@ and the other
    -- predicates specified in the rule are met, WAF triggers the action for
    -- the rule.
    rateLimit :: Prelude.Maybe Prelude.Integer,
    -- | The field that WAF uses to determine whether requests are likely
    -- arriving from single source and are subject to rate monitoring.
    rateKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the metrics for the rate-based rule.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the rate-based rule.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the rate-based rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The predicates to include in the rate-based rule.
    matchPredicates :: Prelude.Maybe [AwsWafRegionalRateBasedRuleMatchPredicate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalRateBasedRuleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rateLimit', 'awsWafRegionalRateBasedRuleDetails_rateLimit' - The maximum number of requests that have an identical value for the
-- field specified in @RateKey@ that are allowed within a five-minute
-- period. If the number of requests exceeds @RateLimit@ and the other
-- predicates specified in the rule are met, WAF triggers the action for
-- the rule.
--
-- 'rateKey', 'awsWafRegionalRateBasedRuleDetails_rateKey' - The field that WAF uses to determine whether requests are likely
-- arriving from single source and are subject to rate monitoring.
--
-- 'metricName', 'awsWafRegionalRateBasedRuleDetails_metricName' - The name of the metrics for the rate-based rule.
--
-- 'ruleId', 'awsWafRegionalRateBasedRuleDetails_ruleId' - The unique identifier for the rate-based rule.
--
-- 'name', 'awsWafRegionalRateBasedRuleDetails_name' - The name of the rate-based rule.
--
-- 'matchPredicates', 'awsWafRegionalRateBasedRuleDetails_matchPredicates' - The predicates to include in the rate-based rule.
newAwsWafRegionalRateBasedRuleDetails ::
  AwsWafRegionalRateBasedRuleDetails
newAwsWafRegionalRateBasedRuleDetails =
  AwsWafRegionalRateBasedRuleDetails'
    { rateLimit =
        Prelude.Nothing,
      rateKey = Prelude.Nothing,
      metricName = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      name = Prelude.Nothing,
      matchPredicates = Prelude.Nothing
    }

-- | The maximum number of requests that have an identical value for the
-- field specified in @RateKey@ that are allowed within a five-minute
-- period. If the number of requests exceeds @RateLimit@ and the other
-- predicates specified in the rule are met, WAF triggers the action for
-- the rule.
awsWafRegionalRateBasedRuleDetails_rateLimit :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Integer)
awsWafRegionalRateBasedRuleDetails_rateLimit = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {rateLimit} -> rateLimit) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {rateLimit = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The field that WAF uses to determine whether requests are likely
-- arriving from single source and are subject to rate monitoring.
awsWafRegionalRateBasedRuleDetails_rateKey :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleDetails_rateKey = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {rateKey} -> rateKey) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {rateKey = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The name of the metrics for the rate-based rule.
awsWafRegionalRateBasedRuleDetails_metricName :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleDetails_metricName = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {metricName} -> metricName) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {metricName = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The unique identifier for the rate-based rule.
awsWafRegionalRateBasedRuleDetails_ruleId :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleDetails_ruleId = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {ruleId} -> ruleId) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {ruleId = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The name of the rate-based rule.
awsWafRegionalRateBasedRuleDetails_name :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleDetails_name = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {name} -> name) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {name = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The predicates to include in the rate-based rule.
awsWafRegionalRateBasedRuleDetails_matchPredicates :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe [AwsWafRegionalRateBasedRuleMatchPredicate])
awsWafRegionalRateBasedRuleDetails_matchPredicates = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {matchPredicates} -> matchPredicates) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {matchPredicates = a} :: AwsWafRegionalRateBasedRuleDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsWafRegionalRateBasedRuleDetails
  where
  parseJSON =
    Core.withObject
      "AwsWafRegionalRateBasedRuleDetails"
      ( \x ->
          AwsWafRegionalRateBasedRuleDetails'
            Prelude.<$> (x Core..:? "RateLimit")
            Prelude.<*> (x Core..:? "RateKey")
            Prelude.<*> (x Core..:? "MetricName")
            Prelude.<*> (x Core..:? "RuleId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> ( x Core..:? "MatchPredicates"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsWafRegionalRateBasedRuleDetails
  where
  hashWithSalt
    salt'
    AwsWafRegionalRateBasedRuleDetails' {..} =
      salt' `Prelude.hashWithSalt` matchPredicates
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` ruleId
        `Prelude.hashWithSalt` metricName
        `Prelude.hashWithSalt` rateKey
        `Prelude.hashWithSalt` rateLimit

instance
  Prelude.NFData
    AwsWafRegionalRateBasedRuleDetails
  where
  rnf AwsWafRegionalRateBasedRuleDetails' {..} =
    Prelude.rnf rateLimit
      `Prelude.seq` Prelude.rnf matchPredicates
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf rateKey

instance
  Core.ToJSON
    AwsWafRegionalRateBasedRuleDetails
  where
  toJSON AwsWafRegionalRateBasedRuleDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RateLimit" Core..=) Prelude.<$> rateLimit,
            ("RateKey" Core..=) Prelude.<$> rateKey,
            ("MetricName" Core..=) Prelude.<$> metricName,
            ("RuleId" Core..=) Prelude.<$> ruleId,
            ("Name" Core..=) Prelude.<$> name,
            ("MatchPredicates" Core..=)
              Prelude.<$> matchPredicates
          ]
      )
