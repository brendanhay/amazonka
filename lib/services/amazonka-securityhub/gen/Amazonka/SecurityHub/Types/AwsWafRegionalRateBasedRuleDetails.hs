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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleMatchPredicate

-- | contains details about a rate-based rule for Regional resources. A
-- rate-based rule provides settings to indicate when to allow, block, or
-- count a request. Rate-based rules include the number of requests that
-- arrive over a specified period of time.
--
-- /See:/ 'newAwsWafRegionalRateBasedRuleDetails' smart constructor.
data AwsWafRegionalRateBasedRuleDetails = AwsWafRegionalRateBasedRuleDetails'
  { -- | The predicates to include in the rate-based rule.
    matchPredicates :: Prelude.Maybe [AwsWafRegionalRateBasedRuleMatchPredicate],
    -- | The name of the metrics for the rate-based rule.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The name of the rate-based rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The field that WAF uses to determine whether requests are likely
    -- arriving from single source and are subject to rate monitoring.
    rateKey :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of requests that have an identical value for the
    -- field specified in @RateKey@ that are allowed within a five-minute
    -- period. If the number of requests exceeds @RateLimit@ and the other
    -- predicates specified in the rule are met, WAF triggers the action for
    -- the rule.
    rateLimit :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier for the rate-based rule.
    ruleId :: Prelude.Maybe Prelude.Text
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
-- 'matchPredicates', 'awsWafRegionalRateBasedRuleDetails_matchPredicates' - The predicates to include in the rate-based rule.
--
-- 'metricName', 'awsWafRegionalRateBasedRuleDetails_metricName' - The name of the metrics for the rate-based rule.
--
-- 'name', 'awsWafRegionalRateBasedRuleDetails_name' - The name of the rate-based rule.
--
-- 'rateKey', 'awsWafRegionalRateBasedRuleDetails_rateKey' - The field that WAF uses to determine whether requests are likely
-- arriving from single source and are subject to rate monitoring.
--
-- 'rateLimit', 'awsWafRegionalRateBasedRuleDetails_rateLimit' - The maximum number of requests that have an identical value for the
-- field specified in @RateKey@ that are allowed within a five-minute
-- period. If the number of requests exceeds @RateLimit@ and the other
-- predicates specified in the rule are met, WAF triggers the action for
-- the rule.
--
-- 'ruleId', 'awsWafRegionalRateBasedRuleDetails_ruleId' - The unique identifier for the rate-based rule.
newAwsWafRegionalRateBasedRuleDetails ::
  AwsWafRegionalRateBasedRuleDetails
newAwsWafRegionalRateBasedRuleDetails =
  AwsWafRegionalRateBasedRuleDetails'
    { matchPredicates =
        Prelude.Nothing,
      metricName = Prelude.Nothing,
      name = Prelude.Nothing,
      rateKey = Prelude.Nothing,
      rateLimit = Prelude.Nothing,
      ruleId = Prelude.Nothing
    }

-- | The predicates to include in the rate-based rule.
awsWafRegionalRateBasedRuleDetails_matchPredicates :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe [AwsWafRegionalRateBasedRuleMatchPredicate])
awsWafRegionalRateBasedRuleDetails_matchPredicates = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {matchPredicates} -> matchPredicates) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {matchPredicates = a} :: AwsWafRegionalRateBasedRuleDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metrics for the rate-based rule.
awsWafRegionalRateBasedRuleDetails_metricName :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleDetails_metricName = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {metricName} -> metricName) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {metricName = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The name of the rate-based rule.
awsWafRegionalRateBasedRuleDetails_name :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleDetails_name = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {name} -> name) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {name = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The field that WAF uses to determine whether requests are likely
-- arriving from single source and are subject to rate monitoring.
awsWafRegionalRateBasedRuleDetails_rateKey :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleDetails_rateKey = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {rateKey} -> rateKey) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {rateKey = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The maximum number of requests that have an identical value for the
-- field specified in @RateKey@ that are allowed within a five-minute
-- period. If the number of requests exceeds @RateLimit@ and the other
-- predicates specified in the rule are met, WAF triggers the action for
-- the rule.
awsWafRegionalRateBasedRuleDetails_rateLimit :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Integer)
awsWafRegionalRateBasedRuleDetails_rateLimit = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {rateLimit} -> rateLimit) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {rateLimit = a} :: AwsWafRegionalRateBasedRuleDetails)

-- | The unique identifier for the rate-based rule.
awsWafRegionalRateBasedRuleDetails_ruleId :: Lens.Lens' AwsWafRegionalRateBasedRuleDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleDetails_ruleId = Lens.lens (\AwsWafRegionalRateBasedRuleDetails' {ruleId} -> ruleId) (\s@AwsWafRegionalRateBasedRuleDetails' {} a -> s {ruleId = a} :: AwsWafRegionalRateBasedRuleDetails)

instance
  Data.FromJSON
    AwsWafRegionalRateBasedRuleDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafRegionalRateBasedRuleDetails"
      ( \x ->
          AwsWafRegionalRateBasedRuleDetails'
            Prelude.<$> ( x
                            Data..:? "MatchPredicates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RateKey")
            Prelude.<*> (x Data..:? "RateLimit")
            Prelude.<*> (x Data..:? "RuleId")
      )

instance
  Prelude.Hashable
    AwsWafRegionalRateBasedRuleDetails
  where
  hashWithSalt
    _salt
    AwsWafRegionalRateBasedRuleDetails' {..} =
      _salt
        `Prelude.hashWithSalt` matchPredicates
        `Prelude.hashWithSalt` metricName
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` rateKey
        `Prelude.hashWithSalt` rateLimit
        `Prelude.hashWithSalt` ruleId

instance
  Prelude.NFData
    AwsWafRegionalRateBasedRuleDetails
  where
  rnf AwsWafRegionalRateBasedRuleDetails' {..} =
    Prelude.rnf matchPredicates `Prelude.seq`
      Prelude.rnf metricName `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf rateKey `Prelude.seq`
            Prelude.rnf rateLimit `Prelude.seq`
              Prelude.rnf ruleId

instance
  Data.ToJSON
    AwsWafRegionalRateBasedRuleDetails
  where
  toJSON AwsWafRegionalRateBasedRuleDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MatchPredicates" Data..=)
              Prelude.<$> matchPredicates,
            ("MetricName" Data..=) Prelude.<$> metricName,
            ("Name" Data..=) Prelude.<$> name,
            ("RateKey" Data..=) Prelude.<$> rateKey,
            ("RateLimit" Data..=) Prelude.<$> rateLimit,
            ("RuleId" Data..=) Prelude.<$> ruleId
          ]
      )
