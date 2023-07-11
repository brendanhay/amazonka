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
-- Module      : Amazonka.WAF.Types.RateBasedRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.RateBasedRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAF.Types.Predicate
import Amazonka.WAF.Types.RateKey

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- A @RateBasedRule@ is identical to a regular Rule, with one addition: a
-- @RateBasedRule@ counts the number of requests that arrive from a
-- specified IP address every five minutes. For example, based on recent
-- requests that you\'ve seen from an attacker, you might create a
-- @RateBasedRule@ that includes the following conditions:
--
-- -   The requests come from 192.0.2.44.
--
-- -   They contain the value @BadBot@ in the @User-Agent@ header.
--
-- In the rule, you also define the rate limit as 1,000.
--
-- Requests that meet both of these conditions and exceed 1,000 requests
-- every five minutes trigger the rule\'s action (block or count), which is
-- defined in the web ACL.
--
-- /See:/ 'newRateBasedRule' smart constructor.
data RateBasedRule = RateBasedRule'
  { -- | A friendly name or description for the metrics for a @RateBasedRule@.
    -- The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
    -- maximum length 128 and minimum length one. It can\'t contain whitespace
    -- or metric names reserved for AWS WAF, including \"All\" and
    -- \"Default_Action.\" You can\'t change the name of the metric after you
    -- create the @RateBasedRule@.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | A friendly name or description for a @RateBasedRule@. You can\'t change
    -- the name of a @RateBasedRule@ after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a @RateBasedRule@. You use @RuleId@ to get more
    -- information about a @RateBasedRule@ (see GetRateBasedRule), update a
    -- @RateBasedRule@ (see UpdateRateBasedRule), insert a @RateBasedRule@ into
    -- a @WebACL@ or delete one from a @WebACL@ (see UpdateWebACL), or delete a
    -- @RateBasedRule@ from AWS WAF (see DeleteRateBasedRule).
    ruleId :: Prelude.Text,
    -- | The @Predicates@ object contains one @Predicate@ element for each
    -- ByteMatchSet, IPSet, or SqlInjectionMatchSet object that you want to
    -- include in a @RateBasedRule@.
    matchPredicates :: [Predicate],
    -- | The field that AWS WAF uses to determine if requests are likely arriving
    -- from single source and thus subject to rate monitoring. The only valid
    -- value for @RateKey@ is @IP@. @IP@ indicates that requests arriving from
    -- the same IP address are subject to the @RateLimit@ that is specified in
    -- the @RateBasedRule@.
    rateKey :: RateKey,
    -- | The maximum number of requests, which have an identical value in the
    -- field specified by the @RateKey@, allowed in a five-minute period. If
    -- the number of requests exceeds the @RateLimit@ and the other predicates
    -- specified in the rule are also met, AWS WAF triggers the action that is
    -- specified for this rule.
    rateLimit :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateBasedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'rateBasedRule_metricName' - A friendly name or description for the metrics for a @RateBasedRule@.
-- The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @RateBasedRule@.
--
-- 'name', 'rateBasedRule_name' - A friendly name or description for a @RateBasedRule@. You can\'t change
-- the name of a @RateBasedRule@ after you create it.
--
-- 'ruleId', 'rateBasedRule_ruleId' - A unique identifier for a @RateBasedRule@. You use @RuleId@ to get more
-- information about a @RateBasedRule@ (see GetRateBasedRule), update a
-- @RateBasedRule@ (see UpdateRateBasedRule), insert a @RateBasedRule@ into
-- a @WebACL@ or delete one from a @WebACL@ (see UpdateWebACL), or delete a
-- @RateBasedRule@ from AWS WAF (see DeleteRateBasedRule).
--
-- 'matchPredicates', 'rateBasedRule_matchPredicates' - The @Predicates@ object contains one @Predicate@ element for each
-- ByteMatchSet, IPSet, or SqlInjectionMatchSet object that you want to
-- include in a @RateBasedRule@.
--
-- 'rateKey', 'rateBasedRule_rateKey' - The field that AWS WAF uses to determine if requests are likely arriving
-- from single source and thus subject to rate monitoring. The only valid
-- value for @RateKey@ is @IP@. @IP@ indicates that requests arriving from
-- the same IP address are subject to the @RateLimit@ that is specified in
-- the @RateBasedRule@.
--
-- 'rateLimit', 'rateBasedRule_rateLimit' - The maximum number of requests, which have an identical value in the
-- field specified by the @RateKey@, allowed in a five-minute period. If
-- the number of requests exceeds the @RateLimit@ and the other predicates
-- specified in the rule are also met, AWS WAF triggers the action that is
-- specified for this rule.
newRateBasedRule ::
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'rateKey'
  RateKey ->
  -- | 'rateLimit'
  Prelude.Natural ->
  RateBasedRule
newRateBasedRule pRuleId_ pRateKey_ pRateLimit_ =
  RateBasedRule'
    { metricName = Prelude.Nothing,
      name = Prelude.Nothing,
      ruleId = pRuleId_,
      matchPredicates = Prelude.mempty,
      rateKey = pRateKey_,
      rateLimit = pRateLimit_
    }

-- | A friendly name or description for the metrics for a @RateBasedRule@.
-- The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @RateBasedRule@.
rateBasedRule_metricName :: Lens.Lens' RateBasedRule (Prelude.Maybe Prelude.Text)
rateBasedRule_metricName = Lens.lens (\RateBasedRule' {metricName} -> metricName) (\s@RateBasedRule' {} a -> s {metricName = a} :: RateBasedRule)

-- | A friendly name or description for a @RateBasedRule@. You can\'t change
-- the name of a @RateBasedRule@ after you create it.
rateBasedRule_name :: Lens.Lens' RateBasedRule (Prelude.Maybe Prelude.Text)
rateBasedRule_name = Lens.lens (\RateBasedRule' {name} -> name) (\s@RateBasedRule' {} a -> s {name = a} :: RateBasedRule)

-- | A unique identifier for a @RateBasedRule@. You use @RuleId@ to get more
-- information about a @RateBasedRule@ (see GetRateBasedRule), update a
-- @RateBasedRule@ (see UpdateRateBasedRule), insert a @RateBasedRule@ into
-- a @WebACL@ or delete one from a @WebACL@ (see UpdateWebACL), or delete a
-- @RateBasedRule@ from AWS WAF (see DeleteRateBasedRule).
rateBasedRule_ruleId :: Lens.Lens' RateBasedRule Prelude.Text
rateBasedRule_ruleId = Lens.lens (\RateBasedRule' {ruleId} -> ruleId) (\s@RateBasedRule' {} a -> s {ruleId = a} :: RateBasedRule)

-- | The @Predicates@ object contains one @Predicate@ element for each
-- ByteMatchSet, IPSet, or SqlInjectionMatchSet object that you want to
-- include in a @RateBasedRule@.
rateBasedRule_matchPredicates :: Lens.Lens' RateBasedRule [Predicate]
rateBasedRule_matchPredicates = Lens.lens (\RateBasedRule' {matchPredicates} -> matchPredicates) (\s@RateBasedRule' {} a -> s {matchPredicates = a} :: RateBasedRule) Prelude.. Lens.coerced

-- | The field that AWS WAF uses to determine if requests are likely arriving
-- from single source and thus subject to rate monitoring. The only valid
-- value for @RateKey@ is @IP@. @IP@ indicates that requests arriving from
-- the same IP address are subject to the @RateLimit@ that is specified in
-- the @RateBasedRule@.
rateBasedRule_rateKey :: Lens.Lens' RateBasedRule RateKey
rateBasedRule_rateKey = Lens.lens (\RateBasedRule' {rateKey} -> rateKey) (\s@RateBasedRule' {} a -> s {rateKey = a} :: RateBasedRule)

-- | The maximum number of requests, which have an identical value in the
-- field specified by the @RateKey@, allowed in a five-minute period. If
-- the number of requests exceeds the @RateLimit@ and the other predicates
-- specified in the rule are also met, AWS WAF triggers the action that is
-- specified for this rule.
rateBasedRule_rateLimit :: Lens.Lens' RateBasedRule Prelude.Natural
rateBasedRule_rateLimit = Lens.lens (\RateBasedRule' {rateLimit} -> rateLimit) (\s@RateBasedRule' {} a -> s {rateLimit = a} :: RateBasedRule)

instance Data.FromJSON RateBasedRule where
  parseJSON =
    Data.withObject
      "RateBasedRule"
      ( \x ->
          RateBasedRule'
            Prelude.<$> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..: "RuleId")
            Prelude.<*> ( x
                            Data..:? "MatchPredicates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "RateKey")
            Prelude.<*> (x Data..: "RateLimit")
      )

instance Prelude.Hashable RateBasedRule where
  hashWithSalt _salt RateBasedRule' {..} =
    _salt
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` matchPredicates
      `Prelude.hashWithSalt` rateKey
      `Prelude.hashWithSalt` rateLimit

instance Prelude.NFData RateBasedRule where
  rnf RateBasedRule' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf matchPredicates
      `Prelude.seq` Prelude.rnf rateKey
      `Prelude.seq` Prelude.rnf rateLimit
