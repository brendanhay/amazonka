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
-- Module      : Amazonka.WAFV2.Types.RateBasedStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateBasedStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ForwardedIPConfig
import Amazonka.WAFV2.Types.RateBasedStatementAggregateKeyType
import {-# SOURCE #-} Amazonka.WAFV2.Types.Statement

-- | A rate-based rule tracks the rate of requests for each originating IP
-- address, and triggers the rule action when the rate exceeds a limit that
-- you specify on the number of requests in any 5-minute time span. You can
-- use this to put a temporary block on requests from an IP address that is
-- sending excessive requests.
--
-- WAF tracks and manages web requests separately for each instance of a
-- rate-based rule that you use. For example, if you provide the same
-- rate-based rule settings in two web ACLs, each of the two rule
-- statements represents a separate instance of the rate-based rule and
-- gets its own tracking and management by WAF. If you define a rate-based
-- rule inside a rule group, and then use that rule group in multiple
-- places, each use creates a separate instance of the rate-based rule that
-- gets its own tracking and management by WAF.
--
-- When the rule action triggers, WAF blocks additional requests from the
-- IP address until the request rate falls below the limit.
--
-- You can optionally nest another statement inside the rate-based
-- statement, to narrow the scope of the rule so that it only counts
-- requests that match the nested statement. For example, based on recent
-- requests that you have seen from an attacker, you might create a
-- rate-based rule with a nested AND rule statement that contains the
-- following nested statements:
--
-- -   An IP match statement with an IP set that specified the address
--     192.0.2.44.
--
-- -   A string match statement that searches in the User-Agent header for
--     the string BadBot.
--
-- In this rate-based rule, you also define a rate limit. For this example,
-- the rate limit is 1,000. Requests that meet the criteria of both of the
-- nested statements are counted. If the count exceeds 1,000 requests per
-- five minutes, the rule action triggers. Requests that do not meet the
-- criteria of both of the nested statements are not counted towards the
-- rate limit and are not affected by this rule.
--
-- You cannot nest a @RateBasedStatement@ inside another statement, for
-- example inside a @NotStatement@ or @OrStatement@. You can define a
-- @RateBasedStatement@ inside a web ACL and inside a rule group.
--
-- /See:/ 'newRateBasedStatement' smart constructor.
data RateBasedStatement = RateBasedStatement'
  { -- | The configuration for inspecting IP addresses in an HTTP header that you
    -- specify, instead of using the IP address that\'s reported by the web
    -- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
    -- you can specify any header name.
    --
    -- If the specified header isn\'t present in the request, WAF doesn\'t
    -- apply the rule to the web request at all.
    --
    -- This is required if @AggregateKeyType@ is set to @FORWARDED_IP@.
    forwardedIPConfig :: Prelude.Maybe ForwardedIPConfig,
    -- | An optional nested statement that narrows the scope of the web requests
    -- that are evaluated by the rate-based statement. Requests are only
    -- tracked by the rate-based statement if they match the scope-down
    -- statement. You can use any nestable Statement in the scope-down
    -- statement, and you can nest statements at any level, the same as you can
    -- for a rule statement.
    scopeDownStatement :: Prelude.Maybe Statement,
    -- | The limit on requests per 5-minute period for a single originating IP
    -- address. If the statement includes a @ScopeDownStatement@, this limit is
    -- applied only to the requests that match the statement.
    limit :: Prelude.Natural,
    -- | Setting that indicates how to aggregate the request counts. The options
    -- are the following:
    --
    -- -   IP - Aggregate the request counts on the IP address from the web
    --     request origin.
    --
    -- -   FORWARDED_IP - Aggregate the request counts on the first IP address
    --     in an HTTP header. If you use this, configure the
    --     @ForwardedIPConfig@, to specify the header to use.
    aggregateKeyType :: RateBasedStatementAggregateKeyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateBasedStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forwardedIPConfig', 'rateBasedStatement_forwardedIPConfig' - The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- This is required if @AggregateKeyType@ is set to @FORWARDED_IP@.
--
-- 'scopeDownStatement', 'rateBasedStatement_scopeDownStatement' - An optional nested statement that narrows the scope of the web requests
-- that are evaluated by the rate-based statement. Requests are only
-- tracked by the rate-based statement if they match the scope-down
-- statement. You can use any nestable Statement in the scope-down
-- statement, and you can nest statements at any level, the same as you can
-- for a rule statement.
--
-- 'limit', 'rateBasedStatement_limit' - The limit on requests per 5-minute period for a single originating IP
-- address. If the statement includes a @ScopeDownStatement@, this limit is
-- applied only to the requests that match the statement.
--
-- 'aggregateKeyType', 'rateBasedStatement_aggregateKeyType' - Setting that indicates how to aggregate the request counts. The options
-- are the following:
--
-- -   IP - Aggregate the request counts on the IP address from the web
--     request origin.
--
-- -   FORWARDED_IP - Aggregate the request counts on the first IP address
--     in an HTTP header. If you use this, configure the
--     @ForwardedIPConfig@, to specify the header to use.
newRateBasedStatement ::
  -- | 'limit'
  Prelude.Natural ->
  -- | 'aggregateKeyType'
  RateBasedStatementAggregateKeyType ->
  RateBasedStatement
newRateBasedStatement pLimit_ pAggregateKeyType_ =
  RateBasedStatement'
    { forwardedIPConfig =
        Prelude.Nothing,
      scopeDownStatement = Prelude.Nothing,
      limit = pLimit_,
      aggregateKeyType = pAggregateKeyType_
    }

-- | The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- This is required if @AggregateKeyType@ is set to @FORWARDED_IP@.
rateBasedStatement_forwardedIPConfig :: Lens.Lens' RateBasedStatement (Prelude.Maybe ForwardedIPConfig)
rateBasedStatement_forwardedIPConfig = Lens.lens (\RateBasedStatement' {forwardedIPConfig} -> forwardedIPConfig) (\s@RateBasedStatement' {} a -> s {forwardedIPConfig = a} :: RateBasedStatement)

-- | An optional nested statement that narrows the scope of the web requests
-- that are evaluated by the rate-based statement. Requests are only
-- tracked by the rate-based statement if they match the scope-down
-- statement. You can use any nestable Statement in the scope-down
-- statement, and you can nest statements at any level, the same as you can
-- for a rule statement.
rateBasedStatement_scopeDownStatement :: Lens.Lens' RateBasedStatement (Prelude.Maybe Statement)
rateBasedStatement_scopeDownStatement = Lens.lens (\RateBasedStatement' {scopeDownStatement} -> scopeDownStatement) (\s@RateBasedStatement' {} a -> s {scopeDownStatement = a} :: RateBasedStatement)

-- | The limit on requests per 5-minute period for a single originating IP
-- address. If the statement includes a @ScopeDownStatement@, this limit is
-- applied only to the requests that match the statement.
rateBasedStatement_limit :: Lens.Lens' RateBasedStatement Prelude.Natural
rateBasedStatement_limit = Lens.lens (\RateBasedStatement' {limit} -> limit) (\s@RateBasedStatement' {} a -> s {limit = a} :: RateBasedStatement)

-- | Setting that indicates how to aggregate the request counts. The options
-- are the following:
--
-- -   IP - Aggregate the request counts on the IP address from the web
--     request origin.
--
-- -   FORWARDED_IP - Aggregate the request counts on the first IP address
--     in an HTTP header. If you use this, configure the
--     @ForwardedIPConfig@, to specify the header to use.
rateBasedStatement_aggregateKeyType :: Lens.Lens' RateBasedStatement RateBasedStatementAggregateKeyType
rateBasedStatement_aggregateKeyType = Lens.lens (\RateBasedStatement' {aggregateKeyType} -> aggregateKeyType) (\s@RateBasedStatement' {} a -> s {aggregateKeyType = a} :: RateBasedStatement)

instance Data.FromJSON RateBasedStatement where
  parseJSON =
    Data.withObject
      "RateBasedStatement"
      ( \x ->
          RateBasedStatement'
            Prelude.<$> (x Data..:? "ForwardedIPConfig")
            Prelude.<*> (x Data..:? "ScopeDownStatement")
            Prelude.<*> (x Data..: "Limit")
            Prelude.<*> (x Data..: "AggregateKeyType")
      )

instance Prelude.Hashable RateBasedStatement where
  hashWithSalt _salt RateBasedStatement' {..} =
    _salt `Prelude.hashWithSalt` forwardedIPConfig
      `Prelude.hashWithSalt` scopeDownStatement
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` aggregateKeyType

instance Prelude.NFData RateBasedStatement where
  rnf RateBasedStatement' {..} =
    Prelude.rnf forwardedIPConfig
      `Prelude.seq` Prelude.rnf scopeDownStatement
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf aggregateKeyType

instance Data.ToJSON RateBasedStatement where
  toJSON RateBasedStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForwardedIPConfig" Data..=)
              Prelude.<$> forwardedIPConfig,
            ("ScopeDownStatement" Data..=)
              Prelude.<$> scopeDownStatement,
            Prelude.Just ("Limit" Data..= limit),
            Prelude.Just
              ("AggregateKeyType" Data..= aggregateKeyType)
          ]
      )
