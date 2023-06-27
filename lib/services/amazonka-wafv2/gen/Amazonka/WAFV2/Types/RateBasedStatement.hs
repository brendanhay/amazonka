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
import Amazonka.WAFV2.Types.RateBasedStatementCustomKey
import {-# SOURCE #-} Amazonka.WAFV2.Types.Statement

-- | A rate-based rule counts incoming requests and rate limits requests when
-- they are coming at too fast a rate. The rule categorizes requests
-- according to your aggregation criteria, collects them into aggregation
-- instances, and counts and rate limits the requests for each instance.
--
-- You can specify individual aggregation keys, like IP address or HTTP
-- method. You can also specify aggregation key combinations, like IP
-- address and HTTP method, or HTTP method, query argument, and cookie.
--
-- Each unique set of values for the aggregation keys that you specify is a
-- separate aggregation instance, with the value from each key contributing
-- to the aggregation instance definition.
--
-- For example, assume the rule evaluates web requests with the following
-- IP address and HTTP method values:
--
-- -   IP address 10.1.1.1, HTTP method POST
--
-- -   IP address 10.1.1.1, HTTP method GET
--
-- -   IP address 127.0.0.0, HTTP method POST
--
-- -   IP address 10.1.1.1, HTTP method GET
--
-- The rule would create different aggregation instances according to your
-- aggregation criteria, for example:
--
-- -   If the aggregation criteria is just the IP address, then each
--     individual address is an aggregation instance, and WAF counts
--     requests separately for each. The aggregation instances and request
--     counts for our example would be the following:
--
--     -   IP address 10.1.1.1: count 3
--
--     -   IP address 127.0.0.0: count 1
--
-- -   If the aggregation criteria is HTTP method, then each individual
--     HTTP method is an aggregation instance. The aggregation instances
--     and request counts for our example would be the following:
--
--     -   HTTP method POST: count 2
--
--     -   HTTP method GET: count 2
--
-- -   If the aggregation criteria is IP address and HTTP method, then each
--     IP address and each HTTP method would contribute to the combined
--     aggregation instance. The aggregation instances and request counts
--     for our example would be the following:
--
--     -   IP address 10.1.1.1, HTTP method POST: count 1
--
--     -   IP address 10.1.1.1, HTTP method GET: count 2
--
--     -   IP address 127.0.0.0, HTTP method POST: count 1
--
-- For any n-tuple of aggregation keys, each unique combination of values
-- for the keys defines a separate aggregation instance, which WAF counts
-- and rate-limits individually.
--
-- You can optionally nest another statement inside the rate-based
-- statement, to narrow the scope of the rule so that it only counts and
-- rate limits requests that match the nested statement. You can use this
-- nested scope-down statement in conjunction with your aggregation key
-- specifications or you can just count and rate limit all requests that
-- match the scope-down statement, without additional aggregation. When you
-- choose to just manage all requests that match a scope-down statement,
-- the aggregation instance is singular for the rule.
--
-- You cannot nest a @RateBasedStatement@ inside another statement, for
-- example inside a @NotStatement@ or @OrStatement@. You can define a
-- @RateBasedStatement@ inside a web ACL and inside a rule group.
--
-- For additional information about the options, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rate-based-rules.html Rate limiting web requests using rate-based rules>
-- in the /WAF Developer Guide/.
--
-- If you only aggregate on the individual IP address or forwarded IP
-- address, you can retrieve the list of IP addresses that WAF is currently
-- rate limiting for a rule through the API call
-- @GetRateBasedStatementManagedKeys@. This option is not available for
-- other aggregation configurations.
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
-- /See:/ 'newRateBasedStatement' smart constructor.
data RateBasedStatement = RateBasedStatement'
  { -- | Specifies the aggregate keys to use in a rate-base rule.
    customKeys :: Prelude.Maybe (Prelude.NonEmpty RateBasedStatementCustomKey),
    -- | The configuration for inspecting IP addresses in an HTTP header that you
    -- specify, instead of using the IP address that\'s reported by the web
    -- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
    -- you can specify any header name.
    --
    -- If the specified header isn\'t present in the request, WAF doesn\'t
    -- apply the rule to the web request at all.
    --
    -- This is required if you specify a forwarded IP in the rule\'s aggregate
    -- key settings.
    forwardedIPConfig :: Prelude.Maybe ForwardedIPConfig,
    -- | An optional nested statement that narrows the scope of the web requests
    -- that are evaluated and managed by the rate-based statement. When you use
    -- a scope-down statement, the rate-based rule only tracks and rate limits
    -- requests that match the scope-down statement. You can use any nestable
    -- Statement in the scope-down statement, and you can nest statements at
    -- any level, the same as you can for a rule statement.
    scopeDownStatement :: Prelude.Maybe Statement,
    -- | The limit on requests per 5-minute period for a single aggregation
    -- instance for the rate-based rule. If the rate-based statement includes a
    -- @ScopeDownStatement@, this limit is applied only to the requests that
    -- match the statement.
    --
    -- Examples:
    --
    -- -   If you aggregate on just the IP address, this is the limit on
    --     requests from any single IP address.
    --
    -- -   If you aggregate on the HTTP method and the query argument name
    --     \"city\", then this is the limit on requests for any single method,
    --     city pair.
    limit :: Prelude.Natural,
    -- | Setting that indicates how to aggregate the request counts.
    --
    -- Web requests that are missing any of the components specified in the
    -- aggregation keys are omitted from the rate-based rule evaluation and
    -- handling.
    --
    -- -   @CONSTANT@ - Count and limit the requests that match the rate-based
    --     rule\'s scope-down statement. With this option, the counted requests
    --     aren\'t further aggregated. The scope-down statement is the only
    --     specification used. When the count of all requests that satisfy the
    --     scope-down statement goes over the limit, WAF applies the rule
    --     action to all requests that satisfy the scope-down statement.
    --
    --     With this option, you must configure the @ScopeDownStatement@
    --     property.
    --
    -- -   @CUSTOM_KEYS@ - Aggregate the request counts using one or more web
    --     request components as the aggregate keys.
    --
    --     With this option, you must specify the aggregate keys in the
    --     @CustomKeys@ property.
    --
    --     To aggregate on only the IP address or only the forwarded IP
    --     address, don\'t use custom keys. Instead, set the aggregate key type
    --     to @IP@ or @FORWARDED_IP@.
    --
    -- -   @FORWARDED_IP@ - Aggregate the request counts on the first IP
    --     address in an HTTP header.
    --
    --     With this option, you must specify the header to use in the
    --     @ForwardedIPConfig@ property.
    --
    --     To aggregate on a combination of the forwarded IP address with other
    --     aggregate keys, use @CUSTOM_KEYS@.
    --
    -- -   @IP@ - Aggregate the request counts on the IP address from the web
    --     request origin.
    --
    --     To aggregate on a combination of the IP address with other aggregate
    --     keys, use @CUSTOM_KEYS@.
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
-- 'customKeys', 'rateBasedStatement_customKeys' - Specifies the aggregate keys to use in a rate-base rule.
--
-- 'forwardedIPConfig', 'rateBasedStatement_forwardedIPConfig' - The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- This is required if you specify a forwarded IP in the rule\'s aggregate
-- key settings.
--
-- 'scopeDownStatement', 'rateBasedStatement_scopeDownStatement' - An optional nested statement that narrows the scope of the web requests
-- that are evaluated and managed by the rate-based statement. When you use
-- a scope-down statement, the rate-based rule only tracks and rate limits
-- requests that match the scope-down statement. You can use any nestable
-- Statement in the scope-down statement, and you can nest statements at
-- any level, the same as you can for a rule statement.
--
-- 'limit', 'rateBasedStatement_limit' - The limit on requests per 5-minute period for a single aggregation
-- instance for the rate-based rule. If the rate-based statement includes a
-- @ScopeDownStatement@, this limit is applied only to the requests that
-- match the statement.
--
-- Examples:
--
-- -   If you aggregate on just the IP address, this is the limit on
--     requests from any single IP address.
--
-- -   If you aggregate on the HTTP method and the query argument name
--     \"city\", then this is the limit on requests for any single method,
--     city pair.
--
-- 'aggregateKeyType', 'rateBasedStatement_aggregateKeyType' - Setting that indicates how to aggregate the request counts.
--
-- Web requests that are missing any of the components specified in the
-- aggregation keys are omitted from the rate-based rule evaluation and
-- handling.
--
-- -   @CONSTANT@ - Count and limit the requests that match the rate-based
--     rule\'s scope-down statement. With this option, the counted requests
--     aren\'t further aggregated. The scope-down statement is the only
--     specification used. When the count of all requests that satisfy the
--     scope-down statement goes over the limit, WAF applies the rule
--     action to all requests that satisfy the scope-down statement.
--
--     With this option, you must configure the @ScopeDownStatement@
--     property.
--
-- -   @CUSTOM_KEYS@ - Aggregate the request counts using one or more web
--     request components as the aggregate keys.
--
--     With this option, you must specify the aggregate keys in the
--     @CustomKeys@ property.
--
--     To aggregate on only the IP address or only the forwarded IP
--     address, don\'t use custom keys. Instead, set the aggregate key type
--     to @IP@ or @FORWARDED_IP@.
--
-- -   @FORWARDED_IP@ - Aggregate the request counts on the first IP
--     address in an HTTP header.
--
--     With this option, you must specify the header to use in the
--     @ForwardedIPConfig@ property.
--
--     To aggregate on a combination of the forwarded IP address with other
--     aggregate keys, use @CUSTOM_KEYS@.
--
-- -   @IP@ - Aggregate the request counts on the IP address from the web
--     request origin.
--
--     To aggregate on a combination of the IP address with other aggregate
--     keys, use @CUSTOM_KEYS@.
newRateBasedStatement ::
  -- | 'limit'
  Prelude.Natural ->
  -- | 'aggregateKeyType'
  RateBasedStatementAggregateKeyType ->
  RateBasedStatement
newRateBasedStatement pLimit_ pAggregateKeyType_ =
  RateBasedStatement'
    { customKeys = Prelude.Nothing,
      forwardedIPConfig = Prelude.Nothing,
      scopeDownStatement = Prelude.Nothing,
      limit = pLimit_,
      aggregateKeyType = pAggregateKeyType_
    }

-- | Specifies the aggregate keys to use in a rate-base rule.
rateBasedStatement_customKeys :: Lens.Lens' RateBasedStatement (Prelude.Maybe (Prelude.NonEmpty RateBasedStatementCustomKey))
rateBasedStatement_customKeys = Lens.lens (\RateBasedStatement' {customKeys} -> customKeys) (\s@RateBasedStatement' {} a -> s {customKeys = a} :: RateBasedStatement) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- This is required if you specify a forwarded IP in the rule\'s aggregate
-- key settings.
rateBasedStatement_forwardedIPConfig :: Lens.Lens' RateBasedStatement (Prelude.Maybe ForwardedIPConfig)
rateBasedStatement_forwardedIPConfig = Lens.lens (\RateBasedStatement' {forwardedIPConfig} -> forwardedIPConfig) (\s@RateBasedStatement' {} a -> s {forwardedIPConfig = a} :: RateBasedStatement)

-- | An optional nested statement that narrows the scope of the web requests
-- that are evaluated and managed by the rate-based statement. When you use
-- a scope-down statement, the rate-based rule only tracks and rate limits
-- requests that match the scope-down statement. You can use any nestable
-- Statement in the scope-down statement, and you can nest statements at
-- any level, the same as you can for a rule statement.
rateBasedStatement_scopeDownStatement :: Lens.Lens' RateBasedStatement (Prelude.Maybe Statement)
rateBasedStatement_scopeDownStatement = Lens.lens (\RateBasedStatement' {scopeDownStatement} -> scopeDownStatement) (\s@RateBasedStatement' {} a -> s {scopeDownStatement = a} :: RateBasedStatement)

-- | The limit on requests per 5-minute period for a single aggregation
-- instance for the rate-based rule. If the rate-based statement includes a
-- @ScopeDownStatement@, this limit is applied only to the requests that
-- match the statement.
--
-- Examples:
--
-- -   If you aggregate on just the IP address, this is the limit on
--     requests from any single IP address.
--
-- -   If you aggregate on the HTTP method and the query argument name
--     \"city\", then this is the limit on requests for any single method,
--     city pair.
rateBasedStatement_limit :: Lens.Lens' RateBasedStatement Prelude.Natural
rateBasedStatement_limit = Lens.lens (\RateBasedStatement' {limit} -> limit) (\s@RateBasedStatement' {} a -> s {limit = a} :: RateBasedStatement)

-- | Setting that indicates how to aggregate the request counts.
--
-- Web requests that are missing any of the components specified in the
-- aggregation keys are omitted from the rate-based rule evaluation and
-- handling.
--
-- -   @CONSTANT@ - Count and limit the requests that match the rate-based
--     rule\'s scope-down statement. With this option, the counted requests
--     aren\'t further aggregated. The scope-down statement is the only
--     specification used. When the count of all requests that satisfy the
--     scope-down statement goes over the limit, WAF applies the rule
--     action to all requests that satisfy the scope-down statement.
--
--     With this option, you must configure the @ScopeDownStatement@
--     property.
--
-- -   @CUSTOM_KEYS@ - Aggregate the request counts using one or more web
--     request components as the aggregate keys.
--
--     With this option, you must specify the aggregate keys in the
--     @CustomKeys@ property.
--
--     To aggregate on only the IP address or only the forwarded IP
--     address, don\'t use custom keys. Instead, set the aggregate key type
--     to @IP@ or @FORWARDED_IP@.
--
-- -   @FORWARDED_IP@ - Aggregate the request counts on the first IP
--     address in an HTTP header.
--
--     With this option, you must specify the header to use in the
--     @ForwardedIPConfig@ property.
--
--     To aggregate on a combination of the forwarded IP address with other
--     aggregate keys, use @CUSTOM_KEYS@.
--
-- -   @IP@ - Aggregate the request counts on the IP address from the web
--     request origin.
--
--     To aggregate on a combination of the IP address with other aggregate
--     keys, use @CUSTOM_KEYS@.
rateBasedStatement_aggregateKeyType :: Lens.Lens' RateBasedStatement RateBasedStatementAggregateKeyType
rateBasedStatement_aggregateKeyType = Lens.lens (\RateBasedStatement' {aggregateKeyType} -> aggregateKeyType) (\s@RateBasedStatement' {} a -> s {aggregateKeyType = a} :: RateBasedStatement)

instance Data.FromJSON RateBasedStatement where
  parseJSON =
    Data.withObject
      "RateBasedStatement"
      ( \x ->
          RateBasedStatement'
            Prelude.<$> (x Data..:? "CustomKeys")
            Prelude.<*> (x Data..:? "ForwardedIPConfig")
            Prelude.<*> (x Data..:? "ScopeDownStatement")
            Prelude.<*> (x Data..: "Limit")
            Prelude.<*> (x Data..: "AggregateKeyType")
      )

instance Prelude.Hashable RateBasedStatement where
  hashWithSalt _salt RateBasedStatement' {..} =
    _salt
      `Prelude.hashWithSalt` customKeys
      `Prelude.hashWithSalt` forwardedIPConfig
      `Prelude.hashWithSalt` scopeDownStatement
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` aggregateKeyType

instance Prelude.NFData RateBasedStatement where
  rnf RateBasedStatement' {..} =
    Prelude.rnf customKeys
      `Prelude.seq` Prelude.rnf forwardedIPConfig
      `Prelude.seq` Prelude.rnf scopeDownStatement
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf aggregateKeyType

instance Data.ToJSON RateBasedStatement where
  toJSON RateBasedStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomKeys" Data..=) Prelude.<$> customKeys,
            ("ForwardedIPConfig" Data..=)
              Prelude.<$> forwardedIPConfig,
            ("ScopeDownStatement" Data..=)
              Prelude.<$> scopeDownStatement,
            Prelude.Just ("Limit" Data..= limit),
            Prelude.Just
              ("AggregateKeyType" Data..= aggregateKeyType)
          ]
      )
