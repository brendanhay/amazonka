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
-- Module      : Amazonka.WAFV2.Types.RateBasedStatementCustomKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateBasedStatementCustomKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.RateLimitCookie
import Amazonka.WAFV2.Types.RateLimitForwardedIP
import Amazonka.WAFV2.Types.RateLimitHTTPMethod
import Amazonka.WAFV2.Types.RateLimitHeader
import Amazonka.WAFV2.Types.RateLimitIP
import Amazonka.WAFV2.Types.RateLimitLabelNamespace
import Amazonka.WAFV2.Types.RateLimitQueryArgument
import Amazonka.WAFV2.Types.RateLimitQueryString

-- | Specifies a single custom aggregate key for a rate-base rule.
--
-- Web requests that are missing any of the components specified in the
-- aggregation keys are omitted from the rate-based rule evaluation and
-- handling.
--
-- /See:/ 'newRateBasedStatementCustomKey' smart constructor.
data RateBasedStatementCustomKey = RateBasedStatementCustomKey'
  { -- | Use the value of a cookie in the request as an aggregate key. Each
    -- distinct value in the cookie contributes to the aggregation instance. If
    -- you use a single cookie as your custom key, then each value fully
    -- defines an aggregation instance.
    cookie :: Prelude.Maybe RateLimitCookie,
    -- | Use the first IP address in an HTTP header as an aggregate key. Each
    -- distinct forwarded IP address contributes to the aggregation instance.
    --
    -- When you specify an IP or forwarded IP in the custom key settings, you
    -- must also specify at least one other key to use. You can aggregate on
    -- only the forwarded IP address by specifying @FORWARDED_IP@ in your
    -- rate-based statement\'s @AggregateKeyType@.
    --
    -- With this option, you must specify the header to use in the rate-based
    -- rule\'s @ForwardedIPConfig@ property.
    forwardedIP :: Prelude.Maybe RateLimitForwardedIP,
    -- | Use the request\'s HTTP method as an aggregate key. Each distinct HTTP
    -- method contributes to the aggregation instance. If you use just the HTTP
    -- method as your custom key, then each method fully defines an aggregation
    -- instance.
    hTTPMethod :: Prelude.Maybe RateLimitHTTPMethod,
    -- | Use the value of a header in the request as an aggregate key. Each
    -- distinct value in the header contributes to the aggregation instance. If
    -- you use a single header as your custom key, then each value fully
    -- defines an aggregation instance.
    header :: Prelude.Maybe RateLimitHeader,
    -- | Use the request\'s originating IP address as an aggregate key. Each
    -- distinct IP address contributes to the aggregation instance.
    --
    -- When you specify an IP or forwarded IP in the custom key settings, you
    -- must also specify at least one other key to use. You can aggregate on
    -- only the IP address by specifying @IP@ in your rate-based statement\'s
    -- @AggregateKeyType@.
    ip :: Prelude.Maybe RateLimitIP,
    -- | Use the specified label namespace as an aggregate key. Each distinct
    -- fully qualified label name that has the specified label namespace
    -- contributes to the aggregation instance. If you use just one label
    -- namespace as your custom key, then each label name fully defines an
    -- aggregation instance.
    --
    -- This uses only labels that have been added to the request by rules that
    -- are evaluated before this rate-based rule in the web ACL.
    --
    -- For information about label namespaces and names, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rule-label-requirements.html Label syntax and naming requirements>
    -- in the /WAF Developer Guide/.
    labelNamespace :: Prelude.Maybe RateLimitLabelNamespace,
    -- | Use the specified query argument as an aggregate key. Each distinct
    -- value for the named query argument contributes to the aggregation
    -- instance. If you use a single query argument as your custom key, then
    -- each value fully defines an aggregation instance.
    queryArgument :: Prelude.Maybe RateLimitQueryArgument,
    -- | Use the request\'s query string as an aggregate key. Each distinct
    -- string contributes to the aggregation instance. If you use just the
    -- query string as your custom key, then each string fully defines an
    -- aggregation instance.
    queryString :: Prelude.Maybe RateLimitQueryString
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateBasedStatementCustomKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cookie', 'rateBasedStatementCustomKey_cookie' - Use the value of a cookie in the request as an aggregate key. Each
-- distinct value in the cookie contributes to the aggregation instance. If
-- you use a single cookie as your custom key, then each value fully
-- defines an aggregation instance.
--
-- 'forwardedIP', 'rateBasedStatementCustomKey_forwardedIP' - Use the first IP address in an HTTP header as an aggregate key. Each
-- distinct forwarded IP address contributes to the aggregation instance.
--
-- When you specify an IP or forwarded IP in the custom key settings, you
-- must also specify at least one other key to use. You can aggregate on
-- only the forwarded IP address by specifying @FORWARDED_IP@ in your
-- rate-based statement\'s @AggregateKeyType@.
--
-- With this option, you must specify the header to use in the rate-based
-- rule\'s @ForwardedIPConfig@ property.
--
-- 'hTTPMethod', 'rateBasedStatementCustomKey_hTTPMethod' - Use the request\'s HTTP method as an aggregate key. Each distinct HTTP
-- method contributes to the aggregation instance. If you use just the HTTP
-- method as your custom key, then each method fully defines an aggregation
-- instance.
--
-- 'header', 'rateBasedStatementCustomKey_header' - Use the value of a header in the request as an aggregate key. Each
-- distinct value in the header contributes to the aggregation instance. If
-- you use a single header as your custom key, then each value fully
-- defines an aggregation instance.
--
-- 'ip', 'rateBasedStatementCustomKey_ip' - Use the request\'s originating IP address as an aggregate key. Each
-- distinct IP address contributes to the aggregation instance.
--
-- When you specify an IP or forwarded IP in the custom key settings, you
-- must also specify at least one other key to use. You can aggregate on
-- only the IP address by specifying @IP@ in your rate-based statement\'s
-- @AggregateKeyType@.
--
-- 'labelNamespace', 'rateBasedStatementCustomKey_labelNamespace' - Use the specified label namespace as an aggregate key. Each distinct
-- fully qualified label name that has the specified label namespace
-- contributes to the aggregation instance. If you use just one label
-- namespace as your custom key, then each label name fully defines an
-- aggregation instance.
--
-- This uses only labels that have been added to the request by rules that
-- are evaluated before this rate-based rule in the web ACL.
--
-- For information about label namespaces and names, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rule-label-requirements.html Label syntax and naming requirements>
-- in the /WAF Developer Guide/.
--
-- 'queryArgument', 'rateBasedStatementCustomKey_queryArgument' - Use the specified query argument as an aggregate key. Each distinct
-- value for the named query argument contributes to the aggregation
-- instance. If you use a single query argument as your custom key, then
-- each value fully defines an aggregation instance.
--
-- 'queryString', 'rateBasedStatementCustomKey_queryString' - Use the request\'s query string as an aggregate key. Each distinct
-- string contributes to the aggregation instance. If you use just the
-- query string as your custom key, then each string fully defines an
-- aggregation instance.
newRateBasedStatementCustomKey ::
  RateBasedStatementCustomKey
newRateBasedStatementCustomKey =
  RateBasedStatementCustomKey'
    { cookie =
        Prelude.Nothing,
      forwardedIP = Prelude.Nothing,
      hTTPMethod = Prelude.Nothing,
      header = Prelude.Nothing,
      ip = Prelude.Nothing,
      labelNamespace = Prelude.Nothing,
      queryArgument = Prelude.Nothing,
      queryString = Prelude.Nothing
    }

-- | Use the value of a cookie in the request as an aggregate key. Each
-- distinct value in the cookie contributes to the aggregation instance. If
-- you use a single cookie as your custom key, then each value fully
-- defines an aggregation instance.
rateBasedStatementCustomKey_cookie :: Lens.Lens' RateBasedStatementCustomKey (Prelude.Maybe RateLimitCookie)
rateBasedStatementCustomKey_cookie = Lens.lens (\RateBasedStatementCustomKey' {cookie} -> cookie) (\s@RateBasedStatementCustomKey' {} a -> s {cookie = a} :: RateBasedStatementCustomKey)

-- | Use the first IP address in an HTTP header as an aggregate key. Each
-- distinct forwarded IP address contributes to the aggregation instance.
--
-- When you specify an IP or forwarded IP in the custom key settings, you
-- must also specify at least one other key to use. You can aggregate on
-- only the forwarded IP address by specifying @FORWARDED_IP@ in your
-- rate-based statement\'s @AggregateKeyType@.
--
-- With this option, you must specify the header to use in the rate-based
-- rule\'s @ForwardedIPConfig@ property.
rateBasedStatementCustomKey_forwardedIP :: Lens.Lens' RateBasedStatementCustomKey (Prelude.Maybe RateLimitForwardedIP)
rateBasedStatementCustomKey_forwardedIP = Lens.lens (\RateBasedStatementCustomKey' {forwardedIP} -> forwardedIP) (\s@RateBasedStatementCustomKey' {} a -> s {forwardedIP = a} :: RateBasedStatementCustomKey)

-- | Use the request\'s HTTP method as an aggregate key. Each distinct HTTP
-- method contributes to the aggregation instance. If you use just the HTTP
-- method as your custom key, then each method fully defines an aggregation
-- instance.
rateBasedStatementCustomKey_hTTPMethod :: Lens.Lens' RateBasedStatementCustomKey (Prelude.Maybe RateLimitHTTPMethod)
rateBasedStatementCustomKey_hTTPMethod = Lens.lens (\RateBasedStatementCustomKey' {hTTPMethod} -> hTTPMethod) (\s@RateBasedStatementCustomKey' {} a -> s {hTTPMethod = a} :: RateBasedStatementCustomKey)

-- | Use the value of a header in the request as an aggregate key. Each
-- distinct value in the header contributes to the aggregation instance. If
-- you use a single header as your custom key, then each value fully
-- defines an aggregation instance.
rateBasedStatementCustomKey_header :: Lens.Lens' RateBasedStatementCustomKey (Prelude.Maybe RateLimitHeader)
rateBasedStatementCustomKey_header = Lens.lens (\RateBasedStatementCustomKey' {header} -> header) (\s@RateBasedStatementCustomKey' {} a -> s {header = a} :: RateBasedStatementCustomKey)

-- | Use the request\'s originating IP address as an aggregate key. Each
-- distinct IP address contributes to the aggregation instance.
--
-- When you specify an IP or forwarded IP in the custom key settings, you
-- must also specify at least one other key to use. You can aggregate on
-- only the IP address by specifying @IP@ in your rate-based statement\'s
-- @AggregateKeyType@.
rateBasedStatementCustomKey_ip :: Lens.Lens' RateBasedStatementCustomKey (Prelude.Maybe RateLimitIP)
rateBasedStatementCustomKey_ip = Lens.lens (\RateBasedStatementCustomKey' {ip} -> ip) (\s@RateBasedStatementCustomKey' {} a -> s {ip = a} :: RateBasedStatementCustomKey)

-- | Use the specified label namespace as an aggregate key. Each distinct
-- fully qualified label name that has the specified label namespace
-- contributes to the aggregation instance. If you use just one label
-- namespace as your custom key, then each label name fully defines an
-- aggregation instance.
--
-- This uses only labels that have been added to the request by rules that
-- are evaluated before this rate-based rule in the web ACL.
--
-- For information about label namespaces and names, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rule-label-requirements.html Label syntax and naming requirements>
-- in the /WAF Developer Guide/.
rateBasedStatementCustomKey_labelNamespace :: Lens.Lens' RateBasedStatementCustomKey (Prelude.Maybe RateLimitLabelNamespace)
rateBasedStatementCustomKey_labelNamespace = Lens.lens (\RateBasedStatementCustomKey' {labelNamespace} -> labelNamespace) (\s@RateBasedStatementCustomKey' {} a -> s {labelNamespace = a} :: RateBasedStatementCustomKey)

-- | Use the specified query argument as an aggregate key. Each distinct
-- value for the named query argument contributes to the aggregation
-- instance. If you use a single query argument as your custom key, then
-- each value fully defines an aggregation instance.
rateBasedStatementCustomKey_queryArgument :: Lens.Lens' RateBasedStatementCustomKey (Prelude.Maybe RateLimitQueryArgument)
rateBasedStatementCustomKey_queryArgument = Lens.lens (\RateBasedStatementCustomKey' {queryArgument} -> queryArgument) (\s@RateBasedStatementCustomKey' {} a -> s {queryArgument = a} :: RateBasedStatementCustomKey)

-- | Use the request\'s query string as an aggregate key. Each distinct
-- string contributes to the aggregation instance. If you use just the
-- query string as your custom key, then each string fully defines an
-- aggregation instance.
rateBasedStatementCustomKey_queryString :: Lens.Lens' RateBasedStatementCustomKey (Prelude.Maybe RateLimitQueryString)
rateBasedStatementCustomKey_queryString = Lens.lens (\RateBasedStatementCustomKey' {queryString} -> queryString) (\s@RateBasedStatementCustomKey' {} a -> s {queryString = a} :: RateBasedStatementCustomKey)

instance Data.FromJSON RateBasedStatementCustomKey where
  parseJSON =
    Data.withObject
      "RateBasedStatementCustomKey"
      ( \x ->
          RateBasedStatementCustomKey'
            Prelude.<$> (x Data..:? "Cookie")
            Prelude.<*> (x Data..:? "ForwardedIP")
            Prelude.<*> (x Data..:? "HTTPMethod")
            Prelude.<*> (x Data..:? "Header")
            Prelude.<*> (x Data..:? "IP")
            Prelude.<*> (x Data..:? "LabelNamespace")
            Prelude.<*> (x Data..:? "QueryArgument")
            Prelude.<*> (x Data..:? "QueryString")
      )

instance Prelude.Hashable RateBasedStatementCustomKey where
  hashWithSalt _salt RateBasedStatementCustomKey' {..} =
    _salt
      `Prelude.hashWithSalt` cookie
      `Prelude.hashWithSalt` forwardedIP
      `Prelude.hashWithSalt` hTTPMethod
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` labelNamespace
      `Prelude.hashWithSalt` queryArgument
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData RateBasedStatementCustomKey where
  rnf RateBasedStatementCustomKey' {..} =
    Prelude.rnf cookie
      `Prelude.seq` Prelude.rnf forwardedIP
      `Prelude.seq` Prelude.rnf hTTPMethod
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf ip
      `Prelude.seq` Prelude.rnf labelNamespace
      `Prelude.seq` Prelude.rnf queryArgument
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToJSON RateBasedStatementCustomKey where
  toJSON RateBasedStatementCustomKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Cookie" Data..=) Prelude.<$> cookie,
            ("ForwardedIP" Data..=) Prelude.<$> forwardedIP,
            ("HTTPMethod" Data..=) Prelude.<$> hTTPMethod,
            ("Header" Data..=) Prelude.<$> header,
            ("IP" Data..=) Prelude.<$> ip,
            ("LabelNamespace" Data..=)
              Prelude.<$> labelNamespace,
            ("QueryArgument" Data..=) Prelude.<$> queryArgument,
            ("QueryString" Data..=) Prelude.<$> queryString
          ]
      )
