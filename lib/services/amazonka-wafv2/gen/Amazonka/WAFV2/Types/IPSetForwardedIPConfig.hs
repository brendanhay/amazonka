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
-- Module      : Amazonka.WAFV2.Types.IPSetForwardedIPConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.IPSetForwardedIPConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FallbackBehavior
import Amazonka.WAFV2.Types.ForwardedIPPosition

-- | The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- This configuration is used only for IPSetReferenceStatement. For
-- GeoMatchStatement and RateBasedStatement, use ForwardedIPConfig instead.
--
-- /See:/ 'newIPSetForwardedIPConfig' smart constructor.
data IPSetForwardedIPConfig = IPSetForwardedIPConfig'
  { -- | The name of the HTTP header to use for the IP address. For example, to
    -- use the X-Forwarded-For (XFF) header, set this to @X-Forwarded-For@.
    --
    -- If the specified header isn\'t present in the request, WAF doesn\'t
    -- apply the rule to the web request at all.
    headerName :: Prelude.Text,
    -- | The match status to assign to the web request if the request doesn\'t
    -- have a valid IP address in the specified position.
    --
    -- If the specified header isn\'t present in the request, WAF doesn\'t
    -- apply the rule to the web request at all.
    --
    -- You can specify the following fallback behaviors:
    --
    -- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
    --     applies the rule action to the request.
    --
    -- -   @NO_MATCH@ - Treat the web request as not matching the rule
    --     statement.
    fallbackBehavior :: FallbackBehavior,
    -- | The position in the header to search for the IP address. The header can
    -- contain IP addresses of the original client and also of proxies. For
    -- example, the header value could be @10.1.1.1, 127.0.0.0, 10.10.10.10@
    -- where the first IP address identifies the original client and the rest
    -- identify proxies that the request went through.
    --
    -- The options for this setting are the following:
    --
    -- -   FIRST - Inspect the first IP address in the list of IP addresses in
    --     the header. This is usually the client\'s original IP.
    --
    -- -   LAST - Inspect the last IP address in the list of IP addresses in
    --     the header.
    --
    -- -   ANY - Inspect all IP addresses in the header for a match. If the
    --     header contains more than 10 IP addresses, WAF inspects the last 10.
    position :: ForwardedIPPosition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSetForwardedIPConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerName', 'iPSetForwardedIPConfig_headerName' - The name of the HTTP header to use for the IP address. For example, to
-- use the X-Forwarded-For (XFF) header, set this to @X-Forwarded-For@.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- 'fallbackBehavior', 'iPSetForwardedIPConfig_fallbackBehavior' - The match status to assign to the web request if the request doesn\'t
-- have a valid IP address in the specified position.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- You can specify the following fallback behaviors:
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
--
-- 'position', 'iPSetForwardedIPConfig_position' - The position in the header to search for the IP address. The header can
-- contain IP addresses of the original client and also of proxies. For
-- example, the header value could be @10.1.1.1, 127.0.0.0, 10.10.10.10@
-- where the first IP address identifies the original client and the rest
-- identify proxies that the request went through.
--
-- The options for this setting are the following:
--
-- -   FIRST - Inspect the first IP address in the list of IP addresses in
--     the header. This is usually the client\'s original IP.
--
-- -   LAST - Inspect the last IP address in the list of IP addresses in
--     the header.
--
-- -   ANY - Inspect all IP addresses in the header for a match. If the
--     header contains more than 10 IP addresses, WAF inspects the last 10.
newIPSetForwardedIPConfig ::
  -- | 'headerName'
  Prelude.Text ->
  -- | 'fallbackBehavior'
  FallbackBehavior ->
  -- | 'position'
  ForwardedIPPosition ->
  IPSetForwardedIPConfig
newIPSetForwardedIPConfig
  pHeaderName_
  pFallbackBehavior_
  pPosition_ =
    IPSetForwardedIPConfig'
      { headerName = pHeaderName_,
        fallbackBehavior = pFallbackBehavior_,
        position = pPosition_
      }

-- | The name of the HTTP header to use for the IP address. For example, to
-- use the X-Forwarded-For (XFF) header, set this to @X-Forwarded-For@.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
iPSetForwardedIPConfig_headerName :: Lens.Lens' IPSetForwardedIPConfig Prelude.Text
iPSetForwardedIPConfig_headerName = Lens.lens (\IPSetForwardedIPConfig' {headerName} -> headerName) (\s@IPSetForwardedIPConfig' {} a -> s {headerName = a} :: IPSetForwardedIPConfig)

-- | The match status to assign to the web request if the request doesn\'t
-- have a valid IP address in the specified position.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- You can specify the following fallback behaviors:
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
iPSetForwardedIPConfig_fallbackBehavior :: Lens.Lens' IPSetForwardedIPConfig FallbackBehavior
iPSetForwardedIPConfig_fallbackBehavior = Lens.lens (\IPSetForwardedIPConfig' {fallbackBehavior} -> fallbackBehavior) (\s@IPSetForwardedIPConfig' {} a -> s {fallbackBehavior = a} :: IPSetForwardedIPConfig)

-- | The position in the header to search for the IP address. The header can
-- contain IP addresses of the original client and also of proxies. For
-- example, the header value could be @10.1.1.1, 127.0.0.0, 10.10.10.10@
-- where the first IP address identifies the original client and the rest
-- identify proxies that the request went through.
--
-- The options for this setting are the following:
--
-- -   FIRST - Inspect the first IP address in the list of IP addresses in
--     the header. This is usually the client\'s original IP.
--
-- -   LAST - Inspect the last IP address in the list of IP addresses in
--     the header.
--
-- -   ANY - Inspect all IP addresses in the header for a match. If the
--     header contains more than 10 IP addresses, WAF inspects the last 10.
iPSetForwardedIPConfig_position :: Lens.Lens' IPSetForwardedIPConfig ForwardedIPPosition
iPSetForwardedIPConfig_position = Lens.lens (\IPSetForwardedIPConfig' {position} -> position) (\s@IPSetForwardedIPConfig' {} a -> s {position = a} :: IPSetForwardedIPConfig)

instance Data.FromJSON IPSetForwardedIPConfig where
  parseJSON =
    Data.withObject
      "IPSetForwardedIPConfig"
      ( \x ->
          IPSetForwardedIPConfig'
            Prelude.<$> (x Data..: "HeaderName")
            Prelude.<*> (x Data..: "FallbackBehavior")
            Prelude.<*> (x Data..: "Position")
      )

instance Prelude.Hashable IPSetForwardedIPConfig where
  hashWithSalt _salt IPSetForwardedIPConfig' {..} =
    _salt `Prelude.hashWithSalt` headerName
      `Prelude.hashWithSalt` fallbackBehavior
      `Prelude.hashWithSalt` position

instance Prelude.NFData IPSetForwardedIPConfig where
  rnf IPSetForwardedIPConfig' {..} =
    Prelude.rnf headerName
      `Prelude.seq` Prelude.rnf fallbackBehavior
      `Prelude.seq` Prelude.rnf position

instance Data.ToJSON IPSetForwardedIPConfig where
  toJSON IPSetForwardedIPConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("HeaderName" Data..= headerName),
            Prelude.Just
              ("FallbackBehavior" Data..= fallbackBehavior),
            Prelude.Just ("Position" Data..= position)
          ]
      )
