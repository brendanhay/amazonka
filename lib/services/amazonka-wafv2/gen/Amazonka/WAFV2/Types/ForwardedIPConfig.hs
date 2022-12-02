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
-- Module      : Amazonka.WAFV2.Types.ForwardedIPConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ForwardedIPConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FallbackBehavior

-- | The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- This configuration is used for GeoMatchStatement and RateBasedStatement.
-- For IPSetReferenceStatement, use IPSetForwardedIPConfig instead.
--
-- WAF only evaluates the first IP address found in the specified HTTP
-- header.
--
-- /See:/ 'newForwardedIPConfig' smart constructor.
data ForwardedIPConfig = ForwardedIPConfig'
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
    fallbackBehavior :: FallbackBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForwardedIPConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerName', 'forwardedIPConfig_headerName' - The name of the HTTP header to use for the IP address. For example, to
-- use the X-Forwarded-For (XFF) header, set this to @X-Forwarded-For@.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- 'fallbackBehavior', 'forwardedIPConfig_fallbackBehavior' - The match status to assign to the web request if the request doesn\'t
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
newForwardedIPConfig ::
  -- | 'headerName'
  Prelude.Text ->
  -- | 'fallbackBehavior'
  FallbackBehavior ->
  ForwardedIPConfig
newForwardedIPConfig pHeaderName_ pFallbackBehavior_ =
  ForwardedIPConfig'
    { headerName = pHeaderName_,
      fallbackBehavior = pFallbackBehavior_
    }

-- | The name of the HTTP header to use for the IP address. For example, to
-- use the X-Forwarded-For (XFF) header, set this to @X-Forwarded-For@.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
forwardedIPConfig_headerName :: Lens.Lens' ForwardedIPConfig Prelude.Text
forwardedIPConfig_headerName = Lens.lens (\ForwardedIPConfig' {headerName} -> headerName) (\s@ForwardedIPConfig' {} a -> s {headerName = a} :: ForwardedIPConfig)

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
forwardedIPConfig_fallbackBehavior :: Lens.Lens' ForwardedIPConfig FallbackBehavior
forwardedIPConfig_fallbackBehavior = Lens.lens (\ForwardedIPConfig' {fallbackBehavior} -> fallbackBehavior) (\s@ForwardedIPConfig' {} a -> s {fallbackBehavior = a} :: ForwardedIPConfig)

instance Data.FromJSON ForwardedIPConfig where
  parseJSON =
    Data.withObject
      "ForwardedIPConfig"
      ( \x ->
          ForwardedIPConfig'
            Prelude.<$> (x Data..: "HeaderName")
            Prelude.<*> (x Data..: "FallbackBehavior")
      )

instance Prelude.Hashable ForwardedIPConfig where
  hashWithSalt _salt ForwardedIPConfig' {..} =
    _salt `Prelude.hashWithSalt` headerName
      `Prelude.hashWithSalt` fallbackBehavior

instance Prelude.NFData ForwardedIPConfig where
  rnf ForwardedIPConfig' {..} =
    Prelude.rnf headerName
      `Prelude.seq` Prelude.rnf fallbackBehavior

instance Data.ToJSON ForwardedIPConfig where
  toJSON ForwardedIPConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("HeaderName" Data..= headerName),
            Prelude.Just
              ("FallbackBehavior" Data..= fallbackBehavior)
          ]
      )
