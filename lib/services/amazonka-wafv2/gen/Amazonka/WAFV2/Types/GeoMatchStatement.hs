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
-- Module      : Amazonka.WAFV2.Types.GeoMatchStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.GeoMatchStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CountryCode
import Amazonka.WAFV2.Types.ForwardedIPConfig

-- | A rule statement that labels web requests by country and region and that
-- matches against web requests based on country code. A geo match rule
-- labels every request that it inspects regardless of whether it finds a
-- match.
--
-- -   To manage requests only by country, you can use this statement by
--     itself and specify the countries that you want to match against in
--     the @CountryCodes@ array.
--
-- -   Otherwise, configure your geo match rule with Count action so that
--     it only labels requests. Then, add one or more label match rules to
--     run after the geo match rule and configure them to match against the
--     geographic labels and handle the requests as needed.
--
-- WAF labels requests using the alpha-2 country and region codes from the
-- International Organization for Standardization (ISO) 3166 standard. WAF
-- determines the codes using either the IP address in the web request
-- origin or, if you specify it, the address in the geo match
-- @ForwardedIPConfig@.
--
-- If you use the web request origin, the label formats are
-- @awswaf:clientip:geo:region:\<ISO country code>-\<ISO region code>@ and
-- @awswaf:clientip:geo:country:\<ISO country code>@.
--
-- If you use a forwarded IP address, the label formats are
-- @awswaf:forwardedip:geo:region:\<ISO country code>-\<ISO region code>@
-- and @awswaf:forwardedip:geo:country:\<ISO country code>@.
--
-- For additional details, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rule-statement-type-geo-match.html Geographic match rule statement>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- /See:/ 'newGeoMatchStatement' smart constructor.
data GeoMatchStatement = GeoMatchStatement'
  { -- | An array of two-character country codes that you want to match against,
    -- for example, @[ \"US\", \"CN\" ]@, from the alpha-2 country ISO codes of
    -- the ISO 3166 international standard.
    --
    -- When you use a geo match statement just for the region and country
    -- labels that it adds to requests, you still have to supply a country code
    -- for the rule to evaluate. In this case, you configure the rule to only
    -- count matching requests, but it will still generate logging and count
    -- metrics for any matches. You can reduce the logging and metrics that the
    -- rule produces by specifying a country that\'s unlikely to be a source of
    -- traffic to your site.
    countryCodes :: Prelude.Maybe (Prelude.NonEmpty CountryCode),
    -- | The configuration for inspecting IP addresses in an HTTP header that you
    -- specify, instead of using the IP address that\'s reported by the web
    -- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
    -- you can specify any header name.
    --
    -- If the specified header isn\'t present in the request, WAF doesn\'t
    -- apply the rule to the web request at all.
    forwardedIPConfig :: Prelude.Maybe ForwardedIPConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeoMatchStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countryCodes', 'geoMatchStatement_countryCodes' - An array of two-character country codes that you want to match against,
-- for example, @[ \"US\", \"CN\" ]@, from the alpha-2 country ISO codes of
-- the ISO 3166 international standard.
--
-- When you use a geo match statement just for the region and country
-- labels that it adds to requests, you still have to supply a country code
-- for the rule to evaluate. In this case, you configure the rule to only
-- count matching requests, but it will still generate logging and count
-- metrics for any matches. You can reduce the logging and metrics that the
-- rule produces by specifying a country that\'s unlikely to be a source of
-- traffic to your site.
--
-- 'forwardedIPConfig', 'geoMatchStatement_forwardedIPConfig' - The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
newGeoMatchStatement ::
  GeoMatchStatement
newGeoMatchStatement =
  GeoMatchStatement'
    { countryCodes = Prelude.Nothing,
      forwardedIPConfig = Prelude.Nothing
    }

-- | An array of two-character country codes that you want to match against,
-- for example, @[ \"US\", \"CN\" ]@, from the alpha-2 country ISO codes of
-- the ISO 3166 international standard.
--
-- When you use a geo match statement just for the region and country
-- labels that it adds to requests, you still have to supply a country code
-- for the rule to evaluate. In this case, you configure the rule to only
-- count matching requests, but it will still generate logging and count
-- metrics for any matches. You can reduce the logging and metrics that the
-- rule produces by specifying a country that\'s unlikely to be a source of
-- traffic to your site.
geoMatchStatement_countryCodes :: Lens.Lens' GeoMatchStatement (Prelude.Maybe (Prelude.NonEmpty CountryCode))
geoMatchStatement_countryCodes = Lens.lens (\GeoMatchStatement' {countryCodes} -> countryCodes) (\s@GeoMatchStatement' {} a -> s {countryCodes = a} :: GeoMatchStatement) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
geoMatchStatement_forwardedIPConfig :: Lens.Lens' GeoMatchStatement (Prelude.Maybe ForwardedIPConfig)
geoMatchStatement_forwardedIPConfig = Lens.lens (\GeoMatchStatement' {forwardedIPConfig} -> forwardedIPConfig) (\s@GeoMatchStatement' {} a -> s {forwardedIPConfig = a} :: GeoMatchStatement)

instance Data.FromJSON GeoMatchStatement where
  parseJSON =
    Data.withObject
      "GeoMatchStatement"
      ( \x ->
          GeoMatchStatement'
            Prelude.<$> (x Data..:? "CountryCodes")
            Prelude.<*> (x Data..:? "ForwardedIPConfig")
      )

instance Prelude.Hashable GeoMatchStatement where
  hashWithSalt _salt GeoMatchStatement' {..} =
    _salt
      `Prelude.hashWithSalt` countryCodes
      `Prelude.hashWithSalt` forwardedIPConfig

instance Prelude.NFData GeoMatchStatement where
  rnf GeoMatchStatement' {..} =
    Prelude.rnf countryCodes `Prelude.seq`
      Prelude.rnf forwardedIPConfig

instance Data.ToJSON GeoMatchStatement where
  toJSON GeoMatchStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CountryCodes" Data..=) Prelude.<$> countryCodes,
            ("ForwardedIPConfig" Data..=)
              Prelude.<$> forwardedIPConfig
          ]
      )
