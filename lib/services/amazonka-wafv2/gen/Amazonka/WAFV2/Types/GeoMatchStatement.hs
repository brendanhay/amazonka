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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.GeoMatchStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CountryCode
import Amazonka.WAFV2.Types.ForwardedIPConfig

-- | A rule statement used to identify web requests based on country of
-- origin.
--
-- /See:/ 'newGeoMatchStatement' smart constructor.
data GeoMatchStatement = GeoMatchStatement'
  { -- | An array of two-character country codes, for example,
    -- @[ \"US\", \"CN\" ]@, from the alpha-2 country ISO codes of the ISO 3166
    -- international standard.
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
-- 'countryCodes', 'geoMatchStatement_countryCodes' - An array of two-character country codes, for example,
-- @[ \"US\", \"CN\" ]@, from the alpha-2 country ISO codes of the ISO 3166
-- international standard.
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

-- | An array of two-character country codes, for example,
-- @[ \"US\", \"CN\" ]@, from the alpha-2 country ISO codes of the ISO 3166
-- international standard.
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

instance Core.FromJSON GeoMatchStatement where
  parseJSON =
    Core.withObject
      "GeoMatchStatement"
      ( \x ->
          GeoMatchStatement'
            Prelude.<$> (x Core..:? "CountryCodes")
            Prelude.<*> (x Core..:? "ForwardedIPConfig")
      )

instance Prelude.Hashable GeoMatchStatement where
  hashWithSalt _salt GeoMatchStatement' {..} =
    _salt `Prelude.hashWithSalt` countryCodes
      `Prelude.hashWithSalt` forwardedIPConfig

instance Prelude.NFData GeoMatchStatement where
  rnf GeoMatchStatement' {..} =
    Prelude.rnf countryCodes
      `Prelude.seq` Prelude.rnf forwardedIPConfig

instance Core.ToJSON GeoMatchStatement where
  toJSON GeoMatchStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CountryCodes" Core..=) Prelude.<$> countryCodes,
            ("ForwardedIPConfig" Core..=)
              Prelude.<$> forwardedIPConfig
          ]
      )
