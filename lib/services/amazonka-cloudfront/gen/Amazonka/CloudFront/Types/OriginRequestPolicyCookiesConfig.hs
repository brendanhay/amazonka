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
-- Module      : Amazonka.CloudFront.Types.OriginRequestPolicyCookiesConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginRequestPolicyCookiesConfig where

import Amazonka.CloudFront.Types.CookieNames
import Amazonka.CloudFront.Types.OriginRequestPolicyCookieBehavior
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that determines whether any cookies in viewer requests (and if
-- so, which cookies) are included in requests that CloudFront sends to the
-- origin.
--
-- /See:/ 'newOriginRequestPolicyCookiesConfig' smart constructor.
data OriginRequestPolicyCookiesConfig = OriginRequestPolicyCookiesConfig'
  { cookies :: Prelude.Maybe CookieNames,
    -- | Determines whether cookies in viewer requests are included in requests
    -- that CloudFront sends to the origin. Valid values are:
    --
    -- -   @none@ – Cookies in viewer requests are not included in requests
    --     that CloudFront sends to the origin. Even when this field is set to
    --     @none@, any cookies that are listed in a @CachePolicy@ /are/
    --     included in origin requests.
    --
    -- -   @whitelist@ – The cookies in viewer requests that are listed in the
    --     @CookieNames@ type are included in requests that CloudFront sends to
    --     the origin.
    --
    -- -   @all@ – All cookies in viewer requests are included in requests that
    --     CloudFront sends to the origin.
    cookieBehavior :: OriginRequestPolicyCookieBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicyCookiesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cookies', 'originRequestPolicyCookiesConfig_cookies' - Undocumented member.
--
-- 'cookieBehavior', 'originRequestPolicyCookiesConfig_cookieBehavior' - Determines whether cookies in viewer requests are included in requests
-- that CloudFront sends to the origin. Valid values are:
--
-- -   @none@ – Cookies in viewer requests are not included in requests
--     that CloudFront sends to the origin. Even when this field is set to
--     @none@, any cookies that are listed in a @CachePolicy@ /are/
--     included in origin requests.
--
-- -   @whitelist@ – The cookies in viewer requests that are listed in the
--     @CookieNames@ type are included in requests that CloudFront sends to
--     the origin.
--
-- -   @all@ – All cookies in viewer requests are included in requests that
--     CloudFront sends to the origin.
newOriginRequestPolicyCookiesConfig ::
  -- | 'cookieBehavior'
  OriginRequestPolicyCookieBehavior ->
  OriginRequestPolicyCookiesConfig
newOriginRequestPolicyCookiesConfig pCookieBehavior_ =
  OriginRequestPolicyCookiesConfig'
    { cookies =
        Prelude.Nothing,
      cookieBehavior = pCookieBehavior_
    }

-- | Undocumented member.
originRequestPolicyCookiesConfig_cookies :: Lens.Lens' OriginRequestPolicyCookiesConfig (Prelude.Maybe CookieNames)
originRequestPolicyCookiesConfig_cookies = Lens.lens (\OriginRequestPolicyCookiesConfig' {cookies} -> cookies) (\s@OriginRequestPolicyCookiesConfig' {} a -> s {cookies = a} :: OriginRequestPolicyCookiesConfig)

-- | Determines whether cookies in viewer requests are included in requests
-- that CloudFront sends to the origin. Valid values are:
--
-- -   @none@ – Cookies in viewer requests are not included in requests
--     that CloudFront sends to the origin. Even when this field is set to
--     @none@, any cookies that are listed in a @CachePolicy@ /are/
--     included in origin requests.
--
-- -   @whitelist@ – The cookies in viewer requests that are listed in the
--     @CookieNames@ type are included in requests that CloudFront sends to
--     the origin.
--
-- -   @all@ – All cookies in viewer requests are included in requests that
--     CloudFront sends to the origin.
originRequestPolicyCookiesConfig_cookieBehavior :: Lens.Lens' OriginRequestPolicyCookiesConfig OriginRequestPolicyCookieBehavior
originRequestPolicyCookiesConfig_cookieBehavior = Lens.lens (\OriginRequestPolicyCookiesConfig' {cookieBehavior} -> cookieBehavior) (\s@OriginRequestPolicyCookiesConfig' {} a -> s {cookieBehavior = a} :: OriginRequestPolicyCookiesConfig)

instance
  Data.FromXML
    OriginRequestPolicyCookiesConfig
  where
  parseXML x =
    OriginRequestPolicyCookiesConfig'
      Prelude.<$> (x Data..@? "Cookies")
      Prelude.<*> (x Data..@ "CookieBehavior")

instance
  Prelude.Hashable
    OriginRequestPolicyCookiesConfig
  where
  hashWithSalt
    _salt
    OriginRequestPolicyCookiesConfig' {..} =
      _salt
        `Prelude.hashWithSalt` cookies
        `Prelude.hashWithSalt` cookieBehavior

instance
  Prelude.NFData
    OriginRequestPolicyCookiesConfig
  where
  rnf OriginRequestPolicyCookiesConfig' {..} =
    Prelude.rnf cookies `Prelude.seq`
      Prelude.rnf cookieBehavior

instance Data.ToXML OriginRequestPolicyCookiesConfig where
  toXML OriginRequestPolicyCookiesConfig' {..} =
    Prelude.mconcat
      [ "Cookies" Data.@= cookies,
        "CookieBehavior" Data.@= cookieBehavior
      ]
