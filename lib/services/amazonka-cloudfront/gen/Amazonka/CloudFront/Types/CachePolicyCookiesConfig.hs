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
-- Module      : Amazonka.CloudFront.Types.CachePolicyCookiesConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CachePolicyCookiesConfig where

import Amazonka.CloudFront.Types.CachePolicyCookieBehavior
import Amazonka.CloudFront.Types.CookieNames
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that determines whether any cookies in viewer requests (and if
-- so, which cookies) are included in the cache key and automatically
-- included in requests that CloudFront sends to the origin.
--
-- /See:/ 'newCachePolicyCookiesConfig' smart constructor.
data CachePolicyCookiesConfig = CachePolicyCookiesConfig'
  { cookies :: Prelude.Maybe CookieNames,
    -- | Determines whether any cookies in viewer requests are included in the
    -- cache key and automatically included in requests that CloudFront sends
    -- to the origin. Valid values are:
    --
    -- -   @none@ – Cookies in viewer requests are not included in the cache
    --     key and are not automatically included in requests that CloudFront
    --     sends to the origin. Even when this field is set to @none@, any
    --     cookies that are listed in an @OriginRequestPolicy@ /are/ included
    --     in origin requests.
    --
    -- -   @whitelist@ – The cookies in viewer requests that are listed in the
    --     @CookieNames@ type are included in the cache key and automatically
    --     included in requests that CloudFront sends to the origin.
    --
    -- -   @allExcept@ – All cookies in viewer requests that are /__not__/
    --     listed in the @CookieNames@ type are included in the cache key and
    --     automatically included in requests that CloudFront sends to the
    --     origin.
    --
    -- -   @all@ – All cookies in viewer requests are included in the cache key
    --     and are automatically included in requests that CloudFront sends to
    --     the origin.
    cookieBehavior :: CachePolicyCookieBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CachePolicyCookiesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cookies', 'cachePolicyCookiesConfig_cookies' - Undocumented member.
--
-- 'cookieBehavior', 'cachePolicyCookiesConfig_cookieBehavior' - Determines whether any cookies in viewer requests are included in the
-- cache key and automatically included in requests that CloudFront sends
-- to the origin. Valid values are:
--
-- -   @none@ – Cookies in viewer requests are not included in the cache
--     key and are not automatically included in requests that CloudFront
--     sends to the origin. Even when this field is set to @none@, any
--     cookies that are listed in an @OriginRequestPolicy@ /are/ included
--     in origin requests.
--
-- -   @whitelist@ – The cookies in viewer requests that are listed in the
--     @CookieNames@ type are included in the cache key and automatically
--     included in requests that CloudFront sends to the origin.
--
-- -   @allExcept@ – All cookies in viewer requests that are /__not__/
--     listed in the @CookieNames@ type are included in the cache key and
--     automatically included in requests that CloudFront sends to the
--     origin.
--
-- -   @all@ – All cookies in viewer requests are included in the cache key
--     and are automatically included in requests that CloudFront sends to
--     the origin.
newCachePolicyCookiesConfig ::
  -- | 'cookieBehavior'
  CachePolicyCookieBehavior ->
  CachePolicyCookiesConfig
newCachePolicyCookiesConfig pCookieBehavior_ =
  CachePolicyCookiesConfig'
    { cookies =
        Prelude.Nothing,
      cookieBehavior = pCookieBehavior_
    }

-- | Undocumented member.
cachePolicyCookiesConfig_cookies :: Lens.Lens' CachePolicyCookiesConfig (Prelude.Maybe CookieNames)
cachePolicyCookiesConfig_cookies = Lens.lens (\CachePolicyCookiesConfig' {cookies} -> cookies) (\s@CachePolicyCookiesConfig' {} a -> s {cookies = a} :: CachePolicyCookiesConfig)

-- | Determines whether any cookies in viewer requests are included in the
-- cache key and automatically included in requests that CloudFront sends
-- to the origin. Valid values are:
--
-- -   @none@ – Cookies in viewer requests are not included in the cache
--     key and are not automatically included in requests that CloudFront
--     sends to the origin. Even when this field is set to @none@, any
--     cookies that are listed in an @OriginRequestPolicy@ /are/ included
--     in origin requests.
--
-- -   @whitelist@ – The cookies in viewer requests that are listed in the
--     @CookieNames@ type are included in the cache key and automatically
--     included in requests that CloudFront sends to the origin.
--
-- -   @allExcept@ – All cookies in viewer requests that are /__not__/
--     listed in the @CookieNames@ type are included in the cache key and
--     automatically included in requests that CloudFront sends to the
--     origin.
--
-- -   @all@ – All cookies in viewer requests are included in the cache key
--     and are automatically included in requests that CloudFront sends to
--     the origin.
cachePolicyCookiesConfig_cookieBehavior :: Lens.Lens' CachePolicyCookiesConfig CachePolicyCookieBehavior
cachePolicyCookiesConfig_cookieBehavior = Lens.lens (\CachePolicyCookiesConfig' {cookieBehavior} -> cookieBehavior) (\s@CachePolicyCookiesConfig' {} a -> s {cookieBehavior = a} :: CachePolicyCookiesConfig)

instance Data.FromXML CachePolicyCookiesConfig where
  parseXML x =
    CachePolicyCookiesConfig'
      Prelude.<$> (x Data..@? "Cookies")
      Prelude.<*> (x Data..@ "CookieBehavior")

instance Prelude.Hashable CachePolicyCookiesConfig where
  hashWithSalt _salt CachePolicyCookiesConfig' {..} =
    _salt
      `Prelude.hashWithSalt` cookies
      `Prelude.hashWithSalt` cookieBehavior

instance Prelude.NFData CachePolicyCookiesConfig where
  rnf CachePolicyCookiesConfig' {..} =
    Prelude.rnf cookies
      `Prelude.seq` Prelude.rnf cookieBehavior

instance Data.ToXML CachePolicyCookiesConfig where
  toXML CachePolicyCookiesConfig' {..} =
    Prelude.mconcat
      [ "Cookies" Data.@= cookies,
        "CookieBehavior" Data.@= cookieBehavior
      ]
