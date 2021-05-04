{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig where

import Network.AWS.CloudFront.Types.CookieNames
import Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromXML
    OriginRequestPolicyCookiesConfig
  where
  parseXML x =
    OriginRequestPolicyCookiesConfig'
      Prelude.<$> (x Prelude..@? "Cookies")
      Prelude.<*> (x Prelude..@ "CookieBehavior")

instance
  Prelude.Hashable
    OriginRequestPolicyCookiesConfig

instance
  Prelude.NFData
    OriginRequestPolicyCookiesConfig

instance
  Prelude.ToXML
    OriginRequestPolicyCookiesConfig
  where
  toXML OriginRequestPolicyCookiesConfig' {..} =
    Prelude.mconcat
      [ "Cookies" Prelude.@= cookies,
        "CookieBehavior" Prelude.@= cookieBehavior
      ]
