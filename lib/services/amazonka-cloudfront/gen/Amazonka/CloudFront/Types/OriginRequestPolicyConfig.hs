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
-- Module      : Amazonka.CloudFront.Types.OriginRequestPolicyConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginRequestPolicyConfig where

import Amazonka.CloudFront.Types.OriginRequestPolicyCookiesConfig
import Amazonka.CloudFront.Types.OriginRequestPolicyHeadersConfig
import Amazonka.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An origin request policy configuration.
--
-- This configuration determines the values that CloudFront includes in
-- requests that it sends to the origin. Each request that CloudFront sends
-- to the origin includes the following:
--
-- -   The request body and the URL path (without the domain name) from the
--     viewer request.
--
-- -   The headers that CloudFront automatically includes in every origin
--     request, including @Host@, @User-Agent@, and @X-Amz-Cf-Id@.
--
-- -   All HTTP headers, cookies, and URL query strings that are specified
--     in the cache policy or the origin request policy. These can include
--     items from the viewer request and, in the case of headers,
--     additional ones that are added by CloudFront.
--
-- CloudFront sends a request when it can’t find an object in its cache
-- that matches the request. If you want to send values to the origin and
-- also include them in the cache key, use @CachePolicy@.
--
-- /See:/ 'newOriginRequestPolicyConfig' smart constructor.
data OriginRequestPolicyConfig = OriginRequestPolicyConfig'
  { -- | A comment to describe the origin request policy. The comment cannot be
    -- longer than 128 characters.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A unique name to identify the origin request policy.
    name :: Prelude.Text,
    -- | The HTTP headers to include in origin requests. These can include
    -- headers from viewer requests and additional headers added by CloudFront.
    headersConfig :: OriginRequestPolicyHeadersConfig,
    -- | The cookies from viewer requests to include in origin requests.
    cookiesConfig :: OriginRequestPolicyCookiesConfig,
    -- | The URL query strings from viewer requests to include in origin
    -- requests.
    queryStringsConfig :: OriginRequestPolicyQueryStringsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'originRequestPolicyConfig_comment' - A comment to describe the origin request policy. The comment cannot be
-- longer than 128 characters.
--
-- 'name', 'originRequestPolicyConfig_name' - A unique name to identify the origin request policy.
--
-- 'headersConfig', 'originRequestPolicyConfig_headersConfig' - The HTTP headers to include in origin requests. These can include
-- headers from viewer requests and additional headers added by CloudFront.
--
-- 'cookiesConfig', 'originRequestPolicyConfig_cookiesConfig' - The cookies from viewer requests to include in origin requests.
--
-- 'queryStringsConfig', 'originRequestPolicyConfig_queryStringsConfig' - The URL query strings from viewer requests to include in origin
-- requests.
newOriginRequestPolicyConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 'headersConfig'
  OriginRequestPolicyHeadersConfig ->
  -- | 'cookiesConfig'
  OriginRequestPolicyCookiesConfig ->
  -- | 'queryStringsConfig'
  OriginRequestPolicyQueryStringsConfig ->
  OriginRequestPolicyConfig
newOriginRequestPolicyConfig
  pName_
  pHeadersConfig_
  pCookiesConfig_
  pQueryStringsConfig_ =
    OriginRequestPolicyConfig'
      { comment =
          Prelude.Nothing,
        name = pName_,
        headersConfig = pHeadersConfig_,
        cookiesConfig = pCookiesConfig_,
        queryStringsConfig = pQueryStringsConfig_
      }

-- | A comment to describe the origin request policy. The comment cannot be
-- longer than 128 characters.
originRequestPolicyConfig_comment :: Lens.Lens' OriginRequestPolicyConfig (Prelude.Maybe Prelude.Text)
originRequestPolicyConfig_comment = Lens.lens (\OriginRequestPolicyConfig' {comment} -> comment) (\s@OriginRequestPolicyConfig' {} a -> s {comment = a} :: OriginRequestPolicyConfig)

-- | A unique name to identify the origin request policy.
originRequestPolicyConfig_name :: Lens.Lens' OriginRequestPolicyConfig Prelude.Text
originRequestPolicyConfig_name = Lens.lens (\OriginRequestPolicyConfig' {name} -> name) (\s@OriginRequestPolicyConfig' {} a -> s {name = a} :: OriginRequestPolicyConfig)

-- | The HTTP headers to include in origin requests. These can include
-- headers from viewer requests and additional headers added by CloudFront.
originRequestPolicyConfig_headersConfig :: Lens.Lens' OriginRequestPolicyConfig OriginRequestPolicyHeadersConfig
originRequestPolicyConfig_headersConfig = Lens.lens (\OriginRequestPolicyConfig' {headersConfig} -> headersConfig) (\s@OriginRequestPolicyConfig' {} a -> s {headersConfig = a} :: OriginRequestPolicyConfig)

-- | The cookies from viewer requests to include in origin requests.
originRequestPolicyConfig_cookiesConfig :: Lens.Lens' OriginRequestPolicyConfig OriginRequestPolicyCookiesConfig
originRequestPolicyConfig_cookiesConfig = Lens.lens (\OriginRequestPolicyConfig' {cookiesConfig} -> cookiesConfig) (\s@OriginRequestPolicyConfig' {} a -> s {cookiesConfig = a} :: OriginRequestPolicyConfig)

-- | The URL query strings from viewer requests to include in origin
-- requests.
originRequestPolicyConfig_queryStringsConfig :: Lens.Lens' OriginRequestPolicyConfig OriginRequestPolicyQueryStringsConfig
originRequestPolicyConfig_queryStringsConfig = Lens.lens (\OriginRequestPolicyConfig' {queryStringsConfig} -> queryStringsConfig) (\s@OriginRequestPolicyConfig' {} a -> s {queryStringsConfig = a} :: OriginRequestPolicyConfig)

instance Data.FromXML OriginRequestPolicyConfig where
  parseXML x =
    OriginRequestPolicyConfig'
      Prelude.<$> (x Data..@? "Comment")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "HeadersConfig")
      Prelude.<*> (x Data..@ "CookiesConfig")
      Prelude.<*> (x Data..@ "QueryStringsConfig")

instance Prelude.Hashable OriginRequestPolicyConfig where
  hashWithSalt _salt OriginRequestPolicyConfig' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` headersConfig
      `Prelude.hashWithSalt` cookiesConfig
      `Prelude.hashWithSalt` queryStringsConfig

instance Prelude.NFData OriginRequestPolicyConfig where
  rnf OriginRequestPolicyConfig' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf headersConfig
      `Prelude.seq` Prelude.rnf cookiesConfig
      `Prelude.seq` Prelude.rnf queryStringsConfig

instance Data.ToXML OriginRequestPolicyConfig where
  toXML OriginRequestPolicyConfig' {..} =
    Prelude.mconcat
      [ "Comment" Data.@= comment,
        "Name" Data.@= name,
        "HeadersConfig" Data.@= headersConfig,
        "CookiesConfig" Data.@= cookiesConfig,
        "QueryStringsConfig" Data.@= queryStringsConfig
      ]
