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
-- Module      : Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyHeadersConfig where

import Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior
import Network.AWS.CloudFront.Types.Headers
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that determines whether any HTTP headers (and if so, which
-- headers) are included in the cache key and automatically included in
-- requests that CloudFront sends to the origin.
--
-- /See:/ 'newCachePolicyHeadersConfig' smart constructor.
data CachePolicyHeadersConfig = CachePolicyHeadersConfig'
  { headers :: Prelude.Maybe Headers,
    -- | Determines whether any HTTP headers are included in the cache key and
    -- automatically included in requests that CloudFront sends to the origin.
    -- Valid values are:
    --
    -- -   @none@ – HTTP headers are not included in the cache key and are not
    --     automatically included in requests that CloudFront sends to the
    --     origin. Even when this field is set to @none@, any headers that are
    --     listed in an @OriginRequestPolicy@ /are/ included in origin
    --     requests.
    --
    -- -   @whitelist@ – The HTTP headers that are listed in the @Headers@ type
    --     are included in the cache key and are automatically included in
    --     requests that CloudFront sends to the origin.
    headerBehavior :: CachePolicyHeaderBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CachePolicyHeadersConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'cachePolicyHeadersConfig_headers' - Undocumented member.
--
-- 'headerBehavior', 'cachePolicyHeadersConfig_headerBehavior' - Determines whether any HTTP headers are included in the cache key and
-- automatically included in requests that CloudFront sends to the origin.
-- Valid values are:
--
-- -   @none@ – HTTP headers are not included in the cache key and are not
--     automatically included in requests that CloudFront sends to the
--     origin. Even when this field is set to @none@, any headers that are
--     listed in an @OriginRequestPolicy@ /are/ included in origin
--     requests.
--
-- -   @whitelist@ – The HTTP headers that are listed in the @Headers@ type
--     are included in the cache key and are automatically included in
--     requests that CloudFront sends to the origin.
newCachePolicyHeadersConfig ::
  -- | 'headerBehavior'
  CachePolicyHeaderBehavior ->
  CachePolicyHeadersConfig
newCachePolicyHeadersConfig pHeaderBehavior_ =
  CachePolicyHeadersConfig'
    { headers =
        Prelude.Nothing,
      headerBehavior = pHeaderBehavior_
    }

-- | Undocumented member.
cachePolicyHeadersConfig_headers :: Lens.Lens' CachePolicyHeadersConfig (Prelude.Maybe Headers)
cachePolicyHeadersConfig_headers = Lens.lens (\CachePolicyHeadersConfig' {headers} -> headers) (\s@CachePolicyHeadersConfig' {} a -> s {headers = a} :: CachePolicyHeadersConfig)

-- | Determines whether any HTTP headers are included in the cache key and
-- automatically included in requests that CloudFront sends to the origin.
-- Valid values are:
--
-- -   @none@ – HTTP headers are not included in the cache key and are not
--     automatically included in requests that CloudFront sends to the
--     origin. Even when this field is set to @none@, any headers that are
--     listed in an @OriginRequestPolicy@ /are/ included in origin
--     requests.
--
-- -   @whitelist@ – The HTTP headers that are listed in the @Headers@ type
--     are included in the cache key and are automatically included in
--     requests that CloudFront sends to the origin.
cachePolicyHeadersConfig_headerBehavior :: Lens.Lens' CachePolicyHeadersConfig CachePolicyHeaderBehavior
cachePolicyHeadersConfig_headerBehavior = Lens.lens (\CachePolicyHeadersConfig' {headerBehavior} -> headerBehavior) (\s@CachePolicyHeadersConfig' {} a -> s {headerBehavior = a} :: CachePolicyHeadersConfig)

instance Prelude.FromXML CachePolicyHeadersConfig where
  parseXML x =
    CachePolicyHeadersConfig'
      Prelude.<$> (x Prelude..@? "Headers")
      Prelude.<*> (x Prelude..@ "HeaderBehavior")

instance Prelude.Hashable CachePolicyHeadersConfig

instance Prelude.NFData CachePolicyHeadersConfig

instance Prelude.ToXML CachePolicyHeadersConfig where
  toXML CachePolicyHeadersConfig' {..} =
    Prelude.mconcat
      [ "Headers" Prelude.@= headers,
        "HeaderBehavior" Prelude.@= headerBehavior
      ]
