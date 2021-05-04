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
-- Module      : Network.AWS.CloudFront.Types.CachePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicy where

import Network.AWS.CloudFront.Types.CachePolicyConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A cache policy.
--
-- When it’s attached to a cache behavior, the cache policy determines the
-- following:
--
-- -   The values that CloudFront includes in the cache key. These values
--     can include HTTP headers, cookies, and URL query strings. CloudFront
--     uses the cache key to find an object in its cache that it can return
--     to the viewer.
--
-- -   The default, minimum, and maximum time to live (TTL) values that you
--     want objects to stay in the CloudFront cache.
--
-- The headers, cookies, and query strings that are included in the cache
-- key are automatically included in requests that CloudFront sends to the
-- origin. CloudFront sends a request when it can’t find a valid object in
-- its cache that matches the request’s cache key. If you want to send
-- values to the origin but /not/ include them in the cache key, use
-- @OriginRequestPolicy@.
--
-- /See:/ 'newCachePolicy' smart constructor.
data CachePolicy = CachePolicy'
  { -- | The unique identifier for the cache policy.
    id :: Prelude.Text,
    -- | The date and time when the cache policy was last modified.
    lastModifiedTime :: Prelude.ISO8601,
    -- | The cache policy configuration.
    cachePolicyConfig :: CachePolicyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CachePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'cachePolicy_id' - The unique identifier for the cache policy.
--
-- 'lastModifiedTime', 'cachePolicy_lastModifiedTime' - The date and time when the cache policy was last modified.
--
-- 'cachePolicyConfig', 'cachePolicy_cachePolicyConfig' - The cache policy configuration.
newCachePolicy ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'cachePolicyConfig'
  CachePolicyConfig ->
  CachePolicy
newCachePolicy
  pId_
  pLastModifiedTime_
  pCachePolicyConfig_ =
    CachePolicy'
      { id = pId_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_,
        cachePolicyConfig = pCachePolicyConfig_
      }

-- | The unique identifier for the cache policy.
cachePolicy_id :: Lens.Lens' CachePolicy Prelude.Text
cachePolicy_id = Lens.lens (\CachePolicy' {id} -> id) (\s@CachePolicy' {} a -> s {id = a} :: CachePolicy)

-- | The date and time when the cache policy was last modified.
cachePolicy_lastModifiedTime :: Lens.Lens' CachePolicy Prelude.UTCTime
cachePolicy_lastModifiedTime = Lens.lens (\CachePolicy' {lastModifiedTime} -> lastModifiedTime) (\s@CachePolicy' {} a -> s {lastModifiedTime = a} :: CachePolicy) Prelude.. Prelude._Time

-- | The cache policy configuration.
cachePolicy_cachePolicyConfig :: Lens.Lens' CachePolicy CachePolicyConfig
cachePolicy_cachePolicyConfig = Lens.lens (\CachePolicy' {cachePolicyConfig} -> cachePolicyConfig) (\s@CachePolicy' {} a -> s {cachePolicyConfig = a} :: CachePolicy)

instance Prelude.FromXML CachePolicy where
  parseXML x =
    CachePolicy'
      Prelude.<$> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "LastModifiedTime")
      Prelude.<*> (x Prelude..@ "CachePolicyConfig")

instance Prelude.Hashable CachePolicy

instance Prelude.NFData CachePolicy
