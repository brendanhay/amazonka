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
-- Module      : Network.AWS.AppSync.Types.CachingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.CachingConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The caching configuration for a resolver that has caching enabled.
--
-- /See:/ 'newCachingConfig' smart constructor.
data CachingConfig = CachingConfig'
  { -- | The TTL in seconds for a resolver that has caching enabled.
    --
    -- Valid values are between 1 and 3600 seconds.
    ttl :: Core.Maybe Core.Integer,
    -- | The caching keys for a resolver that has caching enabled.
    --
    -- Valid values are entries from the @$context.arguments@,
    -- @$context.source@, and @$context.identity@ maps.
    cachingKeys :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CachingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ttl', 'cachingConfig_ttl' - The TTL in seconds for a resolver that has caching enabled.
--
-- Valid values are between 1 and 3600 seconds.
--
-- 'cachingKeys', 'cachingConfig_cachingKeys' - The caching keys for a resolver that has caching enabled.
--
-- Valid values are entries from the @$context.arguments@,
-- @$context.source@, and @$context.identity@ maps.
newCachingConfig ::
  CachingConfig
newCachingConfig =
  CachingConfig'
    { ttl = Core.Nothing,
      cachingKeys = Core.Nothing
    }

-- | The TTL in seconds for a resolver that has caching enabled.
--
-- Valid values are between 1 and 3600 seconds.
cachingConfig_ttl :: Lens.Lens' CachingConfig (Core.Maybe Core.Integer)
cachingConfig_ttl = Lens.lens (\CachingConfig' {ttl} -> ttl) (\s@CachingConfig' {} a -> s {ttl = a} :: CachingConfig)

-- | The caching keys for a resolver that has caching enabled.
--
-- Valid values are entries from the @$context.arguments@,
-- @$context.source@, and @$context.identity@ maps.
cachingConfig_cachingKeys :: Lens.Lens' CachingConfig (Core.Maybe [Core.Text])
cachingConfig_cachingKeys = Lens.lens (\CachingConfig' {cachingKeys} -> cachingKeys) (\s@CachingConfig' {} a -> s {cachingKeys = a} :: CachingConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON CachingConfig where
  parseJSON =
    Core.withObject
      "CachingConfig"
      ( \x ->
          CachingConfig'
            Core.<$> (x Core..:? "ttl")
            Core.<*> (x Core..:? "cachingKeys" Core..!= Core.mempty)
      )

instance Core.Hashable CachingConfig

instance Core.NFData CachingConfig

instance Core.ToJSON CachingConfig where
  toJSON CachingConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ttl" Core..=) Core.<$> ttl,
            ("cachingKeys" Core..=) Core.<$> cachingKeys
          ]
      )
