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
-- Module      : Network.AWS.AppSync.Types.CachingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.CachingConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The caching configuration for a resolver that has caching enabled.
--
-- /See:/ 'newCachingConfig' smart constructor.
data CachingConfig = CachingConfig'
  { -- | The TTL in seconds for a resolver that has caching enabled.
    --
    -- Valid values are between 1 and 3600 seconds.
    ttl :: Prelude.Maybe Prelude.Integer,
    -- | The caching keys for a resolver that has caching enabled.
    --
    -- Valid values are entries from the @$context.arguments@,
    -- @$context.source@, and @$context.identity@ maps.
    cachingKeys :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ttl = Prelude.Nothing,
      cachingKeys = Prelude.Nothing
    }

-- | The TTL in seconds for a resolver that has caching enabled.
--
-- Valid values are between 1 and 3600 seconds.
cachingConfig_ttl :: Lens.Lens' CachingConfig (Prelude.Maybe Prelude.Integer)
cachingConfig_ttl = Lens.lens (\CachingConfig' {ttl} -> ttl) (\s@CachingConfig' {} a -> s {ttl = a} :: CachingConfig)

-- | The caching keys for a resolver that has caching enabled.
--
-- Valid values are entries from the @$context.arguments@,
-- @$context.source@, and @$context.identity@ maps.
cachingConfig_cachingKeys :: Lens.Lens' CachingConfig (Prelude.Maybe [Prelude.Text])
cachingConfig_cachingKeys = Lens.lens (\CachingConfig' {cachingKeys} -> cachingKeys) (\s@CachingConfig' {} a -> s {cachingKeys = a} :: CachingConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON CachingConfig where
  parseJSON =
    Prelude.withObject
      "CachingConfig"
      ( \x ->
          CachingConfig'
            Prelude.<$> (x Prelude..:? "ttl")
            Prelude.<*> ( x Prelude..:? "cachingKeys"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CachingConfig

instance Prelude.NFData CachingConfig

instance Prelude.ToJSON CachingConfig where
  toJSON CachingConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ttl" Prelude..=) Prelude.<$> ttl,
            ("cachingKeys" Prelude..=) Prelude.<$> cachingKeys
          ]
      )
