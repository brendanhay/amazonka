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
-- Module      : Network.AWS.ElastiCache.Types.CacheEngineVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheEngineVersion where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides all of the details about a particular cache engine version.
--
-- /See:/ 'newCacheEngineVersion' smart constructor.
data CacheEngineVersion = CacheEngineVersion'
  { -- | The description of the cache engine.
    cacheEngineDescription :: Prelude.Maybe Prelude.Text,
    -- | The description of the cache engine version.
    cacheEngineVersionDescription :: Prelude.Maybe Prelude.Text,
    -- | The version number of the cache engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache parameter group family associated with this cache
    -- engine.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
    -- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
    -- @redis6.x@ |
    cacheParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache engine.
    engine :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CacheEngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheEngineDescription', 'cacheEngineVersion_cacheEngineDescription' - The description of the cache engine.
--
-- 'cacheEngineVersionDescription', 'cacheEngineVersion_cacheEngineVersionDescription' - The description of the cache engine version.
--
-- 'engineVersion', 'cacheEngineVersion_engineVersion' - The version number of the cache engine.
--
-- 'cacheParameterGroupFamily', 'cacheEngineVersion_cacheParameterGroupFamily' - The name of the cache parameter group family associated with this cache
-- engine.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ |
--
-- 'engine', 'cacheEngineVersion_engine' - The name of the cache engine.
newCacheEngineVersion ::
  CacheEngineVersion
newCacheEngineVersion =
  CacheEngineVersion'
    { cacheEngineDescription =
        Prelude.Nothing,
      cacheEngineVersionDescription = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      cacheParameterGroupFamily = Prelude.Nothing,
      engine = Prelude.Nothing
    }

-- | The description of the cache engine.
cacheEngineVersion_cacheEngineDescription :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_cacheEngineDescription = Lens.lens (\CacheEngineVersion' {cacheEngineDescription} -> cacheEngineDescription) (\s@CacheEngineVersion' {} a -> s {cacheEngineDescription = a} :: CacheEngineVersion)

-- | The description of the cache engine version.
cacheEngineVersion_cacheEngineVersionDescription :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_cacheEngineVersionDescription = Lens.lens (\CacheEngineVersion' {cacheEngineVersionDescription} -> cacheEngineVersionDescription) (\s@CacheEngineVersion' {} a -> s {cacheEngineVersionDescription = a} :: CacheEngineVersion)

-- | The version number of the cache engine.
cacheEngineVersion_engineVersion :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_engineVersion = Lens.lens (\CacheEngineVersion' {engineVersion} -> engineVersion) (\s@CacheEngineVersion' {} a -> s {engineVersion = a} :: CacheEngineVersion)

-- | The name of the cache parameter group family associated with this cache
-- engine.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ |
cacheEngineVersion_cacheParameterGroupFamily :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_cacheParameterGroupFamily = Lens.lens (\CacheEngineVersion' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@CacheEngineVersion' {} a -> s {cacheParameterGroupFamily = a} :: CacheEngineVersion)

-- | The name of the cache engine.
cacheEngineVersion_engine :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_engine = Lens.lens (\CacheEngineVersion' {engine} -> engine) (\s@CacheEngineVersion' {} a -> s {engine = a} :: CacheEngineVersion)

instance Prelude.FromXML CacheEngineVersion where
  parseXML x =
    CacheEngineVersion'
      Prelude.<$> (x Prelude..@? "CacheEngineDescription")
      Prelude.<*> (x Prelude..@? "CacheEngineVersionDescription")
      Prelude.<*> (x Prelude..@? "EngineVersion")
      Prelude.<*> (x Prelude..@? "CacheParameterGroupFamily")
      Prelude.<*> (x Prelude..@? "Engine")

instance Prelude.Hashable CacheEngineVersion

instance Prelude.NFData CacheEngineVersion
