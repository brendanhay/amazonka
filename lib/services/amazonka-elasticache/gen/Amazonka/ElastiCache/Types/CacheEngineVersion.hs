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
-- Module      : Amazonka.ElastiCache.Types.CacheEngineVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheEngineVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides all of the details about a particular cache engine version.
--
-- /See:/ 'newCacheEngineVersion' smart constructor.
data CacheEngineVersion = CacheEngineVersion'
  { -- | The description of the cache engine version.
    cacheEngineVersionDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache parameter group family associated with this cache
    -- engine.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
    -- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
    -- @redis6.x@
    cacheParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The description of the cache engine.
    cacheEngineDescription :: Prelude.Maybe Prelude.Text,
    -- | The version number of the cache engine.
    engineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheEngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheEngineVersionDescription', 'cacheEngineVersion_cacheEngineVersionDescription' - The description of the cache engine version.
--
-- 'engine', 'cacheEngineVersion_engine' - The name of the cache engine.
--
-- 'cacheParameterGroupFamily', 'cacheEngineVersion_cacheParameterGroupFamily' - The name of the cache parameter group family associated with this cache
-- engine.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@
--
-- 'cacheEngineDescription', 'cacheEngineVersion_cacheEngineDescription' - The description of the cache engine.
--
-- 'engineVersion', 'cacheEngineVersion_engineVersion' - The version number of the cache engine.
newCacheEngineVersion ::
  CacheEngineVersion
newCacheEngineVersion =
  CacheEngineVersion'
    { cacheEngineVersionDescription =
        Prelude.Nothing,
      engine = Prelude.Nothing,
      cacheParameterGroupFamily = Prelude.Nothing,
      cacheEngineDescription = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | The description of the cache engine version.
cacheEngineVersion_cacheEngineVersionDescription :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_cacheEngineVersionDescription = Lens.lens (\CacheEngineVersion' {cacheEngineVersionDescription} -> cacheEngineVersionDescription) (\s@CacheEngineVersion' {} a -> s {cacheEngineVersionDescription = a} :: CacheEngineVersion)

-- | The name of the cache engine.
cacheEngineVersion_engine :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_engine = Lens.lens (\CacheEngineVersion' {engine} -> engine) (\s@CacheEngineVersion' {} a -> s {engine = a} :: CacheEngineVersion)

-- | The name of the cache parameter group family associated with this cache
-- engine.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@
cacheEngineVersion_cacheParameterGroupFamily :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_cacheParameterGroupFamily = Lens.lens (\CacheEngineVersion' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@CacheEngineVersion' {} a -> s {cacheParameterGroupFamily = a} :: CacheEngineVersion)

-- | The description of the cache engine.
cacheEngineVersion_cacheEngineDescription :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_cacheEngineDescription = Lens.lens (\CacheEngineVersion' {cacheEngineDescription} -> cacheEngineDescription) (\s@CacheEngineVersion' {} a -> s {cacheEngineDescription = a} :: CacheEngineVersion)

-- | The version number of the cache engine.
cacheEngineVersion_engineVersion :: Lens.Lens' CacheEngineVersion (Prelude.Maybe Prelude.Text)
cacheEngineVersion_engineVersion = Lens.lens (\CacheEngineVersion' {engineVersion} -> engineVersion) (\s@CacheEngineVersion' {} a -> s {engineVersion = a} :: CacheEngineVersion)

instance Core.FromXML CacheEngineVersion where
  parseXML x =
    CacheEngineVersion'
      Prelude.<$> (x Core..@? "CacheEngineVersionDescription")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "CacheParameterGroupFamily")
      Prelude.<*> (x Core..@? "CacheEngineDescription")
      Prelude.<*> (x Core..@? "EngineVersion")

instance Prelude.Hashable CacheEngineVersion where
  hashWithSalt _salt CacheEngineVersion' {..} =
    _salt
      `Prelude.hashWithSalt` cacheEngineVersionDescription
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` cacheParameterGroupFamily
      `Prelude.hashWithSalt` cacheEngineDescription
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData CacheEngineVersion where
  rnf CacheEngineVersion' {..} =
    Prelude.rnf cacheEngineVersionDescription
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf cacheParameterGroupFamily
      `Prelude.seq` Prelude.rnf cacheEngineDescription
      `Prelude.seq` Prelude.rnf engineVersion
