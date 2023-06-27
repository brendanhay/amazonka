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
-- Module      : Amazonka.FinSpace.Types.KxDatabaseCacheConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxDatabaseCacheConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure of database cache configuration that is used for mapping
-- database paths to cache types in clusters.
--
-- /See:/ 'newKxDatabaseCacheConfiguration' smart constructor.
data KxDatabaseCacheConfiguration = KxDatabaseCacheConfiguration'
  { -- | The type of disk cache. This parameter is used to map the database path
    -- to cache storage. The valid values are:
    --
    -- -   CACHE_1000 – This type provides at least 1000 MB\/s disk access
    --     throughput.
    cacheType :: Prelude.Text,
    -- | Specifies the portions of database that will be loaded into the cache
    -- for access.
    dbPaths :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxDatabaseCacheConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheType', 'kxDatabaseCacheConfiguration_cacheType' - The type of disk cache. This parameter is used to map the database path
-- to cache storage. The valid values are:
--
-- -   CACHE_1000 – This type provides at least 1000 MB\/s disk access
--     throughput.
--
-- 'dbPaths', 'kxDatabaseCacheConfiguration_dbPaths' - Specifies the portions of database that will be loaded into the cache
-- for access.
newKxDatabaseCacheConfiguration ::
  -- | 'cacheType'
  Prelude.Text ->
  KxDatabaseCacheConfiguration
newKxDatabaseCacheConfiguration pCacheType_ =
  KxDatabaseCacheConfiguration'
    { cacheType =
        pCacheType_,
      dbPaths = Prelude.mempty
    }

-- | The type of disk cache. This parameter is used to map the database path
-- to cache storage. The valid values are:
--
-- -   CACHE_1000 – This type provides at least 1000 MB\/s disk access
--     throughput.
kxDatabaseCacheConfiguration_cacheType :: Lens.Lens' KxDatabaseCacheConfiguration Prelude.Text
kxDatabaseCacheConfiguration_cacheType = Lens.lens (\KxDatabaseCacheConfiguration' {cacheType} -> cacheType) (\s@KxDatabaseCacheConfiguration' {} a -> s {cacheType = a} :: KxDatabaseCacheConfiguration)

-- | Specifies the portions of database that will be loaded into the cache
-- for access.
kxDatabaseCacheConfiguration_dbPaths :: Lens.Lens' KxDatabaseCacheConfiguration [Prelude.Text]
kxDatabaseCacheConfiguration_dbPaths = Lens.lens (\KxDatabaseCacheConfiguration' {dbPaths} -> dbPaths) (\s@KxDatabaseCacheConfiguration' {} a -> s {dbPaths = a} :: KxDatabaseCacheConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON KxDatabaseCacheConfiguration where
  parseJSON =
    Data.withObject
      "KxDatabaseCacheConfiguration"
      ( \x ->
          KxDatabaseCacheConfiguration'
            Prelude.<$> (x Data..: "cacheType")
            Prelude.<*> (x Data..:? "dbPaths" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    KxDatabaseCacheConfiguration
  where
  hashWithSalt _salt KxDatabaseCacheConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cacheType
      `Prelude.hashWithSalt` dbPaths

instance Prelude.NFData KxDatabaseCacheConfiguration where
  rnf KxDatabaseCacheConfiguration' {..} =
    Prelude.rnf cacheType
      `Prelude.seq` Prelude.rnf dbPaths

instance Data.ToJSON KxDatabaseCacheConfiguration where
  toJSON KxDatabaseCacheConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("cacheType" Data..= cacheType),
            Prelude.Just ("dbPaths" Data..= dbPaths)
          ]
      )
