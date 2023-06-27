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
-- Module      : Amazonka.FinSpace.Types.KxCacheStorageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxCacheStorageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for read only disk cache associated with a cluster.
--
-- /See:/ 'newKxCacheStorageConfiguration' smart constructor.
data KxCacheStorageConfiguration = KxCacheStorageConfiguration'
  { -- | The type of cache storage . The valid values are:
    --
    -- -   CACHE_1000 – This type provides at least 1000 MB\/s disk access
    --     throughput.
    type' :: Prelude.Text,
    -- | The size of cache in Gigabytes.
    size :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxCacheStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'kxCacheStorageConfiguration_type' - The type of cache storage . The valid values are:
--
-- -   CACHE_1000 – This type provides at least 1000 MB\/s disk access
--     throughput.
--
-- 'size', 'kxCacheStorageConfiguration_size' - The size of cache in Gigabytes.
newKxCacheStorageConfiguration ::
  -- | 'type''
  Prelude.Text ->
  -- | 'size'
  Prelude.Natural ->
  KxCacheStorageConfiguration
newKxCacheStorageConfiguration pType_ pSize_ =
  KxCacheStorageConfiguration'
    { type' = pType_,
      size = pSize_
    }

-- | The type of cache storage . The valid values are:
--
-- -   CACHE_1000 – This type provides at least 1000 MB\/s disk access
--     throughput.
kxCacheStorageConfiguration_type :: Lens.Lens' KxCacheStorageConfiguration Prelude.Text
kxCacheStorageConfiguration_type = Lens.lens (\KxCacheStorageConfiguration' {type'} -> type') (\s@KxCacheStorageConfiguration' {} a -> s {type' = a} :: KxCacheStorageConfiguration)

-- | The size of cache in Gigabytes.
kxCacheStorageConfiguration_size :: Lens.Lens' KxCacheStorageConfiguration Prelude.Natural
kxCacheStorageConfiguration_size = Lens.lens (\KxCacheStorageConfiguration' {size} -> size) (\s@KxCacheStorageConfiguration' {} a -> s {size = a} :: KxCacheStorageConfiguration)

instance Data.FromJSON KxCacheStorageConfiguration where
  parseJSON =
    Data.withObject
      "KxCacheStorageConfiguration"
      ( \x ->
          KxCacheStorageConfiguration'
            Prelude.<$> (x Data..: "type")
            Prelude.<*> (x Data..: "size")
      )

instance Prelude.Hashable KxCacheStorageConfiguration where
  hashWithSalt _salt KxCacheStorageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` size

instance Prelude.NFData KxCacheStorageConfiguration where
  rnf KxCacheStorageConfiguration' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf size

instance Data.ToJSON KxCacheStorageConfiguration where
  toJSON KxCacheStorageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Data..= type'),
            Prelude.Just ("size" Data..= size)
          ]
      )
