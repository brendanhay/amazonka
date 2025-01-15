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
-- Module      : Amazonka.OpenSearch.Types.StorageType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.StorageType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.StorageTypeLimit
import qualified Amazonka.Prelude as Prelude

-- | A list of storage types for an Amazon OpenSearch Service domain that are
-- available for a given intance type.
--
-- /See:/ 'newStorageType' smart constructor.
data StorageType = StorageType'
  { -- | The storage sub-type, such as @gp3@ or @io1@.
    storageSubTypeName :: Prelude.Maybe Prelude.Text,
    -- | Limits that are applicable for the given storage type.
    storageTypeLimits :: Prelude.Maybe [StorageTypeLimit],
    -- | The name of the storage type.
    storageTypeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageSubTypeName', 'storageType_storageSubTypeName' - The storage sub-type, such as @gp3@ or @io1@.
--
-- 'storageTypeLimits', 'storageType_storageTypeLimits' - Limits that are applicable for the given storage type.
--
-- 'storageTypeName', 'storageType_storageTypeName' - The name of the storage type.
newStorageType ::
  StorageType
newStorageType =
  StorageType'
    { storageSubTypeName = Prelude.Nothing,
      storageTypeLimits = Prelude.Nothing,
      storageTypeName = Prelude.Nothing
    }

-- | The storage sub-type, such as @gp3@ or @io1@.
storageType_storageSubTypeName :: Lens.Lens' StorageType (Prelude.Maybe Prelude.Text)
storageType_storageSubTypeName = Lens.lens (\StorageType' {storageSubTypeName} -> storageSubTypeName) (\s@StorageType' {} a -> s {storageSubTypeName = a} :: StorageType)

-- | Limits that are applicable for the given storage type.
storageType_storageTypeLimits :: Lens.Lens' StorageType (Prelude.Maybe [StorageTypeLimit])
storageType_storageTypeLimits = Lens.lens (\StorageType' {storageTypeLimits} -> storageTypeLimits) (\s@StorageType' {} a -> s {storageTypeLimits = a} :: StorageType) Prelude.. Lens.mapping Lens.coerced

-- | The name of the storage type.
storageType_storageTypeName :: Lens.Lens' StorageType (Prelude.Maybe Prelude.Text)
storageType_storageTypeName = Lens.lens (\StorageType' {storageTypeName} -> storageTypeName) (\s@StorageType' {} a -> s {storageTypeName = a} :: StorageType)

instance Data.FromJSON StorageType where
  parseJSON =
    Data.withObject
      "StorageType"
      ( \x ->
          StorageType'
            Prelude.<$> (x Data..:? "StorageSubTypeName")
            Prelude.<*> ( x
                            Data..:? "StorageTypeLimits"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StorageTypeName")
      )

instance Prelude.Hashable StorageType where
  hashWithSalt _salt StorageType' {..} =
    _salt
      `Prelude.hashWithSalt` storageSubTypeName
      `Prelude.hashWithSalt` storageTypeLimits
      `Prelude.hashWithSalt` storageTypeName

instance Prelude.NFData StorageType where
  rnf StorageType' {..} =
    Prelude.rnf storageSubTypeName `Prelude.seq`
      Prelude.rnf storageTypeLimits `Prelude.seq`
        Prelude.rnf storageTypeName
