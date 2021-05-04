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
-- Module      : Network.AWS.ElasticSearch.Types.StorageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.StorageType where

import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | StorageTypes represents the list of storage related types and their
-- attributes that are available for given InstanceType.
--
-- /See:/ 'newStorageType' smart constructor.
data StorageType = StorageType'
  { -- | List of limits that are applicable for given storage type.
    storageTypeLimits :: Prelude.Maybe [StorageTypeLimit],
    storageTypeName :: Prelude.Maybe Prelude.Text,
    storageSubTypeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StorageType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageTypeLimits', 'storageType_storageTypeLimits' - List of limits that are applicable for given storage type.
--
-- 'storageTypeName', 'storageType_storageTypeName' - Undocumented member.
--
-- 'storageSubTypeName', 'storageType_storageSubTypeName' - Undocumented member.
newStorageType ::
  StorageType
newStorageType =
  StorageType'
    { storageTypeLimits = Prelude.Nothing,
      storageTypeName = Prelude.Nothing,
      storageSubTypeName = Prelude.Nothing
    }

-- | List of limits that are applicable for given storage type.
storageType_storageTypeLimits :: Lens.Lens' StorageType (Prelude.Maybe [StorageTypeLimit])
storageType_storageTypeLimits = Lens.lens (\StorageType' {storageTypeLimits} -> storageTypeLimits) (\s@StorageType' {} a -> s {storageTypeLimits = a} :: StorageType) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
storageType_storageTypeName :: Lens.Lens' StorageType (Prelude.Maybe Prelude.Text)
storageType_storageTypeName = Lens.lens (\StorageType' {storageTypeName} -> storageTypeName) (\s@StorageType' {} a -> s {storageTypeName = a} :: StorageType)

-- | Undocumented member.
storageType_storageSubTypeName :: Lens.Lens' StorageType (Prelude.Maybe Prelude.Text)
storageType_storageSubTypeName = Lens.lens (\StorageType' {storageSubTypeName} -> storageSubTypeName) (\s@StorageType' {} a -> s {storageSubTypeName = a} :: StorageType)

instance Prelude.FromJSON StorageType where
  parseJSON =
    Prelude.withObject
      "StorageType"
      ( \x ->
          StorageType'
            Prelude.<$> ( x Prelude..:? "StorageTypeLimits"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "StorageTypeName")
            Prelude.<*> (x Prelude..:? "StorageSubTypeName")
      )

instance Prelude.Hashable StorageType

instance Prelude.NFData StorageType
