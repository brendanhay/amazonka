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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import qualified Network.AWS.Lens as Lens

-- | StorageTypes represents the list of storage related types and their
-- attributes that are available for given InstanceType.
--
-- /See:/ 'newStorageType' smart constructor.
data StorageType = StorageType'
  { -- | List of limits that are applicable for given storage type.
    storageTypeLimits :: Core.Maybe [StorageTypeLimit],
    storageTypeName :: Core.Maybe Core.Text,
    storageSubTypeName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { storageTypeLimits = Core.Nothing,
      storageTypeName = Core.Nothing,
      storageSubTypeName = Core.Nothing
    }

-- | List of limits that are applicable for given storage type.
storageType_storageTypeLimits :: Lens.Lens' StorageType (Core.Maybe [StorageTypeLimit])
storageType_storageTypeLimits = Lens.lens (\StorageType' {storageTypeLimits} -> storageTypeLimits) (\s@StorageType' {} a -> s {storageTypeLimits = a} :: StorageType) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
storageType_storageTypeName :: Lens.Lens' StorageType (Core.Maybe Core.Text)
storageType_storageTypeName = Lens.lens (\StorageType' {storageTypeName} -> storageTypeName) (\s@StorageType' {} a -> s {storageTypeName = a} :: StorageType)

-- | Undocumented member.
storageType_storageSubTypeName :: Lens.Lens' StorageType (Core.Maybe Core.Text)
storageType_storageSubTypeName = Lens.lens (\StorageType' {storageSubTypeName} -> storageSubTypeName) (\s@StorageType' {} a -> s {storageSubTypeName = a} :: StorageType)

instance Core.FromJSON StorageType where
  parseJSON =
    Core.withObject
      "StorageType"
      ( \x ->
          StorageType'
            Core.<$> (x Core..:? "StorageTypeLimits" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StorageTypeName")
            Core.<*> (x Core..:? "StorageSubTypeName")
      )

instance Core.Hashable StorageType

instance Core.NFData StorageType
