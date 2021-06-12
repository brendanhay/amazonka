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
-- Module      : Network.AWS.WorkDocs.Types.StorageRuleType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.StorageRuleType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.StorageType

-- | Describes the storage for a user.
--
-- /See:/ 'newStorageRuleType' smart constructor.
data StorageRuleType = StorageRuleType'
  { -- | The type of storage.
    storageType :: Core.Maybe StorageType,
    -- | The amount of storage allocated, in bytes.
    storageAllocatedInBytes :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StorageRuleType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageType', 'storageRuleType_storageType' - The type of storage.
--
-- 'storageAllocatedInBytes', 'storageRuleType_storageAllocatedInBytes' - The amount of storage allocated, in bytes.
newStorageRuleType ::
  StorageRuleType
newStorageRuleType =
  StorageRuleType'
    { storageType = Core.Nothing,
      storageAllocatedInBytes = Core.Nothing
    }

-- | The type of storage.
storageRuleType_storageType :: Lens.Lens' StorageRuleType (Core.Maybe StorageType)
storageRuleType_storageType = Lens.lens (\StorageRuleType' {storageType} -> storageType) (\s@StorageRuleType' {} a -> s {storageType = a} :: StorageRuleType)

-- | The amount of storage allocated, in bytes.
storageRuleType_storageAllocatedInBytes :: Lens.Lens' StorageRuleType (Core.Maybe Core.Natural)
storageRuleType_storageAllocatedInBytes = Lens.lens (\StorageRuleType' {storageAllocatedInBytes} -> storageAllocatedInBytes) (\s@StorageRuleType' {} a -> s {storageAllocatedInBytes = a} :: StorageRuleType)

instance Core.FromJSON StorageRuleType where
  parseJSON =
    Core.withObject
      "StorageRuleType"
      ( \x ->
          StorageRuleType'
            Core.<$> (x Core..:? "StorageType")
            Core.<*> (x Core..:? "StorageAllocatedInBytes")
      )

instance Core.Hashable StorageRuleType

instance Core.NFData StorageRuleType

instance Core.ToJSON StorageRuleType where
  toJSON StorageRuleType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StorageType" Core..=) Core.<$> storageType,
            ("StorageAllocatedInBytes" Core..=)
              Core.<$> storageAllocatedInBytes
          ]
      )
