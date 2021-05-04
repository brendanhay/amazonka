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
-- Module      : Network.AWS.WorkDocs.Types.StorageRuleType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.StorageRuleType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.StorageType

-- | Describes the storage for a user.
--
-- /See:/ 'newStorageRuleType' smart constructor.
data StorageRuleType = StorageRuleType'
  { -- | The type of storage.
    storageType :: Prelude.Maybe StorageType,
    -- | The amount of storage allocated, in bytes.
    storageAllocatedInBytes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { storageType = Prelude.Nothing,
      storageAllocatedInBytes = Prelude.Nothing
    }

-- | The type of storage.
storageRuleType_storageType :: Lens.Lens' StorageRuleType (Prelude.Maybe StorageType)
storageRuleType_storageType = Lens.lens (\StorageRuleType' {storageType} -> storageType) (\s@StorageRuleType' {} a -> s {storageType = a} :: StorageRuleType)

-- | The amount of storage allocated, in bytes.
storageRuleType_storageAllocatedInBytes :: Lens.Lens' StorageRuleType (Prelude.Maybe Prelude.Natural)
storageRuleType_storageAllocatedInBytes = Lens.lens (\StorageRuleType' {storageAllocatedInBytes} -> storageAllocatedInBytes) (\s@StorageRuleType' {} a -> s {storageAllocatedInBytes = a} :: StorageRuleType)

instance Prelude.FromJSON StorageRuleType where
  parseJSON =
    Prelude.withObject
      "StorageRuleType"
      ( \x ->
          StorageRuleType'
            Prelude.<$> (x Prelude..:? "StorageType")
            Prelude.<*> (x Prelude..:? "StorageAllocatedInBytes")
      )

instance Prelude.Hashable StorageRuleType

instance Prelude.NFData StorageRuleType

instance Prelude.ToJSON StorageRuleType where
  toJSON StorageRuleType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StorageType" Prelude..=) Prelude.<$> storageType,
            ("StorageAllocatedInBytes" Prelude..=)
              Prelude.<$> storageAllocatedInBytes
          ]
      )
