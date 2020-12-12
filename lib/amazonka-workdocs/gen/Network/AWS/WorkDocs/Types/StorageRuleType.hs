{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.StorageRuleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.StorageRuleType
  ( StorageRuleType (..),

    -- * Smart constructor
    mkStorageRuleType,

    -- * Lenses
    srtStorageAllocatedInBytes,
    srtStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.StorageType

-- | Describes the storage for a user.
--
-- /See:/ 'mkStorageRuleType' smart constructor.
data StorageRuleType = StorageRuleType'
  { storageAllocatedInBytes ::
      Lude.Maybe Lude.Natural,
    storageType :: Lude.Maybe StorageType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StorageRuleType' with the minimum fields required to make a request.
--
-- * 'storageAllocatedInBytes' - The amount of storage allocated, in bytes.
-- * 'storageType' - The type of storage.
mkStorageRuleType ::
  StorageRuleType
mkStorageRuleType =
  StorageRuleType'
    { storageAllocatedInBytes = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | The amount of storage allocated, in bytes.
--
-- /Note:/ Consider using 'storageAllocatedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtStorageAllocatedInBytes :: Lens.Lens' StorageRuleType (Lude.Maybe Lude.Natural)
srtStorageAllocatedInBytes = Lens.lens (storageAllocatedInBytes :: StorageRuleType -> Lude.Maybe Lude.Natural) (\s a -> s {storageAllocatedInBytes = a} :: StorageRuleType)
{-# DEPRECATED srtStorageAllocatedInBytes "Use generic-lens or generic-optics with 'storageAllocatedInBytes' instead." #-}

-- | The type of storage.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtStorageType :: Lens.Lens' StorageRuleType (Lude.Maybe StorageType)
srtStorageType = Lens.lens (storageType :: StorageRuleType -> Lude.Maybe StorageType) (\s a -> s {storageType = a} :: StorageRuleType)
{-# DEPRECATED srtStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.FromJSON StorageRuleType where
  parseJSON =
    Lude.withObject
      "StorageRuleType"
      ( \x ->
          StorageRuleType'
            Lude.<$> (x Lude..:? "StorageAllocatedInBytes")
            Lude.<*> (x Lude..:? "StorageType")
      )

instance Lude.ToJSON StorageRuleType where
  toJSON StorageRuleType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StorageAllocatedInBytes" Lude..=)
              Lude.<$> storageAllocatedInBytes,
            ("StorageType" Lude..=) Lude.<$> storageType
          ]
      )
