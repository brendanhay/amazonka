{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.StorageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.StorageType
  ( StorageType (..),

    -- * Smart constructor
    mkStorageType,

    -- * Lenses
    stStorageTypeLimits,
    stStorageSubTypeName,
    stStorageTypeName,
  )
where

import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | StorageTypes represents the list of storage related types and their attributes that are available for given InstanceType.
--
-- /See:/ 'mkStorageType' smart constructor.
data StorageType = StorageType'
  { storageTypeLimits ::
      Lude.Maybe [StorageTypeLimit],
    storageSubTypeName :: Lude.Maybe Lude.Text,
    storageTypeName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StorageType' with the minimum fields required to make a request.
--
-- * 'storageSubTypeName' - Undocumented field.
-- * 'storageTypeLimits' - List of limits that are applicable for given storage type.
-- * 'storageTypeName' - Undocumented field.
mkStorageType ::
  StorageType
mkStorageType =
  StorageType'
    { storageTypeLimits = Lude.Nothing,
      storageSubTypeName = Lude.Nothing,
      storageTypeName = Lude.Nothing
    }

-- | List of limits that are applicable for given storage type.
--
-- /Note:/ Consider using 'storageTypeLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStorageTypeLimits :: Lens.Lens' StorageType (Lude.Maybe [StorageTypeLimit])
stStorageTypeLimits = Lens.lens (storageTypeLimits :: StorageType -> Lude.Maybe [StorageTypeLimit]) (\s a -> s {storageTypeLimits = a} :: StorageType)
{-# DEPRECATED stStorageTypeLimits "Use generic-lens or generic-optics with 'storageTypeLimits' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'storageSubTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStorageSubTypeName :: Lens.Lens' StorageType (Lude.Maybe Lude.Text)
stStorageSubTypeName = Lens.lens (storageSubTypeName :: StorageType -> Lude.Maybe Lude.Text) (\s a -> s {storageSubTypeName = a} :: StorageType)
{-# DEPRECATED stStorageSubTypeName "Use generic-lens or generic-optics with 'storageSubTypeName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'storageTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStorageTypeName :: Lens.Lens' StorageType (Lude.Maybe Lude.Text)
stStorageTypeName = Lens.lens (storageTypeName :: StorageType -> Lude.Maybe Lude.Text) (\s a -> s {storageTypeName = a} :: StorageType)
{-# DEPRECATED stStorageTypeName "Use generic-lens or generic-optics with 'storageTypeName' instead." #-}

instance Lude.FromJSON StorageType where
  parseJSON =
    Lude.withObject
      "StorageType"
      ( \x ->
          StorageType'
            Lude.<$> (x Lude..:? "StorageTypeLimits" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StorageSubTypeName")
            Lude.<*> (x Lude..:? "StorageTypeName")
      )
