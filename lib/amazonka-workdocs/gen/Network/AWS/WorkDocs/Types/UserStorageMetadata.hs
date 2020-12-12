{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserStorageMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserStorageMetadata
  ( UserStorageMetadata (..),

    -- * Smart constructor
    mkUserStorageMetadata,

    -- * Lenses
    usmStorageUtilizedInBytes,
    usmStorageRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.StorageRuleType

-- | Describes the storage for a user.
--
-- /See:/ 'mkUserStorageMetadata' smart constructor.
data UserStorageMetadata = UserStorageMetadata'
  { storageUtilizedInBytes ::
      Lude.Maybe Lude.Integer,
    storageRule :: Lude.Maybe StorageRuleType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserStorageMetadata' with the minimum fields required to make a request.
--
-- * 'storageRule' - The storage for a user.
-- * 'storageUtilizedInBytes' - The amount of storage used, in bytes.
mkUserStorageMetadata ::
  UserStorageMetadata
mkUserStorageMetadata =
  UserStorageMetadata'
    { storageUtilizedInBytes = Lude.Nothing,
      storageRule = Lude.Nothing
    }

-- | The amount of storage used, in bytes.
--
-- /Note:/ Consider using 'storageUtilizedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmStorageUtilizedInBytes :: Lens.Lens' UserStorageMetadata (Lude.Maybe Lude.Integer)
usmStorageUtilizedInBytes = Lens.lens (storageUtilizedInBytes :: UserStorageMetadata -> Lude.Maybe Lude.Integer) (\s a -> s {storageUtilizedInBytes = a} :: UserStorageMetadata)
{-# DEPRECATED usmStorageUtilizedInBytes "Use generic-lens or generic-optics with 'storageUtilizedInBytes' instead." #-}

-- | The storage for a user.
--
-- /Note:/ Consider using 'storageRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmStorageRule :: Lens.Lens' UserStorageMetadata (Lude.Maybe StorageRuleType)
usmStorageRule = Lens.lens (storageRule :: UserStorageMetadata -> Lude.Maybe StorageRuleType) (\s a -> s {storageRule = a} :: UserStorageMetadata)
{-# DEPRECATED usmStorageRule "Use generic-lens or generic-optics with 'storageRule' instead." #-}

instance Lude.FromJSON UserStorageMetadata where
  parseJSON =
    Lude.withObject
      "UserStorageMetadata"
      ( \x ->
          UserStorageMetadata'
            Lude.<$> (x Lude..:? "StorageUtilizedInBytes")
            Lude.<*> (x Lude..:? "StorageRule")
      )
