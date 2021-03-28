{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserStorageMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.UserStorageMetadata
  ( UserStorageMetadata (..)
  -- * Smart constructor
  , mkUserStorageMetadata
  -- * Lenses
  , usmStorageRule
  , usmStorageUtilizedInBytes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.StorageRuleType as Types

-- | Describes the storage for a user.
--
-- /See:/ 'mkUserStorageMetadata' smart constructor.
data UserStorageMetadata = UserStorageMetadata'
  { storageRule :: Core.Maybe Types.StorageRuleType
    -- ^ The storage for a user.
  , storageUtilizedInBytes :: Core.Maybe Core.Integer
    -- ^ The amount of storage used, in bytes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserStorageMetadata' value with any optional fields omitted.
mkUserStorageMetadata
    :: UserStorageMetadata
mkUserStorageMetadata
  = UserStorageMetadata'{storageRule = Core.Nothing,
                         storageUtilizedInBytes = Core.Nothing}

-- | The storage for a user.
--
-- /Note:/ Consider using 'storageRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmStorageRule :: Lens.Lens' UserStorageMetadata (Core.Maybe Types.StorageRuleType)
usmStorageRule = Lens.field @"storageRule"
{-# INLINEABLE usmStorageRule #-}
{-# DEPRECATED storageRule "Use generic-lens or generic-optics with 'storageRule' instead"  #-}

-- | The amount of storage used, in bytes.
--
-- /Note:/ Consider using 'storageUtilizedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmStorageUtilizedInBytes :: Lens.Lens' UserStorageMetadata (Core.Maybe Core.Integer)
usmStorageUtilizedInBytes = Lens.field @"storageUtilizedInBytes"
{-# INLINEABLE usmStorageUtilizedInBytes #-}
{-# DEPRECATED storageUtilizedInBytes "Use generic-lens or generic-optics with 'storageUtilizedInBytes' instead"  #-}

instance Core.FromJSON UserStorageMetadata where
        parseJSON
          = Core.withObject "UserStorageMetadata" Core.$
              \ x ->
                UserStorageMetadata' Core.<$>
                  (x Core..:? "StorageRule") Core.<*>
                    x Core..:? "StorageUtilizedInBytes"
