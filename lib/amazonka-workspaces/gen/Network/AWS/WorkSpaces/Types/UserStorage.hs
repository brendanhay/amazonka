{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.UserStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.UserStorage
  ( UserStorage (..)
  -- * Smart constructor
  , mkUserStorage
  -- * Lenses
  , usCapacity
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.NonEmptyString as Types

-- | Describes the user storage for a WorkSpace bundle.
--
-- /See:/ 'mkUserStorage' smart constructor.
newtype UserStorage = UserStorage'
  { capacity :: Core.Maybe Types.NonEmptyString
    -- ^ The size of the user storage.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UserStorage' value with any optional fields omitted.
mkUserStorage
    :: UserStorage
mkUserStorage = UserStorage'{capacity = Core.Nothing}

-- | The size of the user storage.
--
-- /Note:/ Consider using 'capacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCapacity :: Lens.Lens' UserStorage (Core.Maybe Types.NonEmptyString)
usCapacity = Lens.field @"capacity"
{-# INLINEABLE usCapacity #-}
{-# DEPRECATED capacity "Use generic-lens or generic-optics with 'capacity' instead"  #-}

instance Core.FromJSON UserStorage where
        parseJSON
          = Core.withObject "UserStorage" Core.$
              \ x -> UserStorage' Core.<$> (x Core..:? "Capacity")
