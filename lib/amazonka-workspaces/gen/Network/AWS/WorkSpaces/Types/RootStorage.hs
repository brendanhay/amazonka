{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RootStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.RootStorage
  ( RootStorage (..)
  -- * Smart constructor
  , mkRootStorage
  -- * Lenses
  , rsCapacity
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.NonEmptyString as Types

-- | Describes the root volume for a WorkSpace bundle.
--
-- /See:/ 'mkRootStorage' smart constructor.
newtype RootStorage = RootStorage'
  { capacity :: Core.Maybe Types.NonEmptyString
    -- ^ The size of the root volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RootStorage' value with any optional fields omitted.
mkRootStorage
    :: RootStorage
mkRootStorage = RootStorage'{capacity = Core.Nothing}

-- | The size of the root volume.
--
-- /Note:/ Consider using 'capacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsCapacity :: Lens.Lens' RootStorage (Core.Maybe Types.NonEmptyString)
rsCapacity = Lens.field @"capacity"
{-# INLINEABLE rsCapacity #-}
{-# DEPRECATED capacity "Use generic-lens or generic-optics with 'capacity' instead"  #-}

instance Core.FromJSON RootStorage where
        parseJSON
          = Core.withObject "RootStorage" Core.$
              \ x -> RootStorage' Core.<$> (x Core..:? "Capacity")
