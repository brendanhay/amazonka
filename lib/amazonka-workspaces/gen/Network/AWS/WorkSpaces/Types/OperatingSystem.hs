{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.OperatingSystem
  ( OperatingSystem (..)
  -- * Smart constructor
  , mkOperatingSystem
  -- * Lenses
  , osType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.OperatingSystemType as Types

-- | The operating system that the image is running.
--
-- /See:/ 'mkOperatingSystem' smart constructor.
newtype OperatingSystem = OperatingSystem'
  { type' :: Core.Maybe Types.OperatingSystemType
    -- ^ The operating system.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OperatingSystem' value with any optional fields omitted.
mkOperatingSystem
    :: OperatingSystem
mkOperatingSystem = OperatingSystem'{type' = Core.Nothing}

-- | The operating system.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OperatingSystem (Core.Maybe Types.OperatingSystemType)
osType = Lens.field @"type'"
{-# INLINEABLE osType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON OperatingSystem where
        parseJSON
          = Core.withObject "OperatingSystem" Core.$
              \ x -> OperatingSystem' Core.<$> (x Core..:? "Type")
