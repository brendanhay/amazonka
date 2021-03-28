{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MediaPackageGroupSettings
  ( MediaPackageGroupSettings (..)
  -- * Smart constructor
  , mkMediaPackageGroupSettings
  -- * Lenses
  , mpgsDestination
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.OutputLocationRef as Types
import qualified Network.AWS.Prelude as Core

-- | Media Package Group Settings
--
-- /See:/ 'mkMediaPackageGroupSettings' smart constructor.
newtype MediaPackageGroupSettings = MediaPackageGroupSettings'
  { destination :: Types.OutputLocationRef
    -- ^ MediaPackage channel destination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MediaPackageGroupSettings' value with any optional fields omitted.
mkMediaPackageGroupSettings
    :: Types.OutputLocationRef -- ^ 'destination'
    -> MediaPackageGroupSettings
mkMediaPackageGroupSettings destination
  = MediaPackageGroupSettings'{destination}

-- | MediaPackage channel destination.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpgsDestination :: Lens.Lens' MediaPackageGroupSettings Types.OutputLocationRef
mpgsDestination = Lens.field @"destination"
{-# INLINEABLE mpgsDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

instance Core.FromJSON MediaPackageGroupSettings where
        toJSON MediaPackageGroupSettings{..}
          = Core.object
              (Core.catMaybes [Core.Just ("destination" Core..= destination)])

instance Core.FromJSON MediaPackageGroupSettings where
        parseJSON
          = Core.withObject "MediaPackageGroupSettings" Core.$
              \ x ->
                MediaPackageGroupSettings' Core.<$> (x Core..: "destination")
