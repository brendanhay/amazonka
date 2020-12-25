{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImagePermissions
  ( ImagePermissions (..),

    -- * Smart constructor
    mkImagePermissions,

    -- * Lenses
    ipAllowFleet,
    ipAllowImageBuilder,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the permissions for an image.
--
-- /See:/ 'mkImagePermissions' smart constructor.
data ImagePermissions = ImagePermissions'
  { -- | Indicates whether the image can be used for a fleet.
    allowFleet :: Core.Maybe Core.Bool,
    -- | Indicates whether the image can be used for an image builder.
    allowImageBuilder :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImagePermissions' value with any optional fields omitted.
mkImagePermissions ::
  ImagePermissions
mkImagePermissions =
  ImagePermissions'
    { allowFleet = Core.Nothing,
      allowImageBuilder = Core.Nothing
    }

-- | Indicates whether the image can be used for a fleet.
--
-- /Note:/ Consider using 'allowFleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipAllowFleet :: Lens.Lens' ImagePermissions (Core.Maybe Core.Bool)
ipAllowFleet = Lens.field @"allowFleet"
{-# DEPRECATED ipAllowFleet "Use generic-lens or generic-optics with 'allowFleet' instead." #-}

-- | Indicates whether the image can be used for an image builder.
--
-- /Note:/ Consider using 'allowImageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipAllowImageBuilder :: Lens.Lens' ImagePermissions (Core.Maybe Core.Bool)
ipAllowImageBuilder = Lens.field @"allowImageBuilder"
{-# DEPRECATED ipAllowImageBuilder "Use generic-lens or generic-optics with 'allowImageBuilder' instead." #-}

instance Core.FromJSON ImagePermissions where
  toJSON ImagePermissions {..} =
    Core.object
      ( Core.catMaybes
          [ ("allowFleet" Core..=) Core.<$> allowFleet,
            ("allowImageBuilder" Core..=) Core.<$> allowImageBuilder
          ]
      )

instance Core.FromJSON ImagePermissions where
  parseJSON =
    Core.withObject "ImagePermissions" Core.$
      \x ->
        ImagePermissions'
          Core.<$> (x Core..:? "allowFleet") Core.<*> (x Core..:? "allowImageBuilder")
