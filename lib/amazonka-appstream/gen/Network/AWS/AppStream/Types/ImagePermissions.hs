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
import qualified Network.AWS.Prelude as Lude

-- | Describes the permissions for an image.
--
-- /See:/ 'mkImagePermissions' smart constructor.
data ImagePermissions = ImagePermissions'
  { -- | Indicates whether the image can be used for a fleet.
    allowFleet :: Lude.Maybe Lude.Bool,
    -- | Indicates whether the image can be used for an image builder.
    allowImageBuilder :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImagePermissions' with the minimum fields required to make a request.
--
-- * 'allowFleet' - Indicates whether the image can be used for a fleet.
-- * 'allowImageBuilder' - Indicates whether the image can be used for an image builder.
mkImagePermissions ::
  ImagePermissions
mkImagePermissions =
  ImagePermissions'
    { allowFleet = Lude.Nothing,
      allowImageBuilder = Lude.Nothing
    }

-- | Indicates whether the image can be used for a fleet.
--
-- /Note:/ Consider using 'allowFleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipAllowFleet :: Lens.Lens' ImagePermissions (Lude.Maybe Lude.Bool)
ipAllowFleet = Lens.lens (allowFleet :: ImagePermissions -> Lude.Maybe Lude.Bool) (\s a -> s {allowFleet = a} :: ImagePermissions)
{-# DEPRECATED ipAllowFleet "Use generic-lens or generic-optics with 'allowFleet' instead." #-}

-- | Indicates whether the image can be used for an image builder.
--
-- /Note:/ Consider using 'allowImageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipAllowImageBuilder :: Lens.Lens' ImagePermissions (Lude.Maybe Lude.Bool)
ipAllowImageBuilder = Lens.lens (allowImageBuilder :: ImagePermissions -> Lude.Maybe Lude.Bool) (\s a -> s {allowImageBuilder = a} :: ImagePermissions)
{-# DEPRECATED ipAllowImageBuilder "Use generic-lens or generic-optics with 'allowImageBuilder' instead." #-}

instance Lude.FromJSON ImagePermissions where
  parseJSON =
    Lude.withObject
      "ImagePermissions"
      ( \x ->
          ImagePermissions'
            Lude.<$> (x Lude..:? "allowFleet")
            Lude.<*> (x Lude..:? "allowImageBuilder")
      )

instance Lude.ToJSON ImagePermissions where
  toJSON ImagePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("allowFleet" Lude..=) Lude.<$> allowFleet,
            ("allowImageBuilder" Lude..=) Lude.<$> allowImageBuilder
          ]
      )
