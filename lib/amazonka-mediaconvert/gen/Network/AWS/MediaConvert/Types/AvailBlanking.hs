{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvailBlanking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvailBlanking
  ( AvailBlanking (..),

    -- * Smart constructor
    mkAvailBlanking,

    -- * Lenses
    abAvailBlankingImage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for Avail Blanking
--
-- /See:/ 'mkAvailBlanking' smart constructor.
newtype AvailBlanking = AvailBlanking'
  { -- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
    availBlankingImage :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AvailBlanking' value with any optional fields omitted.
mkAvailBlanking ::
  AvailBlanking
mkAvailBlanking = AvailBlanking' {availBlankingImage = Core.Nothing}

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
--
-- /Note:/ Consider using 'availBlankingImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abAvailBlankingImage :: Lens.Lens' AvailBlanking (Core.Maybe Core.Text)
abAvailBlankingImage = Lens.field @"availBlankingImage"
{-# DEPRECATED abAvailBlankingImage "Use generic-lens or generic-optics with 'availBlankingImage' instead." #-}

instance Core.FromJSON AvailBlanking where
  toJSON AvailBlanking {..} =
    Core.object
      ( Core.catMaybes
          [("availBlankingImage" Core..=) Core.<$> availBlankingImage]
      )

instance Core.FromJSON AvailBlanking where
  parseJSON =
    Core.withObject "AvailBlanking" Core.$
      \x -> AvailBlanking' Core.<$> (x Core..:? "availBlankingImage")
