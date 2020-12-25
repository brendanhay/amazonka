{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CdiInputSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CdiInputSpecification
  ( CdiInputSpecification (..),

    -- * Smart constructor
    mkCdiInputSpecification,

    -- * Lenses
    cisResolution,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.CdiInputResolution as Types
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for CdiInputSpecification
--
-- /See:/ 'mkCdiInputSpecification' smart constructor.
newtype CdiInputSpecification = CdiInputSpecification'
  { -- | Maximum CDI input resolution
    resolution :: Core.Maybe Types.CdiInputResolution
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CdiInputSpecification' value with any optional fields omitted.
mkCdiInputSpecification ::
  CdiInputSpecification
mkCdiInputSpecification =
  CdiInputSpecification' {resolution = Core.Nothing}

-- | Maximum CDI input resolution
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisResolution :: Lens.Lens' CdiInputSpecification (Core.Maybe Types.CdiInputResolution)
cisResolution = Lens.field @"resolution"
{-# DEPRECATED cisResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

instance Core.FromJSON CdiInputSpecification where
  toJSON CdiInputSpecification {..} =
    Core.object
      (Core.catMaybes [("resolution" Core..=) Core.<$> resolution])

instance Core.FromJSON CdiInputSpecification where
  parseJSON =
    Core.withObject "CdiInputSpecification" Core.$
      \x -> CdiInputSpecification' Core.<$> (x Core..:? "resolution")
