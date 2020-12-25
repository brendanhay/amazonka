{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbTdtSettings
  ( DvbTdtSettings (..),

    -- * Smart constructor
    mkDvbTdtSettings,

    -- * Lenses
    dtsRepInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | DVB Time and Date Table (SDT)
--
-- /See:/ 'mkDvbTdtSettings' smart constructor.
newtype DvbTdtSettings = DvbTdtSettings'
  { -- | The number of milliseconds between instances of this table in the output transport stream.
    repInterval :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DvbTdtSettings' value with any optional fields omitted.
mkDvbTdtSettings ::
  DvbTdtSettings
mkDvbTdtSettings = DvbTdtSettings' {repInterval = Core.Nothing}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'repInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsRepInterval :: Lens.Lens' DvbTdtSettings (Core.Maybe Core.Natural)
dtsRepInterval = Lens.field @"repInterval"
{-# DEPRECATED dtsRepInterval "Use generic-lens or generic-optics with 'repInterval' instead." #-}

instance Core.FromJSON DvbTdtSettings where
  toJSON DvbTdtSettings {..} =
    Core.object
      (Core.catMaybes [("repInterval" Core..=) Core.<$> repInterval])

instance Core.FromJSON DvbTdtSettings where
  parseJSON =
    Core.withObject "DvbTdtSettings" Core.$
      \x -> DvbTdtSettings' Core.<$> (x Core..:? "repInterval")
