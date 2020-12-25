{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubSourceSettings
  ( DvbSubSourceSettings (..),

    -- * Smart constructor
    mkDvbSubSourceSettings,

    -- * Lenses
    dsssPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | DVB Sub Source Settings
--
-- /See:/ 'mkDvbSubSourceSettings' smart constructor.
newtype DvbSubSourceSettings = DvbSubSourceSettings'
  { -- | When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
    pid :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DvbSubSourceSettings' value with any optional fields omitted.
mkDvbSubSourceSettings ::
  DvbSubSourceSettings
mkDvbSubSourceSettings = DvbSubSourceSettings' {pid = Core.Nothing}

-- | When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsssPid :: Lens.Lens' DvbSubSourceSettings (Core.Maybe Core.Natural)
dsssPid = Lens.field @"pid"
{-# DEPRECATED dsssPid "Use generic-lens or generic-optics with 'pid' instead." #-}

instance Core.FromJSON DvbSubSourceSettings where
  toJSON DvbSubSourceSettings {..} =
    Core.object (Core.catMaybes [("pid" Core..=) Core.<$> pid])

instance Core.FromJSON DvbSubSourceSettings where
  parseJSON =
    Core.withObject "DvbSubSourceSettings" Core.$
      \x -> DvbSubSourceSettings' Core.<$> (x Core..:? "pid")
