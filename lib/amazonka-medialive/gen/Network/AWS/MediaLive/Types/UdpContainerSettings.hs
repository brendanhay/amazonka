{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpContainerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpContainerSettings
  ( UdpContainerSettings (..),

    -- * Smart constructor
    mkUdpContainerSettings,

    -- * Lenses
    ucsM2tsSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.M2tsSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Udp Container Settings
--
-- /See:/ 'mkUdpContainerSettings' smart constructor.
newtype UdpContainerSettings = UdpContainerSettings'
  { m2tsSettings :: Core.Maybe Types.M2tsSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UdpContainerSettings' value with any optional fields omitted.
mkUdpContainerSettings ::
  UdpContainerSettings
mkUdpContainerSettings =
  UdpContainerSettings' {m2tsSettings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'm2tsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsM2tsSettings :: Lens.Lens' UdpContainerSettings (Core.Maybe Types.M2tsSettings)
ucsM2tsSettings = Lens.field @"m2tsSettings"
{-# DEPRECATED ucsM2tsSettings "Use generic-lens or generic-optics with 'm2tsSettings' instead." #-}

instance Core.FromJSON UdpContainerSettings where
  toJSON UdpContainerSettings {..} =
    Core.object
      (Core.catMaybes [("m2tsSettings" Core..=) Core.<$> m2tsSettings])

instance Core.FromJSON UdpContainerSettings where
  parseJSON =
    Core.withObject "UdpContainerSettings" Core.$
      \x -> UdpContainerSettings' Core.<$> (x Core..:? "m2tsSettings")
