{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailConfiguration
  ( AvailConfiguration (..),

    -- * Smart constructor
    mkAvailConfiguration,

    -- * Lenses
    acAvailSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AvailSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Avail Configuration
--
-- /See:/ 'mkAvailConfiguration' smart constructor.
newtype AvailConfiguration = AvailConfiguration'
  { -- | Ad avail settings.
    availSettings :: Core.Maybe Types.AvailSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AvailConfiguration' value with any optional fields omitted.
mkAvailConfiguration ::
  AvailConfiguration
mkAvailConfiguration =
  AvailConfiguration' {availSettings = Core.Nothing}

-- | Ad avail settings.
--
-- /Note:/ Consider using 'availSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAvailSettings :: Lens.Lens' AvailConfiguration (Core.Maybe Types.AvailSettings)
acAvailSettings = Lens.field @"availSettings"
{-# DEPRECATED acAvailSettings "Use generic-lens or generic-optics with 'availSettings' instead." #-}

instance Core.FromJSON AvailConfiguration where
  toJSON AvailConfiguration {..} =
    Core.object
      (Core.catMaybes [("availSettings" Core..=) Core.<$> availSettings])

instance Core.FromJSON AvailConfiguration where
  parseJSON =
    Core.withObject "AvailConfiguration" Core.$
      \x -> AvailConfiguration' Core.<$> (x Core..:? "availSettings")
