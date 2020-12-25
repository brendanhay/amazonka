{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ApplicationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ApplicationSettings
  ( ApplicationSettings (..),

    -- * Smart constructor
    mkApplicationSettings,

    -- * Lenses
    asEnabled,
    asSettingsGroup,
  )
where

import qualified Network.AWS.AppStream.Types.SettingsGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The persistent application settings for users of a stack.
--
-- /See:/ 'mkApplicationSettings' smart constructor.
data ApplicationSettings = ApplicationSettings'
  { -- | Enables or disables persistent application settings for users during their streaming sessions.
    enabled :: Core.Bool,
    -- | The path prefix for the S3 bucket where users’ persistent application settings are stored. You can allow the same persistent application settings to be used across multiple stacks by specifying the same settings group for each stack.
    settingsGroup :: Core.Maybe Types.SettingsGroup
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationSettings' value with any optional fields omitted.
mkApplicationSettings ::
  -- | 'enabled'
  Core.Bool ->
  ApplicationSettings
mkApplicationSettings enabled =
  ApplicationSettings' {enabled, settingsGroup = Core.Nothing}

-- | Enables or disables persistent application settings for users during their streaming sessions.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEnabled :: Lens.Lens' ApplicationSettings Core.Bool
asEnabled = Lens.field @"enabled"
{-# DEPRECATED asEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The path prefix for the S3 bucket where users’ persistent application settings are stored. You can allow the same persistent application settings to be used across multiple stacks by specifying the same settings group for each stack.
--
-- /Note:/ Consider using 'settingsGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSettingsGroup :: Lens.Lens' ApplicationSettings (Core.Maybe Types.SettingsGroup)
asSettingsGroup = Lens.field @"settingsGroup"
{-# DEPRECATED asSettingsGroup "Use generic-lens or generic-optics with 'settingsGroup' instead." #-}

instance Core.FromJSON ApplicationSettings where
  toJSON ApplicationSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Enabled" Core..= enabled),
            ("SettingsGroup" Core..=) Core.<$> settingsGroup
          ]
      )
