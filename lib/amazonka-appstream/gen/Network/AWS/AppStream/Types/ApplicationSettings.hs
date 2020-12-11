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
    aSettingsGroup,
    aEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The persistent application settings for users of a stack.
--
-- /See:/ 'mkApplicationSettings' smart constructor.
data ApplicationSettings = ApplicationSettings'
  { settingsGroup ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationSettings' with the minimum fields required to make a request.
--
-- * 'enabled' - Enables or disables persistent application settings for users during their streaming sessions.
-- * 'settingsGroup' - The path prefix for the S3 bucket where users’ persistent application settings are stored. You can allow the same persistent application settings to be used across multiple stacks by specifying the same settings group for each stack.
mkApplicationSettings ::
  -- | 'enabled'
  Lude.Bool ->
  ApplicationSettings
mkApplicationSettings pEnabled_ =
  ApplicationSettings'
    { settingsGroup = Lude.Nothing,
      enabled = pEnabled_
    }

-- | The path prefix for the S3 bucket where users’ persistent application settings are stored. You can allow the same persistent application settings to be used across multiple stacks by specifying the same settings group for each stack.
--
-- /Note:/ Consider using 'settingsGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSettingsGroup :: Lens.Lens' ApplicationSettings (Lude.Maybe Lude.Text)
aSettingsGroup = Lens.lens (settingsGroup :: ApplicationSettings -> Lude.Maybe Lude.Text) (\s a -> s {settingsGroup = a} :: ApplicationSettings)
{-# DEPRECATED aSettingsGroup "Use generic-lens or generic-optics with 'settingsGroup' instead." #-}

-- | Enables or disables persistent application settings for users during their streaming sessions.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnabled :: Lens.Lens' ApplicationSettings Lude.Bool
aEnabled = Lens.lens (enabled :: ApplicationSettings -> Lude.Bool) (\s a -> s {enabled = a} :: ApplicationSettings)
{-# DEPRECATED aEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToJSON ApplicationSettings where
  toJSON ApplicationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SettingsGroup" Lude..=) Lude.<$> settingsGroup,
            Lude.Just ("Enabled" Lude..= enabled)
          ]
      )
