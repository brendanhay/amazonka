{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.KeyProviderSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.KeyProviderSettings
  ( KeyProviderSettings (..),

    -- * Smart constructor
    mkKeyProviderSettings,

    -- * Lenses
    kpsStaticKeySettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.StaticKeySettings as Types
import qualified Network.AWS.Prelude as Core

-- | Key Provider Settings
--
-- /See:/ 'mkKeyProviderSettings' smart constructor.
newtype KeyProviderSettings = KeyProviderSettings'
  { staticKeySettings :: Core.Maybe Types.StaticKeySettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'KeyProviderSettings' value with any optional fields omitted.
mkKeyProviderSettings ::
  KeyProviderSettings
mkKeyProviderSettings =
  KeyProviderSettings' {staticKeySettings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'staticKeySettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpsStaticKeySettings :: Lens.Lens' KeyProviderSettings (Core.Maybe Types.StaticKeySettings)
kpsStaticKeySettings = Lens.field @"staticKeySettings"
{-# DEPRECATED kpsStaticKeySettings "Use generic-lens or generic-optics with 'staticKeySettings' instead." #-}

instance Core.FromJSON KeyProviderSettings where
  toJSON KeyProviderSettings {..} =
    Core.object
      ( Core.catMaybes
          [("staticKeySettings" Core..=) Core.<$> staticKeySettings]
      )

instance Core.FromJSON KeyProviderSettings where
  parseJSON =
    Core.withObject "KeyProviderSettings" Core.$
      \x ->
        KeyProviderSettings' Core.<$> (x Core..:? "staticKeySettings")
