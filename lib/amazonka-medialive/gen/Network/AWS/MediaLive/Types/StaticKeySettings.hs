{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticKeySettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticKeySettings
  ( StaticKeySettings (..),

    -- * Smart constructor
    mkStaticKeySettings,

    -- * Lenses
    sksStaticKeyValue,
    sksKeyProviderServer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputLocation as Types
import qualified Network.AWS.Prelude as Core

-- | Static Key Settings
--
-- /See:/ 'mkStaticKeySettings' smart constructor.
data StaticKeySettings = StaticKeySettings'
  { -- | Static key value as a 32 character hexadecimal string.
    staticKeyValue :: Core.Text,
    -- | The URL of the license server used for protecting content.
    keyProviderServer :: Core.Maybe Types.InputLocation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StaticKeySettings' value with any optional fields omitted.
mkStaticKeySettings ::
  -- | 'staticKeyValue'
  Core.Text ->
  StaticKeySettings
mkStaticKeySettings staticKeyValue =
  StaticKeySettings'
    { staticKeyValue,
      keyProviderServer = Core.Nothing
    }

-- | Static key value as a 32 character hexadecimal string.
--
-- /Note:/ Consider using 'staticKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sksStaticKeyValue :: Lens.Lens' StaticKeySettings Core.Text
sksStaticKeyValue = Lens.field @"staticKeyValue"
{-# DEPRECATED sksStaticKeyValue "Use generic-lens or generic-optics with 'staticKeyValue' instead." #-}

-- | The URL of the license server used for protecting content.
--
-- /Note:/ Consider using 'keyProviderServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sksKeyProviderServer :: Lens.Lens' StaticKeySettings (Core.Maybe Types.InputLocation)
sksKeyProviderServer = Lens.field @"keyProviderServer"
{-# DEPRECATED sksKeyProviderServer "Use generic-lens or generic-optics with 'keyProviderServer' instead." #-}

instance Core.FromJSON StaticKeySettings where
  toJSON StaticKeySettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("staticKeyValue" Core..= staticKeyValue),
            ("keyProviderServer" Core..=) Core.<$> keyProviderServer
          ]
      )

instance Core.FromJSON StaticKeySettings where
  parseJSON =
    Core.withObject "StaticKeySettings" Core.$
      \x ->
        StaticKeySettings'
          Core.<$> (x Core..: "staticKeyValue")
          Core.<*> (x Core..:? "keyProviderServer")
