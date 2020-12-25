{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
  ( MsSmoothEncryptionSettings (..),

    -- * Smart constructor
    mkMsSmoothEncryptionSettings,

    -- * Lenses
    msesSpekeKeyProvider,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.SpekeKeyProvider as Types
import qualified Network.AWS.Prelude as Core

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
--
-- /See:/ 'mkMsSmoothEncryptionSettings' smart constructor.
newtype MsSmoothEncryptionSettings = MsSmoothEncryptionSettings'
  { -- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
    spekeKeyProvider :: Core.Maybe Types.SpekeKeyProvider
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MsSmoothEncryptionSettings' value with any optional fields omitted.
mkMsSmoothEncryptionSettings ::
  MsSmoothEncryptionSettings
mkMsSmoothEncryptionSettings =
  MsSmoothEncryptionSettings' {spekeKeyProvider = Core.Nothing}

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msesSpekeKeyProvider :: Lens.Lens' MsSmoothEncryptionSettings (Core.Maybe Types.SpekeKeyProvider)
msesSpekeKeyProvider = Lens.field @"spekeKeyProvider"
{-# DEPRECATED msesSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

instance Core.FromJSON MsSmoothEncryptionSettings where
  toJSON MsSmoothEncryptionSettings {..} =
    Core.object
      ( Core.catMaybes
          [("spekeKeyProvider" Core..=) Core.<$> spekeKeyProvider]
      )

instance Core.FromJSON MsSmoothEncryptionSettings where
  parseJSON =
    Core.withObject "MsSmoothEncryptionSettings" Core.$
      \x ->
        MsSmoothEncryptionSettings'
          Core.<$> (x Core..:? "spekeKeyProvider")
