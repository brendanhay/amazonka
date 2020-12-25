{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsEncryptionSettings
  ( HlsEncryptionSettings (..),

    -- * Smart constructor
    mkHlsEncryptionSettings,

    -- * Lenses
    hesConstantInitializationVector,
    hesEncryptionMethod,
    hesInitializationVectorInManifest,
    hesOfflineEncrypted,
    hesSpekeKeyProvider,
    hesStaticKeyProvider,
    hesType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.HlsEncryptionType as Types
import qualified Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest as Types
import qualified Network.AWS.MediaConvert.Types.HlsKeyProviderType as Types
import qualified Network.AWS.MediaConvert.Types.HlsOfflineEncrypted as Types
import qualified Network.AWS.MediaConvert.Types.SpekeKeyProvider as Types
import qualified Network.AWS.MediaConvert.Types.StaticKeyProvider as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for HLS encryption
--
-- /See:/ 'mkHlsEncryptionSettings' smart constructor.
data HlsEncryptionSettings = HlsEncryptionSettings'
  { -- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
    constantInitializationVector :: Core.Maybe Core.Text,
    -- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
    encryptionMethod :: Core.Maybe Types.HlsEncryptionType,
    -- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
    initializationVectorInManifest :: Core.Maybe Types.HlsInitializationVectorInManifest,
    -- | Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
    offlineEncrypted :: Core.Maybe Types.HlsOfflineEncrypted,
    -- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
    spekeKeyProvider :: Core.Maybe Types.SpekeKeyProvider,
    -- | Use these settings to set up encryption with a static key provider.
    staticKeyProvider :: Core.Maybe Types.StaticKeyProvider,
    -- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
    type' :: Core.Maybe Types.HlsKeyProviderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsEncryptionSettings' value with any optional fields omitted.
mkHlsEncryptionSettings ::
  HlsEncryptionSettings
mkHlsEncryptionSettings =
  HlsEncryptionSettings'
    { constantInitializationVector =
        Core.Nothing,
      encryptionMethod = Core.Nothing,
      initializationVectorInManifest = Core.Nothing,
      offlineEncrypted = Core.Nothing,
      spekeKeyProvider = Core.Nothing,
      staticKeyProvider = Core.Nothing,
      type' = Core.Nothing
    }

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
--
-- /Note:/ Consider using 'constantInitializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesConstantInitializationVector :: Lens.Lens' HlsEncryptionSettings (Core.Maybe Core.Text)
hesConstantInitializationVector = Lens.field @"constantInitializationVector"
{-# DEPRECATED hesConstantInitializationVector "Use generic-lens or generic-optics with 'constantInitializationVector' instead." #-}

-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
--
-- /Note:/ Consider using 'encryptionMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesEncryptionMethod :: Lens.Lens' HlsEncryptionSettings (Core.Maybe Types.HlsEncryptionType)
hesEncryptionMethod = Lens.field @"encryptionMethod"
{-# DEPRECATED hesEncryptionMethod "Use generic-lens or generic-optics with 'encryptionMethod' instead." #-}

-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
--
-- /Note:/ Consider using 'initializationVectorInManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesInitializationVectorInManifest :: Lens.Lens' HlsEncryptionSettings (Core.Maybe Types.HlsInitializationVectorInManifest)
hesInitializationVectorInManifest = Lens.field @"initializationVectorInManifest"
{-# DEPRECATED hesInitializationVectorInManifest "Use generic-lens or generic-optics with 'initializationVectorInManifest' instead." #-}

-- | Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
--
-- /Note:/ Consider using 'offlineEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesOfflineEncrypted :: Lens.Lens' HlsEncryptionSettings (Core.Maybe Types.HlsOfflineEncrypted)
hesOfflineEncrypted = Lens.field @"offlineEncrypted"
{-# DEPRECATED hesOfflineEncrypted "Use generic-lens or generic-optics with 'offlineEncrypted' instead." #-}

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesSpekeKeyProvider :: Lens.Lens' HlsEncryptionSettings (Core.Maybe Types.SpekeKeyProvider)
hesSpekeKeyProvider = Lens.field @"spekeKeyProvider"
{-# DEPRECATED hesSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

-- | Use these settings to set up encryption with a static key provider.
--
-- /Note:/ Consider using 'staticKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesStaticKeyProvider :: Lens.Lens' HlsEncryptionSettings (Core.Maybe Types.StaticKeyProvider)
hesStaticKeyProvider = Lens.field @"staticKeyProvider"
{-# DEPRECATED hesStaticKeyProvider "Use generic-lens or generic-optics with 'staticKeyProvider' instead." #-}

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesType :: Lens.Lens' HlsEncryptionSettings (Core.Maybe Types.HlsKeyProviderType)
hesType = Lens.field @"type'"
{-# DEPRECATED hesType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON HlsEncryptionSettings where
  toJSON HlsEncryptionSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("constantInitializationVector" Core..=)
              Core.<$> constantInitializationVector,
            ("encryptionMethod" Core..=) Core.<$> encryptionMethod,
            ("initializationVectorInManifest" Core..=)
              Core.<$> initializationVectorInManifest,
            ("offlineEncrypted" Core..=) Core.<$> offlineEncrypted,
            ("spekeKeyProvider" Core..=) Core.<$> spekeKeyProvider,
            ("staticKeyProvider" Core..=) Core.<$> staticKeyProvider,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON HlsEncryptionSettings where
  parseJSON =
    Core.withObject "HlsEncryptionSettings" Core.$
      \x ->
        HlsEncryptionSettings'
          Core.<$> (x Core..:? "constantInitializationVector")
          Core.<*> (x Core..:? "encryptionMethod")
          Core.<*> (x Core..:? "initializationVectorInManifest")
          Core.<*> (x Core..:? "offlineEncrypted")
          Core.<*> (x Core..:? "spekeKeyProvider")
          Core.<*> (x Core..:? "staticKeyProvider")
          Core.<*> (x Core..:? "type")
