{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafEncryptionSettings
  ( CmafEncryptionSettings (..),

    -- * Smart constructor
    mkCmafEncryptionSettings,

    -- * Lenses
    cesConstantInitializationVector,
    cesEncryptionMethod,
    cesInitializationVectorInManifest,
    cesSpekeKeyProvider,
    cesStaticKeyProvider,
    cesType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CmafEncryptionType as Types
import qualified Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest as Types
import qualified Network.AWS.MediaConvert.Types.CmafKeyProviderType as Types
import qualified Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf as Types
import qualified Network.AWS.MediaConvert.Types.StaticKeyProvider as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for CMAF encryption
--
-- /See:/ 'mkCmafEncryptionSettings' smart constructor.
data CmafEncryptionSettings = CmafEncryptionSettings'
  { -- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
    constantInitializationVector :: Core.Maybe Core.Text,
    -- | Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
    encryptionMethod :: Core.Maybe Types.CmafEncryptionType,
    -- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
    initializationVectorInManifest :: Core.Maybe Types.CmafInitializationVectorInManifest,
    -- | If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
    spekeKeyProvider :: Core.Maybe Types.SpekeKeyProviderCmaf,
    -- | Use these settings to set up encryption with a static key provider.
    staticKeyProvider :: Core.Maybe Types.StaticKeyProvider,
    -- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
    type' :: Core.Maybe Types.CmafKeyProviderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CmafEncryptionSettings' value with any optional fields omitted.
mkCmafEncryptionSettings ::
  CmafEncryptionSettings
mkCmafEncryptionSettings =
  CmafEncryptionSettings'
    { constantInitializationVector =
        Core.Nothing,
      encryptionMethod = Core.Nothing,
      initializationVectorInManifest = Core.Nothing,
      spekeKeyProvider = Core.Nothing,
      staticKeyProvider = Core.Nothing,
      type' = Core.Nothing
    }

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
--
-- /Note:/ Consider using 'constantInitializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesConstantInitializationVector :: Lens.Lens' CmafEncryptionSettings (Core.Maybe Core.Text)
cesConstantInitializationVector = Lens.field @"constantInitializationVector"
{-# DEPRECATED cesConstantInitializationVector "Use generic-lens or generic-optics with 'constantInitializationVector' instead." #-}

-- | Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
--
-- /Note:/ Consider using 'encryptionMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesEncryptionMethod :: Lens.Lens' CmafEncryptionSettings (Core.Maybe Types.CmafEncryptionType)
cesEncryptionMethod = Lens.field @"encryptionMethod"
{-# DEPRECATED cesEncryptionMethod "Use generic-lens or generic-optics with 'encryptionMethod' instead." #-}

-- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
--
-- /Note:/ Consider using 'initializationVectorInManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesInitializationVectorInManifest :: Lens.Lens' CmafEncryptionSettings (Core.Maybe Types.CmafInitializationVectorInManifest)
cesInitializationVectorInManifest = Lens.field @"initializationVectorInManifest"
{-# DEPRECATED cesInitializationVectorInManifest "Use generic-lens or generic-optics with 'initializationVectorInManifest' instead." #-}

-- | If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesSpekeKeyProvider :: Lens.Lens' CmafEncryptionSettings (Core.Maybe Types.SpekeKeyProviderCmaf)
cesSpekeKeyProvider = Lens.field @"spekeKeyProvider"
{-# DEPRECATED cesSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

-- | Use these settings to set up encryption with a static key provider.
--
-- /Note:/ Consider using 'staticKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesStaticKeyProvider :: Lens.Lens' CmafEncryptionSettings (Core.Maybe Types.StaticKeyProvider)
cesStaticKeyProvider = Lens.field @"staticKeyProvider"
{-# DEPRECATED cesStaticKeyProvider "Use generic-lens or generic-optics with 'staticKeyProvider' instead." #-}

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesType :: Lens.Lens' CmafEncryptionSettings (Core.Maybe Types.CmafKeyProviderType)
cesType = Lens.field @"type'"
{-# DEPRECATED cesType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON CmafEncryptionSettings where
  toJSON CmafEncryptionSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("constantInitializationVector" Core..=)
              Core.<$> constantInitializationVector,
            ("encryptionMethod" Core..=) Core.<$> encryptionMethod,
            ("initializationVectorInManifest" Core..=)
              Core.<$> initializationVectorInManifest,
            ("spekeKeyProvider" Core..=) Core.<$> spekeKeyProvider,
            ("staticKeyProvider" Core..=) Core.<$> staticKeyProvider,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON CmafEncryptionSettings where
  parseJSON =
    Core.withObject "CmafEncryptionSettings" Core.$
      \x ->
        CmafEncryptionSettings'
          Core.<$> (x Core..:? "constantInitializationVector")
          Core.<*> (x Core..:? "encryptionMethod")
          Core.<*> (x Core..:? "initializationVectorInManifest")
          Core.<*> (x Core..:? "spekeKeyProvider")
          Core.<*> (x Core..:? "staticKeyProvider")
          Core.<*> (x Core..:? "type")
