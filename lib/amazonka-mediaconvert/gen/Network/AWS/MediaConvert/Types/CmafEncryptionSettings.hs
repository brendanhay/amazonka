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
    cesEncryptionMethod,
    cesConstantInitializationVector,
    cesType,
    cesStaticKeyProvider,
    cesSpekeKeyProvider,
    cesInitializationVectorInManifest,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmafEncryptionType
import Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
import Network.AWS.MediaConvert.Types.CmafKeyProviderType
import Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
import Network.AWS.MediaConvert.Types.StaticKeyProvider
import qualified Network.AWS.Prelude as Lude

-- | Settings for CMAF encryption
--
-- /See:/ 'mkCmafEncryptionSettings' smart constructor.
data CmafEncryptionSettings = CmafEncryptionSettings'
  { -- | Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
    encryptionMethod :: Lude.Maybe CmafEncryptionType,
    -- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
    constantInitializationVector :: Lude.Maybe Lude.Text,
    -- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
    type' :: Lude.Maybe CmafKeyProviderType,
    -- | Use these settings to set up encryption with a static key provider.
    staticKeyProvider :: Lude.Maybe StaticKeyProvider,
    -- | If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
    spekeKeyProvider :: Lude.Maybe SpekeKeyProviderCmaf,
    -- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
    initializationVectorInManifest :: Lude.Maybe CmafInitializationVectorInManifest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CmafEncryptionSettings' with the minimum fields required to make a request.
--
-- * 'encryptionMethod' - Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
-- * 'constantInitializationVector' - This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
-- * 'type'' - Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
-- * 'staticKeyProvider' - Use these settings to set up encryption with a static key provider.
-- * 'spekeKeyProvider' - If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
-- * 'initializationVectorInManifest' - When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
mkCmafEncryptionSettings ::
  CmafEncryptionSettings
mkCmafEncryptionSettings =
  CmafEncryptionSettings'
    { encryptionMethod = Lude.Nothing,
      constantInitializationVector = Lude.Nothing,
      type' = Lude.Nothing,
      staticKeyProvider = Lude.Nothing,
      spekeKeyProvider = Lude.Nothing,
      initializationVectorInManifest = Lude.Nothing
    }

-- | Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
--
-- /Note:/ Consider using 'encryptionMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesEncryptionMethod :: Lens.Lens' CmafEncryptionSettings (Lude.Maybe CmafEncryptionType)
cesEncryptionMethod = Lens.lens (encryptionMethod :: CmafEncryptionSettings -> Lude.Maybe CmafEncryptionType) (\s a -> s {encryptionMethod = a} :: CmafEncryptionSettings)
{-# DEPRECATED cesEncryptionMethod "Use generic-lens or generic-optics with 'encryptionMethod' instead." #-}

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
--
-- /Note:/ Consider using 'constantInitializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesConstantInitializationVector :: Lens.Lens' CmafEncryptionSettings (Lude.Maybe Lude.Text)
cesConstantInitializationVector = Lens.lens (constantInitializationVector :: CmafEncryptionSettings -> Lude.Maybe Lude.Text) (\s a -> s {constantInitializationVector = a} :: CmafEncryptionSettings)
{-# DEPRECATED cesConstantInitializationVector "Use generic-lens or generic-optics with 'constantInitializationVector' instead." #-}

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesType :: Lens.Lens' CmafEncryptionSettings (Lude.Maybe CmafKeyProviderType)
cesType = Lens.lens (type' :: CmafEncryptionSettings -> Lude.Maybe CmafKeyProviderType) (\s a -> s {type' = a} :: CmafEncryptionSettings)
{-# DEPRECATED cesType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Use these settings to set up encryption with a static key provider.
--
-- /Note:/ Consider using 'staticKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesStaticKeyProvider :: Lens.Lens' CmafEncryptionSettings (Lude.Maybe StaticKeyProvider)
cesStaticKeyProvider = Lens.lens (staticKeyProvider :: CmafEncryptionSettings -> Lude.Maybe StaticKeyProvider) (\s a -> s {staticKeyProvider = a} :: CmafEncryptionSettings)
{-# DEPRECATED cesStaticKeyProvider "Use generic-lens or generic-optics with 'staticKeyProvider' instead." #-}

-- | If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesSpekeKeyProvider :: Lens.Lens' CmafEncryptionSettings (Lude.Maybe SpekeKeyProviderCmaf)
cesSpekeKeyProvider = Lens.lens (spekeKeyProvider :: CmafEncryptionSettings -> Lude.Maybe SpekeKeyProviderCmaf) (\s a -> s {spekeKeyProvider = a} :: CmafEncryptionSettings)
{-# DEPRECATED cesSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

-- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
--
-- /Note:/ Consider using 'initializationVectorInManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesInitializationVectorInManifest :: Lens.Lens' CmafEncryptionSettings (Lude.Maybe CmafInitializationVectorInManifest)
cesInitializationVectorInManifest = Lens.lens (initializationVectorInManifest :: CmafEncryptionSettings -> Lude.Maybe CmafInitializationVectorInManifest) (\s a -> s {initializationVectorInManifest = a} :: CmafEncryptionSettings)
{-# DEPRECATED cesInitializationVectorInManifest "Use generic-lens or generic-optics with 'initializationVectorInManifest' instead." #-}

instance Lude.FromJSON CmafEncryptionSettings where
  parseJSON =
    Lude.withObject
      "CmafEncryptionSettings"
      ( \x ->
          CmafEncryptionSettings'
            Lude.<$> (x Lude..:? "encryptionMethod")
            Lude.<*> (x Lude..:? "constantInitializationVector")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "staticKeyProvider")
            Lude.<*> (x Lude..:? "spekeKeyProvider")
            Lude.<*> (x Lude..:? "initializationVectorInManifest")
      )

instance Lude.ToJSON CmafEncryptionSettings where
  toJSON CmafEncryptionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("encryptionMethod" Lude..=) Lude.<$> encryptionMethod,
            ("constantInitializationVector" Lude..=)
              Lude.<$> constantInitializationVector,
            ("type" Lude..=) Lude.<$> type',
            ("staticKeyProvider" Lude..=) Lude.<$> staticKeyProvider,
            ("spekeKeyProvider" Lude..=) Lude.<$> spekeKeyProvider,
            ("initializationVectorInManifest" Lude..=)
              Lude.<$> initializationVectorInManifest
          ]
      )
