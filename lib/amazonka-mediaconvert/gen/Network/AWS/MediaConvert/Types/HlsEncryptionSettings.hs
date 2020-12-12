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
    hesOfflineEncrypted,
    hesEncryptionMethod,
    hesConstantInitializationVector,
    hesType,
    hesStaticKeyProvider,
    hesSpekeKeyProvider,
    hesInitializationVectorInManifest,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.HlsEncryptionType
import Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
import Network.AWS.MediaConvert.Types.HlsKeyProviderType
import Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import Network.AWS.MediaConvert.Types.StaticKeyProvider
import qualified Network.AWS.Prelude as Lude

-- | Settings for HLS encryption
--
-- /See:/ 'mkHlsEncryptionSettings' smart constructor.
data HlsEncryptionSettings = HlsEncryptionSettings'
  { offlineEncrypted ::
      Lude.Maybe HlsOfflineEncrypted,
    encryptionMethod ::
      Lude.Maybe HlsEncryptionType,
    constantInitializationVector ::
      Lude.Maybe Lude.Text,
    type' :: Lude.Maybe HlsKeyProviderType,
    staticKeyProvider ::
      Lude.Maybe StaticKeyProvider,
    spekeKeyProvider :: Lude.Maybe SpekeKeyProvider,
    initializationVectorInManifest ::
      Lude.Maybe HlsInitializationVectorInManifest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsEncryptionSettings' with the minimum fields required to make a request.
--
-- * 'constantInitializationVector' - This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
-- * 'encryptionMethod' - Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
-- * 'initializationVectorInManifest' - The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
-- * 'offlineEncrypted' - Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
-- * 'spekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
-- * 'staticKeyProvider' - Use these settings to set up encryption with a static key provider.
-- * 'type'' - Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
mkHlsEncryptionSettings ::
  HlsEncryptionSettings
mkHlsEncryptionSettings =
  HlsEncryptionSettings'
    { offlineEncrypted = Lude.Nothing,
      encryptionMethod = Lude.Nothing,
      constantInitializationVector = Lude.Nothing,
      type' = Lude.Nothing,
      staticKeyProvider = Lude.Nothing,
      spekeKeyProvider = Lude.Nothing,
      initializationVectorInManifest = Lude.Nothing
    }

-- | Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
--
-- /Note:/ Consider using 'offlineEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesOfflineEncrypted :: Lens.Lens' HlsEncryptionSettings (Lude.Maybe HlsOfflineEncrypted)
hesOfflineEncrypted = Lens.lens (offlineEncrypted :: HlsEncryptionSettings -> Lude.Maybe HlsOfflineEncrypted) (\s a -> s {offlineEncrypted = a} :: HlsEncryptionSettings)
{-# DEPRECATED hesOfflineEncrypted "Use generic-lens or generic-optics with 'offlineEncrypted' instead." #-}

-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
--
-- /Note:/ Consider using 'encryptionMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesEncryptionMethod :: Lens.Lens' HlsEncryptionSettings (Lude.Maybe HlsEncryptionType)
hesEncryptionMethod = Lens.lens (encryptionMethod :: HlsEncryptionSettings -> Lude.Maybe HlsEncryptionType) (\s a -> s {encryptionMethod = a} :: HlsEncryptionSettings)
{-# DEPRECATED hesEncryptionMethod "Use generic-lens or generic-optics with 'encryptionMethod' instead." #-}

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
--
-- /Note:/ Consider using 'constantInitializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesConstantInitializationVector :: Lens.Lens' HlsEncryptionSettings (Lude.Maybe Lude.Text)
hesConstantInitializationVector = Lens.lens (constantInitializationVector :: HlsEncryptionSettings -> Lude.Maybe Lude.Text) (\s a -> s {constantInitializationVector = a} :: HlsEncryptionSettings)
{-# DEPRECATED hesConstantInitializationVector "Use generic-lens or generic-optics with 'constantInitializationVector' instead." #-}

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesType :: Lens.Lens' HlsEncryptionSettings (Lude.Maybe HlsKeyProviderType)
hesType = Lens.lens (type' :: HlsEncryptionSettings -> Lude.Maybe HlsKeyProviderType) (\s a -> s {type' = a} :: HlsEncryptionSettings)
{-# DEPRECATED hesType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Use these settings to set up encryption with a static key provider.
--
-- /Note:/ Consider using 'staticKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesStaticKeyProvider :: Lens.Lens' HlsEncryptionSettings (Lude.Maybe StaticKeyProvider)
hesStaticKeyProvider = Lens.lens (staticKeyProvider :: HlsEncryptionSettings -> Lude.Maybe StaticKeyProvider) (\s a -> s {staticKeyProvider = a} :: HlsEncryptionSettings)
{-# DEPRECATED hesStaticKeyProvider "Use generic-lens or generic-optics with 'staticKeyProvider' instead." #-}

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesSpekeKeyProvider :: Lens.Lens' HlsEncryptionSettings (Lude.Maybe SpekeKeyProvider)
hesSpekeKeyProvider = Lens.lens (spekeKeyProvider :: HlsEncryptionSettings -> Lude.Maybe SpekeKeyProvider) (\s a -> s {spekeKeyProvider = a} :: HlsEncryptionSettings)
{-# DEPRECATED hesSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
--
-- /Note:/ Consider using 'initializationVectorInManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hesInitializationVectorInManifest :: Lens.Lens' HlsEncryptionSettings (Lude.Maybe HlsInitializationVectorInManifest)
hesInitializationVectorInManifest = Lens.lens (initializationVectorInManifest :: HlsEncryptionSettings -> Lude.Maybe HlsInitializationVectorInManifest) (\s a -> s {initializationVectorInManifest = a} :: HlsEncryptionSettings)
{-# DEPRECATED hesInitializationVectorInManifest "Use generic-lens or generic-optics with 'initializationVectorInManifest' instead." #-}

instance Lude.FromJSON HlsEncryptionSettings where
  parseJSON =
    Lude.withObject
      "HlsEncryptionSettings"
      ( \x ->
          HlsEncryptionSettings'
            Lude.<$> (x Lude..:? "offlineEncrypted")
            Lude.<*> (x Lude..:? "encryptionMethod")
            Lude.<*> (x Lude..:? "constantInitializationVector")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "staticKeyProvider")
            Lude.<*> (x Lude..:? "spekeKeyProvider")
            Lude.<*> (x Lude..:? "initializationVectorInManifest")
      )

instance Lude.ToJSON HlsEncryptionSettings where
  toJSON HlsEncryptionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("offlineEncrypted" Lude..=) Lude.<$> offlineEncrypted,
            ("encryptionMethod" Lude..=) Lude.<$> encryptionMethod,
            ("constantInitializationVector" Lude..=)
              Lude.<$> constantInitializationVector,
            ("type" Lude..=) Lude.<$> type',
            ("staticKeyProvider" Lude..=) Lude.<$> staticKeyProvider,
            ("spekeKeyProvider" Lude..=) Lude.<$> spekeKeyProvider,
            ("initializationVectorInManifest" Lude..=)
              Lude.<$> initializationVectorInManifest
          ]
      )
