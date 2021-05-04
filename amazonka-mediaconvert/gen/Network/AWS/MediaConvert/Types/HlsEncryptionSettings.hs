{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsEncryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsEncryptionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.HlsEncryptionType
import Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
import Network.AWS.MediaConvert.Types.HlsKeyProviderType
import Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import Network.AWS.MediaConvert.Types.StaticKeyProvider
import qualified Network.AWS.Prelude as Prelude

-- | Settings for HLS encryption
--
-- /See:/ 'newHlsEncryptionSettings' smart constructor.
data HlsEncryptionSettings = HlsEncryptionSettings'
  { -- | Enable this setting to insert the EXT-X-SESSION-KEY element into the
    -- master playlist. This allows for offline Apple HLS FairPlay content
    -- protection.
    offlineEncrypted :: Prelude.Maybe HlsOfflineEncrypted,
    -- | If your output group type is HLS, DASH, or Microsoft Smooth, use these
    -- settings when doing DRM encryption with a SPEKE-compliant key provider.
    -- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
    -- instead.
    spekeKeyProvider :: Prelude.Maybe SpekeKeyProvider,
    -- | Encrypts the segments with the given encryption scheme. Leave blank to
    -- disable. Selecting \'Disabled\' in the web interface also disables
    -- encryption.
    encryptionMethod :: Prelude.Maybe HlsEncryptionType,
    -- | This is a 128-bit, 16-byte hex value represented by a 32-character text
    -- string. If this parameter is not set then the Initialization Vector will
    -- follow the segment number by default.
    constantInitializationVector :: Prelude.Maybe Prelude.Text,
    -- | The Initialization Vector is a 128-bit number used in conjunction with
    -- the key for encrypting blocks. If set to INCLUDE, Initialization Vector
    -- is listed in the manifest. Otherwise Initialization Vector is not in the
    -- manifest.
    initializationVectorInManifest :: Prelude.Maybe HlsInitializationVectorInManifest,
    -- | Use these settings to set up encryption with a static key provider.
    staticKeyProvider :: Prelude.Maybe StaticKeyProvider,
    -- | Specify whether your DRM encryption key is static or from a key provider
    -- that follows the SPEKE standard. For more information about SPEKE, see
    -- https:\/\/docs.aws.amazon.com\/speke\/latest\/documentation\/what-is-speke.html.
    type' :: Prelude.Maybe HlsKeyProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HlsEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offlineEncrypted', 'hlsEncryptionSettings_offlineEncrypted' - Enable this setting to insert the EXT-X-SESSION-KEY element into the
-- master playlist. This allows for offline Apple HLS FairPlay content
-- protection.
--
-- 'spekeKeyProvider', 'hlsEncryptionSettings_spekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these
-- settings when doing DRM encryption with a SPEKE-compliant key provider.
-- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
-- instead.
--
-- 'encryptionMethod', 'hlsEncryptionSettings_encryptionMethod' - Encrypts the segments with the given encryption scheme. Leave blank to
-- disable. Selecting \'Disabled\' in the web interface also disables
-- encryption.
--
-- 'constantInitializationVector', 'hlsEncryptionSettings_constantInitializationVector' - This is a 128-bit, 16-byte hex value represented by a 32-character text
-- string. If this parameter is not set then the Initialization Vector will
-- follow the segment number by default.
--
-- 'initializationVectorInManifest', 'hlsEncryptionSettings_initializationVectorInManifest' - The Initialization Vector is a 128-bit number used in conjunction with
-- the key for encrypting blocks. If set to INCLUDE, Initialization Vector
-- is listed in the manifest. Otherwise Initialization Vector is not in the
-- manifest.
--
-- 'staticKeyProvider', 'hlsEncryptionSettings_staticKeyProvider' - Use these settings to set up encryption with a static key provider.
--
-- 'type'', 'hlsEncryptionSettings_type' - Specify whether your DRM encryption key is static or from a key provider
-- that follows the SPEKE standard. For more information about SPEKE, see
-- https:\/\/docs.aws.amazon.com\/speke\/latest\/documentation\/what-is-speke.html.
newHlsEncryptionSettings ::
  HlsEncryptionSettings
newHlsEncryptionSettings =
  HlsEncryptionSettings'
    { offlineEncrypted =
        Prelude.Nothing,
      spekeKeyProvider = Prelude.Nothing,
      encryptionMethod = Prelude.Nothing,
      constantInitializationVector = Prelude.Nothing,
      initializationVectorInManifest = Prelude.Nothing,
      staticKeyProvider = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Enable this setting to insert the EXT-X-SESSION-KEY element into the
-- master playlist. This allows for offline Apple HLS FairPlay content
-- protection.
hlsEncryptionSettings_offlineEncrypted :: Lens.Lens' HlsEncryptionSettings (Prelude.Maybe HlsOfflineEncrypted)
hlsEncryptionSettings_offlineEncrypted = Lens.lens (\HlsEncryptionSettings' {offlineEncrypted} -> offlineEncrypted) (\s@HlsEncryptionSettings' {} a -> s {offlineEncrypted = a} :: HlsEncryptionSettings)

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these
-- settings when doing DRM encryption with a SPEKE-compliant key provider.
-- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
-- instead.
hlsEncryptionSettings_spekeKeyProvider :: Lens.Lens' HlsEncryptionSettings (Prelude.Maybe SpekeKeyProvider)
hlsEncryptionSettings_spekeKeyProvider = Lens.lens (\HlsEncryptionSettings' {spekeKeyProvider} -> spekeKeyProvider) (\s@HlsEncryptionSettings' {} a -> s {spekeKeyProvider = a} :: HlsEncryptionSettings)

-- | Encrypts the segments with the given encryption scheme. Leave blank to
-- disable. Selecting \'Disabled\' in the web interface also disables
-- encryption.
hlsEncryptionSettings_encryptionMethod :: Lens.Lens' HlsEncryptionSettings (Prelude.Maybe HlsEncryptionType)
hlsEncryptionSettings_encryptionMethod = Lens.lens (\HlsEncryptionSettings' {encryptionMethod} -> encryptionMethod) (\s@HlsEncryptionSettings' {} a -> s {encryptionMethod = a} :: HlsEncryptionSettings)

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text
-- string. If this parameter is not set then the Initialization Vector will
-- follow the segment number by default.
hlsEncryptionSettings_constantInitializationVector :: Lens.Lens' HlsEncryptionSettings (Prelude.Maybe Prelude.Text)
hlsEncryptionSettings_constantInitializationVector = Lens.lens (\HlsEncryptionSettings' {constantInitializationVector} -> constantInitializationVector) (\s@HlsEncryptionSettings' {} a -> s {constantInitializationVector = a} :: HlsEncryptionSettings)

-- | The Initialization Vector is a 128-bit number used in conjunction with
-- the key for encrypting blocks. If set to INCLUDE, Initialization Vector
-- is listed in the manifest. Otherwise Initialization Vector is not in the
-- manifest.
hlsEncryptionSettings_initializationVectorInManifest :: Lens.Lens' HlsEncryptionSettings (Prelude.Maybe HlsInitializationVectorInManifest)
hlsEncryptionSettings_initializationVectorInManifest = Lens.lens (\HlsEncryptionSettings' {initializationVectorInManifest} -> initializationVectorInManifest) (\s@HlsEncryptionSettings' {} a -> s {initializationVectorInManifest = a} :: HlsEncryptionSettings)

-- | Use these settings to set up encryption with a static key provider.
hlsEncryptionSettings_staticKeyProvider :: Lens.Lens' HlsEncryptionSettings (Prelude.Maybe StaticKeyProvider)
hlsEncryptionSettings_staticKeyProvider = Lens.lens (\HlsEncryptionSettings' {staticKeyProvider} -> staticKeyProvider) (\s@HlsEncryptionSettings' {} a -> s {staticKeyProvider = a} :: HlsEncryptionSettings)

-- | Specify whether your DRM encryption key is static or from a key provider
-- that follows the SPEKE standard. For more information about SPEKE, see
-- https:\/\/docs.aws.amazon.com\/speke\/latest\/documentation\/what-is-speke.html.
hlsEncryptionSettings_type :: Lens.Lens' HlsEncryptionSettings (Prelude.Maybe HlsKeyProviderType)
hlsEncryptionSettings_type = Lens.lens (\HlsEncryptionSettings' {type'} -> type') (\s@HlsEncryptionSettings' {} a -> s {type' = a} :: HlsEncryptionSettings)

instance Prelude.FromJSON HlsEncryptionSettings where
  parseJSON =
    Prelude.withObject
      "HlsEncryptionSettings"
      ( \x ->
          HlsEncryptionSettings'
            Prelude.<$> (x Prelude..:? "offlineEncrypted")
            Prelude.<*> (x Prelude..:? "spekeKeyProvider")
            Prelude.<*> (x Prelude..:? "encryptionMethod")
            Prelude.<*> (x Prelude..:? "constantInitializationVector")
            Prelude.<*> (x Prelude..:? "initializationVectorInManifest")
            Prelude.<*> (x Prelude..:? "staticKeyProvider")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable HlsEncryptionSettings

instance Prelude.NFData HlsEncryptionSettings

instance Prelude.ToJSON HlsEncryptionSettings where
  toJSON HlsEncryptionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("offlineEncrypted" Prelude..=)
              Prelude.<$> offlineEncrypted,
            ("spekeKeyProvider" Prelude..=)
              Prelude.<$> spekeKeyProvider,
            ("encryptionMethod" Prelude..=)
              Prelude.<$> encryptionMethod,
            ("constantInitializationVector" Prelude..=)
              Prelude.<$> constantInitializationVector,
            ("initializationVectorInManifest" Prelude..=)
              Prelude.<$> initializationVectorInManifest,
            ("staticKeyProvider" Prelude..=)
              Prelude.<$> staticKeyProvider,
            ("type" Prelude..=) Prelude.<$> type'
          ]
      )
