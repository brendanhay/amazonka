{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsEncryptionSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.HlsEncryptionType
import Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
import Network.AWS.MediaConvert.Types.HlsKeyProviderType
import Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import Network.AWS.MediaConvert.Types.StaticKeyProvider
import Network.AWS.Prelude

-- | Settings for HLS encryption
--
-- /See:/ 'hlsEncryptionSettings' smart constructor.
data HlsEncryptionSettings = HlsEncryptionSettings'
  { _hesOfflineEncrypted ::
      !(Maybe HlsOfflineEncrypted),
    _hesEncryptionMethod ::
      !(Maybe HlsEncryptionType),
    _hesConstantInitializationVector ::
      !(Maybe Text),
    _hesType :: !(Maybe HlsKeyProviderType),
    _hesStaticKeyProvider ::
      !(Maybe StaticKeyProvider),
    _hesSpekeKeyProvider ::
      !(Maybe SpekeKeyProvider),
    _hesInitializationVectorInManifest ::
      !(Maybe HlsInitializationVectorInManifest)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hesOfflineEncrypted' - Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
--
-- * 'hesEncryptionMethod' - Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
--
-- * 'hesConstantInitializationVector' - This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
--
-- * 'hesType' - Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
--
-- * 'hesStaticKeyProvider' - Use these settings to set up encryption with a static key provider.
--
-- * 'hesSpekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- * 'hesInitializationVectorInManifest' - The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
hlsEncryptionSettings ::
  HlsEncryptionSettings
hlsEncryptionSettings =
  HlsEncryptionSettings'
    { _hesOfflineEncrypted = Nothing,
      _hesEncryptionMethod = Nothing,
      _hesConstantInitializationVector = Nothing,
      _hesType = Nothing,
      _hesStaticKeyProvider = Nothing,
      _hesSpekeKeyProvider = Nothing,
      _hesInitializationVectorInManifest = Nothing
    }

-- | Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
hesOfflineEncrypted :: Lens' HlsEncryptionSettings (Maybe HlsOfflineEncrypted)
hesOfflineEncrypted = lens _hesOfflineEncrypted (\s a -> s {_hesOfflineEncrypted = a})

-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
hesEncryptionMethod :: Lens' HlsEncryptionSettings (Maybe HlsEncryptionType)
hesEncryptionMethod = lens _hesEncryptionMethod (\s a -> s {_hesEncryptionMethod = a})

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
hesConstantInitializationVector :: Lens' HlsEncryptionSettings (Maybe Text)
hesConstantInitializationVector = lens _hesConstantInitializationVector (\s a -> s {_hesConstantInitializationVector = a})

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
hesType :: Lens' HlsEncryptionSettings (Maybe HlsKeyProviderType)
hesType = lens _hesType (\s a -> s {_hesType = a})

-- | Use these settings to set up encryption with a static key provider.
hesStaticKeyProvider :: Lens' HlsEncryptionSettings (Maybe StaticKeyProvider)
hesStaticKeyProvider = lens _hesStaticKeyProvider (\s a -> s {_hesStaticKeyProvider = a})

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
hesSpekeKeyProvider :: Lens' HlsEncryptionSettings (Maybe SpekeKeyProvider)
hesSpekeKeyProvider = lens _hesSpekeKeyProvider (\s a -> s {_hesSpekeKeyProvider = a})

-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
hesInitializationVectorInManifest :: Lens' HlsEncryptionSettings (Maybe HlsInitializationVectorInManifest)
hesInitializationVectorInManifest = lens _hesInitializationVectorInManifest (\s a -> s {_hesInitializationVectorInManifest = a})

instance FromJSON HlsEncryptionSettings where
  parseJSON =
    withObject
      "HlsEncryptionSettings"
      ( \x ->
          HlsEncryptionSettings'
            <$> (x .:? "offlineEncrypted")
            <*> (x .:? "encryptionMethod")
            <*> (x .:? "constantInitializationVector")
            <*> (x .:? "type")
            <*> (x .:? "staticKeyProvider")
            <*> (x .:? "spekeKeyProvider")
            <*> (x .:? "initializationVectorInManifest")
      )

instance Hashable HlsEncryptionSettings

instance NFData HlsEncryptionSettings

instance ToJSON HlsEncryptionSettings where
  toJSON HlsEncryptionSettings' {..} =
    object
      ( catMaybes
          [ ("offlineEncrypted" .=) <$> _hesOfflineEncrypted,
            ("encryptionMethod" .=) <$> _hesEncryptionMethod,
            ("constantInitializationVector" .=)
              <$> _hesConstantInitializationVector,
            ("type" .=) <$> _hesType,
            ("staticKeyProvider" .=) <$> _hesStaticKeyProvider,
            ("spekeKeyProvider" .=) <$> _hesSpekeKeyProvider,
            ("initializationVectorInManifest" .=)
              <$> _hesInitializationVectorInManifest
          ]
      )
