{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafEncryptionSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.CmafEncryptionType
import Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
import Network.AWS.MediaConvert.Types.CmafKeyProviderType
import Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
import Network.AWS.MediaConvert.Types.StaticKeyProvider
import Network.AWS.Prelude

-- | Settings for CMAF encryption
--
-- /See:/ 'cmafEncryptionSettings' smart constructor.
data CmafEncryptionSettings = CmafEncryptionSettings'
  { _cesEncryptionMethod ::
      !(Maybe CmafEncryptionType),
    _cesConstantInitializationVector ::
      !(Maybe Text),
    _cesType :: !(Maybe CmafKeyProviderType),
    _cesStaticKeyProvider ::
      !(Maybe StaticKeyProvider),
    _cesSpekeKeyProvider ::
      !(Maybe SpekeKeyProviderCmaf),
    _cesInitializationVectorInManifest ::
      !(Maybe CmafInitializationVectorInManifest)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CmafEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesEncryptionMethod' - Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
--
-- * 'cesConstantInitializationVector' - This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
--
-- * 'cesType' - Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
--
-- * 'cesStaticKeyProvider' - Use these settings to set up encryption with a static key provider.
--
-- * 'cesSpekeKeyProvider' - If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
--
-- * 'cesInitializationVectorInManifest' - When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
cmafEncryptionSettings ::
  CmafEncryptionSettings
cmafEncryptionSettings =
  CmafEncryptionSettings'
    { _cesEncryptionMethod = Nothing,
      _cesConstantInitializationVector = Nothing,
      _cesType = Nothing,
      _cesStaticKeyProvider = Nothing,
      _cesSpekeKeyProvider = Nothing,
      _cesInitializationVectorInManifest = Nothing
    }

-- | Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
cesEncryptionMethod :: Lens' CmafEncryptionSettings (Maybe CmafEncryptionType)
cesEncryptionMethod = lens _cesEncryptionMethod (\s a -> s {_cesEncryptionMethod = a})

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
cesConstantInitializationVector :: Lens' CmafEncryptionSettings (Maybe Text)
cesConstantInitializationVector = lens _cesConstantInitializationVector (\s a -> s {_cesConstantInitializationVector = a})

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
cesType :: Lens' CmafEncryptionSettings (Maybe CmafKeyProviderType)
cesType = lens _cesType (\s a -> s {_cesType = a})

-- | Use these settings to set up encryption with a static key provider.
cesStaticKeyProvider :: Lens' CmafEncryptionSettings (Maybe StaticKeyProvider)
cesStaticKeyProvider = lens _cesStaticKeyProvider (\s a -> s {_cesStaticKeyProvider = a})

-- | If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
cesSpekeKeyProvider :: Lens' CmafEncryptionSettings (Maybe SpekeKeyProviderCmaf)
cesSpekeKeyProvider = lens _cesSpekeKeyProvider (\s a -> s {_cesSpekeKeyProvider = a})

-- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
cesInitializationVectorInManifest :: Lens' CmafEncryptionSettings (Maybe CmafInitializationVectorInManifest)
cesInitializationVectorInManifest = lens _cesInitializationVectorInManifest (\s a -> s {_cesInitializationVectorInManifest = a})

instance FromJSON CmafEncryptionSettings where
  parseJSON =
    withObject
      "CmafEncryptionSettings"
      ( \x ->
          CmafEncryptionSettings'
            <$> (x .:? "encryptionMethod")
            <*> (x .:? "constantInitializationVector")
            <*> (x .:? "type")
            <*> (x .:? "staticKeyProvider")
            <*> (x .:? "spekeKeyProvider")
            <*> (x .:? "initializationVectorInManifest")
      )

instance Hashable CmafEncryptionSettings

instance NFData CmafEncryptionSettings

instance ToJSON CmafEncryptionSettings where
  toJSON CmafEncryptionSettings' {..} =
    object
      ( catMaybes
          [ ("encryptionMethod" .=) <$> _cesEncryptionMethod,
            ("constantInitializationVector" .=)
              <$> _cesConstantInitializationVector,
            ("type" .=) <$> _cesType,
            ("staticKeyProvider" .=) <$> _cesStaticKeyProvider,
            ("spekeKeyProvider" .=) <$> _cesSpekeKeyProvider,
            ("initializationVectorInManifest" .=)
              <$> _cesInitializationVectorInManifest
          ]
      )
