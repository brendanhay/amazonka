{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.HlsContentProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.HlsContentProtection where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to your output files.
--
--
--
-- /See:/ 'hlsContentProtection' smart constructor.
data HlsContentProtection = HlsContentProtection'
  { _hcpKeyMD5 ::
      !(Maybe Text),
    _hcpKeyStoragePolicy :: !(Maybe Text),
    _hcpKey :: !(Maybe Text),
    _hcpMethod :: !(Maybe Text),
    _hcpInitializationVector :: !(Maybe Text),
    _hcpLicenseAcquisitionURL :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsContentProtection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcpKeyMD5' - If Elastic Transcoder is generating your key for you, you must leave this field blank. The MD5 digest of the key that you want Elastic Transcoder to use to encrypt your output file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64- encoded.
--
-- * 'hcpKeyStoragePolicy' - Specify whether you want Elastic Transcoder to write your HLS license key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@ , @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes your data key into the same bucket as the associated playlist.
--
-- * 'hcpKey' - If you want Elastic Transcoder to generate a key for you, leave this field blank. If you choose to supply your own key, you must encrypt the key by using AWS KMS. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .
--
-- * 'hcpMethod' - The content protection method for your output. The only valid value is: @aes-128@ . This value is written into the method attribute of the @EXT-X-KEY@ metadata tag in the output playlist.
--
-- * 'hcpInitializationVector' - If Elastic Transcoder is generating your key for you, you must leave this field blank. The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
--
-- * 'hcpLicenseAcquisitionURL' - The location of the license key required to decrypt your HLS playlist. The URL must be an absolute path, and is referenced in the URI attribute of the EXT-X-KEY metadata tag in the playlist file.
hlsContentProtection ::
  HlsContentProtection
hlsContentProtection =
  HlsContentProtection'
    { _hcpKeyMD5 = Nothing,
      _hcpKeyStoragePolicy = Nothing,
      _hcpKey = Nothing,
      _hcpMethod = Nothing,
      _hcpInitializationVector = Nothing,
      _hcpLicenseAcquisitionURL = Nothing
    }

-- | If Elastic Transcoder is generating your key for you, you must leave this field blank. The MD5 digest of the key that you want Elastic Transcoder to use to encrypt your output file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64- encoded.
hcpKeyMD5 :: Lens' HlsContentProtection (Maybe Text)
hcpKeyMD5 = lens _hcpKeyMD5 (\s a -> s {_hcpKeyMD5 = a})

-- | Specify whether you want Elastic Transcoder to write your HLS license key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@ , @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes your data key into the same bucket as the associated playlist.
hcpKeyStoragePolicy :: Lens' HlsContentProtection (Maybe Text)
hcpKeyStoragePolicy = lens _hcpKeyStoragePolicy (\s a -> s {_hcpKeyStoragePolicy = a})

-- | If you want Elastic Transcoder to generate a key for you, leave this field blank. If you choose to supply your own key, you must encrypt the key by using AWS KMS. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .
hcpKey :: Lens' HlsContentProtection (Maybe Text)
hcpKey = lens _hcpKey (\s a -> s {_hcpKey = a})

-- | The content protection method for your output. The only valid value is: @aes-128@ . This value is written into the method attribute of the @EXT-X-KEY@ metadata tag in the output playlist.
hcpMethod :: Lens' HlsContentProtection (Maybe Text)
hcpMethod = lens _hcpMethod (\s a -> s {_hcpMethod = a})

-- | If Elastic Transcoder is generating your key for you, you must leave this field blank. The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
hcpInitializationVector :: Lens' HlsContentProtection (Maybe Text)
hcpInitializationVector = lens _hcpInitializationVector (\s a -> s {_hcpInitializationVector = a})

-- | The location of the license key required to decrypt your HLS playlist. The URL must be an absolute path, and is referenced in the URI attribute of the EXT-X-KEY metadata tag in the playlist file.
hcpLicenseAcquisitionURL :: Lens' HlsContentProtection (Maybe Text)
hcpLicenseAcquisitionURL = lens _hcpLicenseAcquisitionURL (\s a -> s {_hcpLicenseAcquisitionURL = a})

instance FromJSON HlsContentProtection where
  parseJSON =
    withObject
      "HlsContentProtection"
      ( \x ->
          HlsContentProtection'
            <$> (x .:? "KeyMd5")
            <*> (x .:? "KeyStoragePolicy")
            <*> (x .:? "Key")
            <*> (x .:? "Method")
            <*> (x .:? "InitializationVector")
            <*> (x .:? "LicenseAcquisitionUrl")
      )

instance Hashable HlsContentProtection

instance NFData HlsContentProtection

instance ToJSON HlsContentProtection where
  toJSON HlsContentProtection' {..} =
    object
      ( catMaybes
          [ ("KeyMd5" .=) <$> _hcpKeyMD5,
            ("KeyStoragePolicy" .=) <$> _hcpKeyStoragePolicy,
            ("Key" .=) <$> _hcpKey,
            ("Method" .=) <$> _hcpMethod,
            ("InitializationVector" .=) <$> _hcpInitializationVector,
            ("LicenseAcquisitionUrl" .=) <$> _hcpLicenseAcquisitionURL
          ]
      )
