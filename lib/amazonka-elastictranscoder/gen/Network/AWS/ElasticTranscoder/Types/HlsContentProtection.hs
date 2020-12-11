-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.HlsContentProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.HlsContentProtection
  ( HlsContentProtection (..),

    -- * Smart constructor
    mkHlsContentProtection,

    -- * Lenses
    hcpKeyMD5,
    hcpKeyStoragePolicy,
    hcpKey,
    hcpMethod,
    hcpInitializationVector,
    hcpLicenseAcquisitionURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to your output files.
--
-- /See:/ 'mkHlsContentProtection' smart constructor.
data HlsContentProtection = HlsContentProtection'
  { keyMD5 ::
      Lude.Maybe Lude.Text,
    keyStoragePolicy :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
    method :: Lude.Maybe Lude.Text,
    initializationVector :: Lude.Maybe Lude.Text,
    licenseAcquisitionURL :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsContentProtection' with the minimum fields required to make a request.
--
-- * 'initializationVector' - If Elastic Transcoder is generating your key for you, you must leave this field blank.
--
-- The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
-- * 'key' - If you want Elastic Transcoder to generate a key for you, leave this field blank.
--
-- If you choose to supply your own key, you must encrypt the key by using AWS KMS. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded:
-- @128@ , @192@ , or @256@ .
-- * 'keyMD5' - If Elastic Transcoder is generating your key for you, you must leave this field blank.
--
-- The MD5 digest of the key that you want Elastic Transcoder to use to encrypt your output file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64- encoded.
-- * 'keyStoragePolicy' - Specify whether you want Elastic Transcoder to write your HLS license key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@ , @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes your data key into the same bucket as the associated playlist.
-- * 'licenseAcquisitionURL' - The location of the license key required to decrypt your HLS playlist. The URL must be an absolute path, and is referenced in the URI attribute of the EXT-X-KEY metadata tag in the playlist file.
-- * 'method' - The content protection method for your output. The only valid value is: @aes-128@ .
--
-- This value is written into the method attribute of the @EXT-X-KEY@ metadata tag in the output playlist.
mkHlsContentProtection ::
  HlsContentProtection
mkHlsContentProtection =
  HlsContentProtection'
    { keyMD5 = Lude.Nothing,
      keyStoragePolicy = Lude.Nothing,
      key = Lude.Nothing,
      method = Lude.Nothing,
      initializationVector = Lude.Nothing,
      licenseAcquisitionURL = Lude.Nothing
    }

-- | If Elastic Transcoder is generating your key for you, you must leave this field blank.
--
-- The MD5 digest of the key that you want Elastic Transcoder to use to encrypt your output file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64- encoded.
--
-- /Note:/ Consider using 'keyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpKeyMD5 :: Lens.Lens' HlsContentProtection (Lude.Maybe Lude.Text)
hcpKeyMD5 = Lens.lens (keyMD5 :: HlsContentProtection -> Lude.Maybe Lude.Text) (\s a -> s {keyMD5 = a} :: HlsContentProtection)
{-# DEPRECATED hcpKeyMD5 "Use generic-lens or generic-optics with 'keyMD5' instead." #-}

-- | Specify whether you want Elastic Transcoder to write your HLS license key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@ , @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes your data key into the same bucket as the associated playlist.
--
-- /Note:/ Consider using 'keyStoragePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpKeyStoragePolicy :: Lens.Lens' HlsContentProtection (Lude.Maybe Lude.Text)
hcpKeyStoragePolicy = Lens.lens (keyStoragePolicy :: HlsContentProtection -> Lude.Maybe Lude.Text) (\s a -> s {keyStoragePolicy = a} :: HlsContentProtection)
{-# DEPRECATED hcpKeyStoragePolicy "Use generic-lens or generic-optics with 'keyStoragePolicy' instead." #-}

-- | If you want Elastic Transcoder to generate a key for you, leave this field blank.
--
-- If you choose to supply your own key, you must encrypt the key by using AWS KMS. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded:
-- @128@ , @192@ , or @256@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpKey :: Lens.Lens' HlsContentProtection (Lude.Maybe Lude.Text)
hcpKey = Lens.lens (key :: HlsContentProtection -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: HlsContentProtection)
{-# DEPRECATED hcpKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The content protection method for your output. The only valid value is: @aes-128@ .
--
-- This value is written into the method attribute of the @EXT-X-KEY@ metadata tag in the output playlist.
--
-- /Note:/ Consider using 'method' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpMethod :: Lens.Lens' HlsContentProtection (Lude.Maybe Lude.Text)
hcpMethod = Lens.lens (method :: HlsContentProtection -> Lude.Maybe Lude.Text) (\s a -> s {method = a} :: HlsContentProtection)
{-# DEPRECATED hcpMethod "Use generic-lens or generic-optics with 'method' instead." #-}

-- | If Elastic Transcoder is generating your key for you, you must leave this field blank.
--
-- The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
--
-- /Note:/ Consider using 'initializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpInitializationVector :: Lens.Lens' HlsContentProtection (Lude.Maybe Lude.Text)
hcpInitializationVector = Lens.lens (initializationVector :: HlsContentProtection -> Lude.Maybe Lude.Text) (\s a -> s {initializationVector = a} :: HlsContentProtection)
{-# DEPRECATED hcpInitializationVector "Use generic-lens or generic-optics with 'initializationVector' instead." #-}

-- | The location of the license key required to decrypt your HLS playlist. The URL must be an absolute path, and is referenced in the URI attribute of the EXT-X-KEY metadata tag in the playlist file.
--
-- /Note:/ Consider using 'licenseAcquisitionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpLicenseAcquisitionURL :: Lens.Lens' HlsContentProtection (Lude.Maybe Lude.Text)
hcpLicenseAcquisitionURL = Lens.lens (licenseAcquisitionURL :: HlsContentProtection -> Lude.Maybe Lude.Text) (\s a -> s {licenseAcquisitionURL = a} :: HlsContentProtection)
{-# DEPRECATED hcpLicenseAcquisitionURL "Use generic-lens or generic-optics with 'licenseAcquisitionURL' instead." #-}

instance Lude.FromJSON HlsContentProtection where
  parseJSON =
    Lude.withObject
      "HlsContentProtection"
      ( \x ->
          HlsContentProtection'
            Lude.<$> (x Lude..:? "KeyMd5")
            Lude.<*> (x Lude..:? "KeyStoragePolicy")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "Method")
            Lude.<*> (x Lude..:? "InitializationVector")
            Lude.<*> (x Lude..:? "LicenseAcquisitionUrl")
      )

instance Lude.ToJSON HlsContentProtection where
  toJSON HlsContentProtection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyMd5" Lude..=) Lude.<$> keyMD5,
            ("KeyStoragePolicy" Lude..=) Lude.<$> keyStoragePolicy,
            ("Key" Lude..=) Lude.<$> key,
            ("Method" Lude..=) Lude.<$> method,
            ("InitializationVector" Lude..=) Lude.<$> initializationVector,
            ("LicenseAcquisitionUrl" Lude..=) Lude.<$> licenseAcquisitionURL
          ]
      )
