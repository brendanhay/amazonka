{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
  ( PlayReadyDrm (..),

    -- * Smart constructor
    mkPlayReadyDrm,

    -- * Lenses
    prdKeyId,
    prdFormat,
    prdKeyMD5,
    prdKey,
    prdInitializationVector,
    prdLicenseAcquisitionURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The PlayReady DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- PlayReady DRM encrypts your media files using @aes-ctr@ encryption.
-- If you use DRM for an @HLSv3@ playlist, your outputs must have a master playlist.
--
-- /See:/ 'mkPlayReadyDrm' smart constructor.
data PlayReadyDrm = PlayReadyDrm'
  { keyId :: Lude.Maybe Lude.Text,
    format :: Lude.Maybe Lude.Text,
    keyMD5 :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'PlayReadyDrm' with the minimum fields required to make a request.
--
-- * 'format' - The type of DRM, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
-- * 'initializationVector' - The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your files. The initialization vector must be base64-encoded, and it must be exactly 8 bytes long before being base64-encoded. If no initialization vector is provided, Elastic Transcoder generates one for you.
-- * 'key' - The DRM key for your file, provided by your DRM license provider. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded:
--
-- @128@ , @192@ , or @256@ .
-- The key must also be encrypted by using AWS KMS.
-- * 'keyId' - The ID for your DRM key, so that your DRM license provider knows which key to provide.
--
-- The key ID must be provided in big endian, and Elastic Transcoder converts it to little endian before inserting it into the PlayReady DRM headers. If you are unsure whether your license server provides your key ID in big or little endian, check with your DRM provider.
-- * 'keyMD5' - The MD5 digest of the key used for DRM on your file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
-- * 'licenseAcquisitionURL' - The location of the license key required to play DRM content. The URL must be an absolute path, and is referenced by the PlayReady header. The PlayReady header is referenced in the protection header of the client manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL looks like this: @https://www.example.com/exampleKey/@
mkPlayReadyDrm ::
  PlayReadyDrm
mkPlayReadyDrm =
  PlayReadyDrm'
    { keyId = Lude.Nothing,
      format = Lude.Nothing,
      keyMD5 = Lude.Nothing,
      key = Lude.Nothing,
      initializationVector = Lude.Nothing,
      licenseAcquisitionURL = Lude.Nothing
    }

-- | The ID for your DRM key, so that your DRM license provider knows which key to provide.
--
-- The key ID must be provided in big endian, and Elastic Transcoder converts it to little endian before inserting it into the PlayReady DRM headers. If you are unsure whether your license server provides your key ID in big or little endian, check with your DRM provider.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdKeyId :: Lens.Lens' PlayReadyDrm (Lude.Maybe Lude.Text)
prdKeyId = Lens.lens (keyId :: PlayReadyDrm -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: PlayReadyDrm)
{-# DEPRECATED prdKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The type of DRM, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdFormat :: Lens.Lens' PlayReadyDrm (Lude.Maybe Lude.Text)
prdFormat = Lens.lens (format :: PlayReadyDrm -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: PlayReadyDrm)
{-# DEPRECATED prdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The MD5 digest of the key used for DRM on your file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
--
-- /Note:/ Consider using 'keyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdKeyMD5 :: Lens.Lens' PlayReadyDrm (Lude.Maybe Lude.Text)
prdKeyMD5 = Lens.lens (keyMD5 :: PlayReadyDrm -> Lude.Maybe Lude.Text) (\s a -> s {keyMD5 = a} :: PlayReadyDrm)
{-# DEPRECATED prdKeyMD5 "Use generic-lens or generic-optics with 'keyMD5' instead." #-}

-- | The DRM key for your file, provided by your DRM license provider. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded:
--
-- @128@ , @192@ , or @256@ .
-- The key must also be encrypted by using AWS KMS.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdKey :: Lens.Lens' PlayReadyDrm (Lude.Maybe Lude.Text)
prdKey = Lens.lens (key :: PlayReadyDrm -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: PlayReadyDrm)
{-# DEPRECATED prdKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your files. The initialization vector must be base64-encoded, and it must be exactly 8 bytes long before being base64-encoded. If no initialization vector is provided, Elastic Transcoder generates one for you.
--
-- /Note:/ Consider using 'initializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdInitializationVector :: Lens.Lens' PlayReadyDrm (Lude.Maybe Lude.Text)
prdInitializationVector = Lens.lens (initializationVector :: PlayReadyDrm -> Lude.Maybe Lude.Text) (\s a -> s {initializationVector = a} :: PlayReadyDrm)
{-# DEPRECATED prdInitializationVector "Use generic-lens or generic-optics with 'initializationVector' instead." #-}

-- | The location of the license key required to play DRM content. The URL must be an absolute path, and is referenced by the PlayReady header. The PlayReady header is referenced in the protection header of the client manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL looks like this: @https://www.example.com/exampleKey/@
--
-- /Note:/ Consider using 'licenseAcquisitionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdLicenseAcquisitionURL :: Lens.Lens' PlayReadyDrm (Lude.Maybe Lude.Text)
prdLicenseAcquisitionURL = Lens.lens (licenseAcquisitionURL :: PlayReadyDrm -> Lude.Maybe Lude.Text) (\s a -> s {licenseAcquisitionURL = a} :: PlayReadyDrm)
{-# DEPRECATED prdLicenseAcquisitionURL "Use generic-lens or generic-optics with 'licenseAcquisitionURL' instead." #-}

instance Lude.FromJSON PlayReadyDrm where
  parseJSON =
    Lude.withObject
      "PlayReadyDrm"
      ( \x ->
          PlayReadyDrm'
            Lude.<$> (x Lude..:? "KeyId")
            Lude.<*> (x Lude..:? "Format")
            Lude.<*> (x Lude..:? "KeyMd5")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "InitializationVector")
            Lude.<*> (x Lude..:? "LicenseAcquisitionUrl")
      )

instance Lude.ToJSON PlayReadyDrm where
  toJSON PlayReadyDrm' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyId" Lude..=) Lude.<$> keyId,
            ("Format" Lude..=) Lude.<$> format,
            ("KeyMd5" Lude..=) Lude.<$> keyMD5,
            ("Key" Lude..=) Lude.<$> key,
            ("InitializationVector" Lude..=) Lude.<$> initializationVector,
            ("LicenseAcquisitionUrl" Lude..=) Lude.<$> licenseAcquisitionURL
          ]
      )
