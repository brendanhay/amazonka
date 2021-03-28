{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
  ( PlayReadyDrm (..)
  -- * Smart constructor
  , mkPlayReadyDrm
  -- * Lenses
  , prdFormat
  , prdInitializationVector
  , prdKey
  , prdKeyId
  , prdKeyMd5
  , prdLicenseAcquisitionUrl
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.Format as Types
import qualified Network.AWS.ElasticTranscoder.Types.InitializationVector as Types
import qualified Network.AWS.ElasticTranscoder.Types.KeyId as Types
import qualified Network.AWS.ElasticTranscoder.Types.KeyMd5 as Types
import qualified Network.AWS.ElasticTranscoder.Types.LicenseAcquisitionUrl as Types
import qualified Network.AWS.ElasticTranscoder.Types.NonEmptyBase64EncodedString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The PlayReady DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- PlayReady DRM encrypts your media files using @aes-ctr@ encryption.
-- If you use DRM for an @HLSv3@ playlist, your outputs must have a master playlist.
--
-- /See:/ 'mkPlayReadyDrm' smart constructor.
data PlayReadyDrm = PlayReadyDrm'
  { format :: Core.Maybe Types.Format
    -- ^ The type of DRM, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
  , initializationVector :: Core.Maybe Types.InitializationVector
    -- ^ The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your files. The initialization vector must be base64-encoded, and it must be exactly 8 bytes long before being base64-encoded. If no initialization vector is provided, Elastic Transcoder generates one for you.
  , key :: Core.Maybe Types.NonEmptyBase64EncodedString
    -- ^ The DRM key for your file, provided by your DRM license provider. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded:
--
-- @128@ , @192@ , or @256@ . 
-- The key must also be encrypted by using AWS KMS.
  , keyId :: Core.Maybe Types.KeyId
    -- ^ The ID for your DRM key, so that your DRM license provider knows which key to provide.
--
-- The key ID must be provided in big endian, and Elastic Transcoder converts it to little endian before inserting it into the PlayReady DRM headers. If you are unsure whether your license server provides your key ID in big or little endian, check with your DRM provider.
  , keyMd5 :: Core.Maybe Types.KeyMd5
    -- ^ The MD5 digest of the key used for DRM on your file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
  , licenseAcquisitionUrl :: Core.Maybe Types.LicenseAcquisitionUrl
    -- ^ The location of the license key required to play DRM content. The URL must be an absolute path, and is referenced by the PlayReady header. The PlayReady header is referenced in the protection header of the client manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL looks like this: @https://www.example.com/exampleKey/@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlayReadyDrm' value with any optional fields omitted.
mkPlayReadyDrm
    :: PlayReadyDrm
mkPlayReadyDrm
  = PlayReadyDrm'{format = Core.Nothing,
                  initializationVector = Core.Nothing, key = Core.Nothing,
                  keyId = Core.Nothing, keyMd5 = Core.Nothing,
                  licenseAcquisitionUrl = Core.Nothing}

-- | The type of DRM, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdFormat :: Lens.Lens' PlayReadyDrm (Core.Maybe Types.Format)
prdFormat = Lens.field @"format"
{-# INLINEABLE prdFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your files. The initialization vector must be base64-encoded, and it must be exactly 8 bytes long before being base64-encoded. If no initialization vector is provided, Elastic Transcoder generates one for you.
--
-- /Note:/ Consider using 'initializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdInitializationVector :: Lens.Lens' PlayReadyDrm (Core.Maybe Types.InitializationVector)
prdInitializationVector = Lens.field @"initializationVector"
{-# INLINEABLE prdInitializationVector #-}
{-# DEPRECATED initializationVector "Use generic-lens or generic-optics with 'initializationVector' instead"  #-}

-- | The DRM key for your file, provided by your DRM license provider. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded:
--
-- @128@ , @192@ , or @256@ . 
-- The key must also be encrypted by using AWS KMS.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdKey :: Lens.Lens' PlayReadyDrm (Core.Maybe Types.NonEmptyBase64EncodedString)
prdKey = Lens.field @"key"
{-# INLINEABLE prdKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The ID for your DRM key, so that your DRM license provider knows which key to provide.
--
-- The key ID must be provided in big endian, and Elastic Transcoder converts it to little endian before inserting it into the PlayReady DRM headers. If you are unsure whether your license server provides your key ID in big or little endian, check with your DRM provider.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdKeyId :: Lens.Lens' PlayReadyDrm (Core.Maybe Types.KeyId)
prdKeyId = Lens.field @"keyId"
{-# INLINEABLE prdKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The MD5 digest of the key used for DRM on your file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
--
-- /Note:/ Consider using 'keyMd5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdKeyMd5 :: Lens.Lens' PlayReadyDrm (Core.Maybe Types.KeyMd5)
prdKeyMd5 = Lens.field @"keyMd5"
{-# INLINEABLE prdKeyMd5 #-}
{-# DEPRECATED keyMd5 "Use generic-lens or generic-optics with 'keyMd5' instead"  #-}

-- | The location of the license key required to play DRM content. The URL must be an absolute path, and is referenced by the PlayReady header. The PlayReady header is referenced in the protection header of the client manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL looks like this: @https://www.example.com/exampleKey/@ 
--
-- /Note:/ Consider using 'licenseAcquisitionUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdLicenseAcquisitionUrl :: Lens.Lens' PlayReadyDrm (Core.Maybe Types.LicenseAcquisitionUrl)
prdLicenseAcquisitionUrl = Lens.field @"licenseAcquisitionUrl"
{-# INLINEABLE prdLicenseAcquisitionUrl #-}
{-# DEPRECATED licenseAcquisitionUrl "Use generic-lens or generic-optics with 'licenseAcquisitionUrl' instead"  #-}

instance Core.FromJSON PlayReadyDrm where
        toJSON PlayReadyDrm{..}
          = Core.object
              (Core.catMaybes
                 [("Format" Core..=) Core.<$> format,
                  ("InitializationVector" Core..=) Core.<$> initializationVector,
                  ("Key" Core..=) Core.<$> key, ("KeyId" Core..=) Core.<$> keyId,
                  ("KeyMd5" Core..=) Core.<$> keyMd5,
                  ("LicenseAcquisitionUrl" Core..=) Core.<$> licenseAcquisitionUrl])

instance Core.FromJSON PlayReadyDrm where
        parseJSON
          = Core.withObject "PlayReadyDrm" Core.$
              \ x ->
                PlayReadyDrm' Core.<$>
                  (x Core..:? "Format") Core.<*> x Core..:? "InitializationVector"
                    Core.<*> x Core..:? "Key"
                    Core.<*> x Core..:? "KeyId"
                    Core.<*> x Core..:? "KeyMd5"
                    Core.<*> x Core..:? "LicenseAcquisitionUrl"
