{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.HlsContentProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.HlsContentProtection
  ( HlsContentProtection (..)
  -- * Smart constructor
  , mkHlsContentProtection
  -- * Lenses
  , hcpInitializationVector
  , hcpKey
  , hcpKeyMd5
  , hcpKeyStoragePolicy
  , hcpLicenseAcquisitionUrl
  , hcpMethod
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.HlsContentProtectionMethod as Types
import qualified Network.AWS.ElasticTranscoder.Types.InitializationVector as Types
import qualified Network.AWS.ElasticTranscoder.Types.Key as Types
import qualified Network.AWS.ElasticTranscoder.Types.KeyMd5 as Types
import qualified Network.AWS.ElasticTranscoder.Types.KeyStoragePolicy as Types
import qualified Network.AWS.ElasticTranscoder.Types.ZeroTo512String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to your output files.
--
-- /See:/ 'mkHlsContentProtection' smart constructor.
data HlsContentProtection = HlsContentProtection'
  { initializationVector :: Core.Maybe Types.InitializationVector
    -- ^ If Elastic Transcoder is generating your key for you, you must leave this field blank.
--
-- The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
  , key :: Core.Maybe Types.Key
    -- ^ If you want Elastic Transcoder to generate a key for you, leave this field blank.
--
-- If you choose to supply your own key, you must encrypt the key by using AWS KMS. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded:
-- @128@ , @192@ , or @256@ . 
  , keyMd5 :: Core.Maybe Types.KeyMd5
    -- ^ If Elastic Transcoder is generating your key for you, you must leave this field blank.
--
-- The MD5 digest of the key that you want Elastic Transcoder to use to encrypt your output file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64- encoded.
  , keyStoragePolicy :: Core.Maybe Types.KeyStoragePolicy
    -- ^ Specify whether you want Elastic Transcoder to write your HLS license key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@ , @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes your data key into the same bucket as the associated playlist.
  , licenseAcquisitionUrl :: Core.Maybe Types.ZeroTo512String
    -- ^ The location of the license key required to decrypt your HLS playlist. The URL must be an absolute path, and is referenced in the URI attribute of the EXT-X-KEY metadata tag in the playlist file.
  , method :: Core.Maybe Types.HlsContentProtectionMethod
    -- ^ The content protection method for your output. The only valid value is: @aes-128@ .
--
-- This value is written into the method attribute of the @EXT-X-KEY@ metadata tag in the output playlist.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsContentProtection' value with any optional fields omitted.
mkHlsContentProtection
    :: HlsContentProtection
mkHlsContentProtection
  = HlsContentProtection'{initializationVector = Core.Nothing,
                          key = Core.Nothing, keyMd5 = Core.Nothing,
                          keyStoragePolicy = Core.Nothing,
                          licenseAcquisitionUrl = Core.Nothing, method = Core.Nothing}

-- | If Elastic Transcoder is generating your key for you, you must leave this field blank.
--
-- The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
--
-- /Note:/ Consider using 'initializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpInitializationVector :: Lens.Lens' HlsContentProtection (Core.Maybe Types.InitializationVector)
hcpInitializationVector = Lens.field @"initializationVector"
{-# INLINEABLE hcpInitializationVector #-}
{-# DEPRECATED initializationVector "Use generic-lens or generic-optics with 'initializationVector' instead"  #-}

-- | If you want Elastic Transcoder to generate a key for you, leave this field blank.
--
-- If you choose to supply your own key, you must encrypt the key by using AWS KMS. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded:
-- @128@ , @192@ , or @256@ . 
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpKey :: Lens.Lens' HlsContentProtection (Core.Maybe Types.Key)
hcpKey = Lens.field @"key"
{-# INLINEABLE hcpKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | If Elastic Transcoder is generating your key for you, you must leave this field blank.
--
-- The MD5 digest of the key that you want Elastic Transcoder to use to encrypt your output file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64- encoded.
--
-- /Note:/ Consider using 'keyMd5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpKeyMd5 :: Lens.Lens' HlsContentProtection (Core.Maybe Types.KeyMd5)
hcpKeyMd5 = Lens.field @"keyMd5"
{-# INLINEABLE hcpKeyMd5 #-}
{-# DEPRECATED keyMd5 "Use generic-lens or generic-optics with 'keyMd5' instead"  #-}

-- | Specify whether you want Elastic Transcoder to write your HLS license key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@ , @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes your data key into the same bucket as the associated playlist.
--
-- /Note:/ Consider using 'keyStoragePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpKeyStoragePolicy :: Lens.Lens' HlsContentProtection (Core.Maybe Types.KeyStoragePolicy)
hcpKeyStoragePolicy = Lens.field @"keyStoragePolicy"
{-# INLINEABLE hcpKeyStoragePolicy #-}
{-# DEPRECATED keyStoragePolicy "Use generic-lens or generic-optics with 'keyStoragePolicy' instead"  #-}

-- | The location of the license key required to decrypt your HLS playlist. The URL must be an absolute path, and is referenced in the URI attribute of the EXT-X-KEY metadata tag in the playlist file.
--
-- /Note:/ Consider using 'licenseAcquisitionUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpLicenseAcquisitionUrl :: Lens.Lens' HlsContentProtection (Core.Maybe Types.ZeroTo512String)
hcpLicenseAcquisitionUrl = Lens.field @"licenseAcquisitionUrl"
{-# INLINEABLE hcpLicenseAcquisitionUrl #-}
{-# DEPRECATED licenseAcquisitionUrl "Use generic-lens or generic-optics with 'licenseAcquisitionUrl' instead"  #-}

-- | The content protection method for your output. The only valid value is: @aes-128@ .
--
-- This value is written into the method attribute of the @EXT-X-KEY@ metadata tag in the output playlist.
--
-- /Note:/ Consider using 'method' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcpMethod :: Lens.Lens' HlsContentProtection (Core.Maybe Types.HlsContentProtectionMethod)
hcpMethod = Lens.field @"method"
{-# INLINEABLE hcpMethod #-}
{-# DEPRECATED method "Use generic-lens or generic-optics with 'method' instead"  #-}

instance Core.FromJSON HlsContentProtection where
        toJSON HlsContentProtection{..}
          = Core.object
              (Core.catMaybes
                 [("InitializationVector" Core..=) Core.<$> initializationVector,
                  ("Key" Core..=) Core.<$> key, ("KeyMd5" Core..=) Core.<$> keyMd5,
                  ("KeyStoragePolicy" Core..=) Core.<$> keyStoragePolicy,
                  ("LicenseAcquisitionUrl" Core..=) Core.<$> licenseAcquisitionUrl,
                  ("Method" Core..=) Core.<$> method])

instance Core.FromJSON HlsContentProtection where
        parseJSON
          = Core.withObject "HlsContentProtection" Core.$
              \ x ->
                HlsContentProtection' Core.<$>
                  (x Core..:? "InitializationVector") Core.<*> x Core..:? "Key"
                    Core.<*> x Core..:? "KeyMd5"
                    Core.<*> x Core..:? "KeyStoragePolicy"
                    Core.<*> x Core..:? "LicenseAcquisitionUrl"
                    Core.<*> x Core..:? "Method"
