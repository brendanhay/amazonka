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
-- Module      : Network.AWS.ElasticTranscoder.Types.HlsContentProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.HlsContentProtection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The HLS content protection settings, if any, that you want Elastic
-- Transcoder to apply to your output files.
--
-- /See:/ 'newHlsContentProtection' smart constructor.
data HlsContentProtection = HlsContentProtection'
  { -- | If you want Elastic Transcoder to generate a key for you, leave this
    -- field blank.
    --
    -- If you choose to supply your own key, you must encrypt the key by using
    -- AWS KMS. The key must be base64-encoded, and it must be one of the
    -- following bit lengths before being base64-encoded:
    --
    -- @128@, @192@, or @256@.
    key :: Core.Maybe Core.Text,
    -- | The location of the license key required to decrypt your HLS playlist.
    -- The URL must be an absolute path, and is referenced in the URI attribute
    -- of the EXT-X-KEY metadata tag in the playlist file.
    licenseAcquisitionUrl :: Core.Maybe Core.Text,
    -- | If Elastic Transcoder is generating your key for you, you must leave
    -- this field blank.
    --
    -- The MD5 digest of the key that you want Elastic Transcoder to use to
    -- encrypt your output file, and that you want Elastic Transcoder to use as
    -- a checksum to make sure your key was not corrupted in transit. The key
    -- MD5 must be base64-encoded, and it must be exactly 16 bytes before being
    -- base64- encoded.
    keyMd5 :: Core.Maybe Core.Text,
    -- | The content protection method for your output. The only valid value is:
    -- @aes-128@.
    --
    -- This value is written into the method attribute of the @EXT-X-KEY@
    -- metadata tag in the output playlist.
    method :: Core.Maybe Core.Text,
    -- | If Elastic Transcoder is generating your key for you, you must leave
    -- this field blank.
    --
    -- The series of random bits created by a random bit generator, unique for
    -- every encryption operation, that you want Elastic Transcoder to use to
    -- encrypt your output files. The initialization vector must be
    -- base64-encoded, and it must be exactly 16 bytes before being
    -- base64-encoded.
    initializationVector :: Core.Maybe Core.Text,
    -- | Specify whether you want Elastic Transcoder to write your HLS license
    -- key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@,
    -- @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes
    -- your data key into the same bucket as the associated playlist.
    keyStoragePolicy :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsContentProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'hlsContentProtection_key' - If you want Elastic Transcoder to generate a key for you, leave this
-- field blank.
--
-- If you choose to supply your own key, you must encrypt the key by using
-- AWS KMS. The key must be base64-encoded, and it must be one of the
-- following bit lengths before being base64-encoded:
--
-- @128@, @192@, or @256@.
--
-- 'licenseAcquisitionUrl', 'hlsContentProtection_licenseAcquisitionUrl' - The location of the license key required to decrypt your HLS playlist.
-- The URL must be an absolute path, and is referenced in the URI attribute
-- of the EXT-X-KEY metadata tag in the playlist file.
--
-- 'keyMd5', 'hlsContentProtection_keyMd5' - If Elastic Transcoder is generating your key for you, you must leave
-- this field blank.
--
-- The MD5 digest of the key that you want Elastic Transcoder to use to
-- encrypt your output file, and that you want Elastic Transcoder to use as
-- a checksum to make sure your key was not corrupted in transit. The key
-- MD5 must be base64-encoded, and it must be exactly 16 bytes before being
-- base64- encoded.
--
-- 'method', 'hlsContentProtection_method' - The content protection method for your output. The only valid value is:
-- @aes-128@.
--
-- This value is written into the method attribute of the @EXT-X-KEY@
-- metadata tag in the output playlist.
--
-- 'initializationVector', 'hlsContentProtection_initializationVector' - If Elastic Transcoder is generating your key for you, you must leave
-- this field blank.
--
-- The series of random bits created by a random bit generator, unique for
-- every encryption operation, that you want Elastic Transcoder to use to
-- encrypt your output files. The initialization vector must be
-- base64-encoded, and it must be exactly 16 bytes before being
-- base64-encoded.
--
-- 'keyStoragePolicy', 'hlsContentProtection_keyStoragePolicy' - Specify whether you want Elastic Transcoder to write your HLS license
-- key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@,
-- @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes
-- your data key into the same bucket as the associated playlist.
newHlsContentProtection ::
  HlsContentProtection
newHlsContentProtection =
  HlsContentProtection'
    { key = Core.Nothing,
      licenseAcquisitionUrl = Core.Nothing,
      keyMd5 = Core.Nothing,
      method = Core.Nothing,
      initializationVector = Core.Nothing,
      keyStoragePolicy = Core.Nothing
    }

-- | If you want Elastic Transcoder to generate a key for you, leave this
-- field blank.
--
-- If you choose to supply your own key, you must encrypt the key by using
-- AWS KMS. The key must be base64-encoded, and it must be one of the
-- following bit lengths before being base64-encoded:
--
-- @128@, @192@, or @256@.
hlsContentProtection_key :: Lens.Lens' HlsContentProtection (Core.Maybe Core.Text)
hlsContentProtection_key = Lens.lens (\HlsContentProtection' {key} -> key) (\s@HlsContentProtection' {} a -> s {key = a} :: HlsContentProtection)

-- | The location of the license key required to decrypt your HLS playlist.
-- The URL must be an absolute path, and is referenced in the URI attribute
-- of the EXT-X-KEY metadata tag in the playlist file.
hlsContentProtection_licenseAcquisitionUrl :: Lens.Lens' HlsContentProtection (Core.Maybe Core.Text)
hlsContentProtection_licenseAcquisitionUrl = Lens.lens (\HlsContentProtection' {licenseAcquisitionUrl} -> licenseAcquisitionUrl) (\s@HlsContentProtection' {} a -> s {licenseAcquisitionUrl = a} :: HlsContentProtection)

-- | If Elastic Transcoder is generating your key for you, you must leave
-- this field blank.
--
-- The MD5 digest of the key that you want Elastic Transcoder to use to
-- encrypt your output file, and that you want Elastic Transcoder to use as
-- a checksum to make sure your key was not corrupted in transit. The key
-- MD5 must be base64-encoded, and it must be exactly 16 bytes before being
-- base64- encoded.
hlsContentProtection_keyMd5 :: Lens.Lens' HlsContentProtection (Core.Maybe Core.Text)
hlsContentProtection_keyMd5 = Lens.lens (\HlsContentProtection' {keyMd5} -> keyMd5) (\s@HlsContentProtection' {} a -> s {keyMd5 = a} :: HlsContentProtection)

-- | The content protection method for your output. The only valid value is:
-- @aes-128@.
--
-- This value is written into the method attribute of the @EXT-X-KEY@
-- metadata tag in the output playlist.
hlsContentProtection_method :: Lens.Lens' HlsContentProtection (Core.Maybe Core.Text)
hlsContentProtection_method = Lens.lens (\HlsContentProtection' {method} -> method) (\s@HlsContentProtection' {} a -> s {method = a} :: HlsContentProtection)

-- | If Elastic Transcoder is generating your key for you, you must leave
-- this field blank.
--
-- The series of random bits created by a random bit generator, unique for
-- every encryption operation, that you want Elastic Transcoder to use to
-- encrypt your output files. The initialization vector must be
-- base64-encoded, and it must be exactly 16 bytes before being
-- base64-encoded.
hlsContentProtection_initializationVector :: Lens.Lens' HlsContentProtection (Core.Maybe Core.Text)
hlsContentProtection_initializationVector = Lens.lens (\HlsContentProtection' {initializationVector} -> initializationVector) (\s@HlsContentProtection' {} a -> s {initializationVector = a} :: HlsContentProtection)

-- | Specify whether you want Elastic Transcoder to write your HLS license
-- key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@,
-- @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes
-- your data key into the same bucket as the associated playlist.
hlsContentProtection_keyStoragePolicy :: Lens.Lens' HlsContentProtection (Core.Maybe Core.Text)
hlsContentProtection_keyStoragePolicy = Lens.lens (\HlsContentProtection' {keyStoragePolicy} -> keyStoragePolicy) (\s@HlsContentProtection' {} a -> s {keyStoragePolicy = a} :: HlsContentProtection)

instance Core.FromJSON HlsContentProtection where
  parseJSON =
    Core.withObject
      "HlsContentProtection"
      ( \x ->
          HlsContentProtection'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "LicenseAcquisitionUrl")
            Core.<*> (x Core..:? "KeyMd5")
            Core.<*> (x Core..:? "Method")
            Core.<*> (x Core..:? "InitializationVector")
            Core.<*> (x Core..:? "KeyStoragePolicy")
      )

instance Core.Hashable HlsContentProtection

instance Core.NFData HlsContentProtection

instance Core.ToJSON HlsContentProtection where
  toJSON HlsContentProtection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("LicenseAcquisitionUrl" Core..=)
              Core.<$> licenseAcquisitionUrl,
            ("KeyMd5" Core..=) Core.<$> keyMd5,
            ("Method" Core..=) Core.<$> method,
            ("InitializationVector" Core..=)
              Core.<$> initializationVector,
            ("KeyStoragePolicy" Core..=)
              Core.<$> keyStoragePolicy
          ]
      )
