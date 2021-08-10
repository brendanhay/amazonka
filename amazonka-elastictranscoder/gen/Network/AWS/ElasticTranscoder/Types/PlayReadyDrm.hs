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
-- Module      : Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.PlayReadyDrm where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The PlayReady DRM settings, if any, that you want Elastic Transcoder to
-- apply to the output files associated with this playlist.
--
-- PlayReady DRM encrypts your media files using @aes-ctr@ encryption.
--
-- If you use DRM for an @HLSv3@ playlist, your outputs must have a master
-- playlist.
--
-- /See:/ 'newPlayReadyDrm' smart constructor.
data PlayReadyDrm = PlayReadyDrm'
  { -- | The DRM key for your file, provided by your DRM license provider. The
    -- key must be base64-encoded, and it must be one of the following bit
    -- lengths before being base64-encoded:
    --
    -- @128@, @192@, or @256@.
    --
    -- The key must also be encrypted by using AWS KMS.
    key :: Prelude.Maybe Prelude.Text,
    -- | The location of the license key required to play DRM content. The URL
    -- must be an absolute path, and is referenced by the PlayReady header. The
    -- PlayReady header is referenced in the protection header of the client
    -- manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and
    -- EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL
    -- looks like this: @https:\/\/www.example.com\/exampleKey\/@
    licenseAcquisitionUrl :: Prelude.Maybe Prelude.Text,
    -- | The MD5 digest of the key used for DRM on your file, and that you want
    -- Elastic Transcoder to use as a checksum to make sure your key was not
    -- corrupted in transit. The key MD5 must be base64-encoded, and it must be
    -- exactly 16 bytes before being base64-encoded.
    keyMd5 :: Prelude.Maybe Prelude.Text,
    -- | The type of DRM, if any, that you want Elastic Transcoder to apply to
    -- the output files associated with this playlist.
    format :: Prelude.Maybe Prelude.Text,
    -- | The series of random bits created by a random bit generator, unique for
    -- every encryption operation, that you want Elastic Transcoder to use to
    -- encrypt your files. The initialization vector must be base64-encoded,
    -- and it must be exactly 8 bytes long before being base64-encoded. If no
    -- initialization vector is provided, Elastic Transcoder generates one for
    -- you.
    initializationVector :: Prelude.Maybe Prelude.Text,
    -- | The ID for your DRM key, so that your DRM license provider knows which
    -- key to provide.
    --
    -- The key ID must be provided in big endian, and Elastic Transcoder
    -- converts it to little endian before inserting it into the PlayReady DRM
    -- headers. If you are unsure whether your license server provides your key
    -- ID in big or little endian, check with your DRM provider.
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlayReadyDrm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'playReadyDrm_key' - The DRM key for your file, provided by your DRM license provider. The
-- key must be base64-encoded, and it must be one of the following bit
-- lengths before being base64-encoded:
--
-- @128@, @192@, or @256@.
--
-- The key must also be encrypted by using AWS KMS.
--
-- 'licenseAcquisitionUrl', 'playReadyDrm_licenseAcquisitionUrl' - The location of the license key required to play DRM content. The URL
-- must be an absolute path, and is referenced by the PlayReady header. The
-- PlayReady header is referenced in the protection header of the client
-- manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and
-- EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL
-- looks like this: @https:\/\/www.example.com\/exampleKey\/@
--
-- 'keyMd5', 'playReadyDrm_keyMd5' - The MD5 digest of the key used for DRM on your file, and that you want
-- Elastic Transcoder to use as a checksum to make sure your key was not
-- corrupted in transit. The key MD5 must be base64-encoded, and it must be
-- exactly 16 bytes before being base64-encoded.
--
-- 'format', 'playReadyDrm_format' - The type of DRM, if any, that you want Elastic Transcoder to apply to
-- the output files associated with this playlist.
--
-- 'initializationVector', 'playReadyDrm_initializationVector' - The series of random bits created by a random bit generator, unique for
-- every encryption operation, that you want Elastic Transcoder to use to
-- encrypt your files. The initialization vector must be base64-encoded,
-- and it must be exactly 8 bytes long before being base64-encoded. If no
-- initialization vector is provided, Elastic Transcoder generates one for
-- you.
--
-- 'keyId', 'playReadyDrm_keyId' - The ID for your DRM key, so that your DRM license provider knows which
-- key to provide.
--
-- The key ID must be provided in big endian, and Elastic Transcoder
-- converts it to little endian before inserting it into the PlayReady DRM
-- headers. If you are unsure whether your license server provides your key
-- ID in big or little endian, check with your DRM provider.
newPlayReadyDrm ::
  PlayReadyDrm
newPlayReadyDrm =
  PlayReadyDrm'
    { key = Prelude.Nothing,
      licenseAcquisitionUrl = Prelude.Nothing,
      keyMd5 = Prelude.Nothing,
      format = Prelude.Nothing,
      initializationVector = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | The DRM key for your file, provided by your DRM license provider. The
-- key must be base64-encoded, and it must be one of the following bit
-- lengths before being base64-encoded:
--
-- @128@, @192@, or @256@.
--
-- The key must also be encrypted by using AWS KMS.
playReadyDrm_key :: Lens.Lens' PlayReadyDrm (Prelude.Maybe Prelude.Text)
playReadyDrm_key = Lens.lens (\PlayReadyDrm' {key} -> key) (\s@PlayReadyDrm' {} a -> s {key = a} :: PlayReadyDrm)

-- | The location of the license key required to play DRM content. The URL
-- must be an absolute path, and is referenced by the PlayReady header. The
-- PlayReady header is referenced in the protection header of the client
-- manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and
-- EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL
-- looks like this: @https:\/\/www.example.com\/exampleKey\/@
playReadyDrm_licenseAcquisitionUrl :: Lens.Lens' PlayReadyDrm (Prelude.Maybe Prelude.Text)
playReadyDrm_licenseAcquisitionUrl = Lens.lens (\PlayReadyDrm' {licenseAcquisitionUrl} -> licenseAcquisitionUrl) (\s@PlayReadyDrm' {} a -> s {licenseAcquisitionUrl = a} :: PlayReadyDrm)

-- | The MD5 digest of the key used for DRM on your file, and that you want
-- Elastic Transcoder to use as a checksum to make sure your key was not
-- corrupted in transit. The key MD5 must be base64-encoded, and it must be
-- exactly 16 bytes before being base64-encoded.
playReadyDrm_keyMd5 :: Lens.Lens' PlayReadyDrm (Prelude.Maybe Prelude.Text)
playReadyDrm_keyMd5 = Lens.lens (\PlayReadyDrm' {keyMd5} -> keyMd5) (\s@PlayReadyDrm' {} a -> s {keyMd5 = a} :: PlayReadyDrm)

-- | The type of DRM, if any, that you want Elastic Transcoder to apply to
-- the output files associated with this playlist.
playReadyDrm_format :: Lens.Lens' PlayReadyDrm (Prelude.Maybe Prelude.Text)
playReadyDrm_format = Lens.lens (\PlayReadyDrm' {format} -> format) (\s@PlayReadyDrm' {} a -> s {format = a} :: PlayReadyDrm)

-- | The series of random bits created by a random bit generator, unique for
-- every encryption operation, that you want Elastic Transcoder to use to
-- encrypt your files. The initialization vector must be base64-encoded,
-- and it must be exactly 8 bytes long before being base64-encoded. If no
-- initialization vector is provided, Elastic Transcoder generates one for
-- you.
playReadyDrm_initializationVector :: Lens.Lens' PlayReadyDrm (Prelude.Maybe Prelude.Text)
playReadyDrm_initializationVector = Lens.lens (\PlayReadyDrm' {initializationVector} -> initializationVector) (\s@PlayReadyDrm' {} a -> s {initializationVector = a} :: PlayReadyDrm)

-- | The ID for your DRM key, so that your DRM license provider knows which
-- key to provide.
--
-- The key ID must be provided in big endian, and Elastic Transcoder
-- converts it to little endian before inserting it into the PlayReady DRM
-- headers. If you are unsure whether your license server provides your key
-- ID in big or little endian, check with your DRM provider.
playReadyDrm_keyId :: Lens.Lens' PlayReadyDrm (Prelude.Maybe Prelude.Text)
playReadyDrm_keyId = Lens.lens (\PlayReadyDrm' {keyId} -> keyId) (\s@PlayReadyDrm' {} a -> s {keyId = a} :: PlayReadyDrm)

instance Core.FromJSON PlayReadyDrm where
  parseJSON =
    Core.withObject
      "PlayReadyDrm"
      ( \x ->
          PlayReadyDrm'
            Prelude.<$> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "LicenseAcquisitionUrl")
            Prelude.<*> (x Core..:? "KeyMd5")
            Prelude.<*> (x Core..:? "Format")
            Prelude.<*> (x Core..:? "InitializationVector")
            Prelude.<*> (x Core..:? "KeyId")
      )

instance Prelude.Hashable PlayReadyDrm

instance Prelude.NFData PlayReadyDrm

instance Core.ToJSON PlayReadyDrm where
  toJSON PlayReadyDrm' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("LicenseAcquisitionUrl" Core..=)
              Prelude.<$> licenseAcquisitionUrl,
            ("KeyMd5" Core..=) Prelude.<$> keyMd5,
            ("Format" Core..=) Prelude.<$> format,
            ("InitializationVector" Core..=)
              Prelude.<$> initializationVector,
            ("KeyId" Core..=) Prelude.<$> keyId
          ]
      )
