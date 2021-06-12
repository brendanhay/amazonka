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
-- Module      : Network.AWS.EC2.Types.DiskImageDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageFormat
import qualified Network.AWS.Lens as Lens

-- | Describes a disk image.
--
-- /See:/ 'newDiskImageDescription' smart constructor.
data DiskImageDescription = DiskImageDescription'
  { -- | The disk image format.
    format :: Core.Maybe DiskImageFormat,
    -- | A presigned URL for the import manifest stored in Amazon S3. For
    -- information about creating a presigned URL for an Amazon S3 object, read
    -- the \"Query String Request Authentication Alternative\" section of the
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests>
    -- topic in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- For information about the import manifest referenced by this API action,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
    importManifestUrl :: Core.Maybe Core.Text,
    -- | The checksum computed for the disk image.
    checksum :: Core.Maybe Core.Text,
    -- | The size of the disk image, in GiB.
    size :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiskImageDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'diskImageDescription_format' - The disk image format.
--
-- 'importManifestUrl', 'diskImageDescription_importManifestUrl' - A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the \"Query String Request Authentication Alternative\" section of the
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests>
-- topic in the /Amazon Simple Storage Service Developer Guide/.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
--
-- 'checksum', 'diskImageDescription_checksum' - The checksum computed for the disk image.
--
-- 'size', 'diskImageDescription_size' - The size of the disk image, in GiB.
newDiskImageDescription ::
  DiskImageDescription
newDiskImageDescription =
  DiskImageDescription'
    { format = Core.Nothing,
      importManifestUrl = Core.Nothing,
      checksum = Core.Nothing,
      size = Core.Nothing
    }

-- | The disk image format.
diskImageDescription_format :: Lens.Lens' DiskImageDescription (Core.Maybe DiskImageFormat)
diskImageDescription_format = Lens.lens (\DiskImageDescription' {format} -> format) (\s@DiskImageDescription' {} a -> s {format = a} :: DiskImageDescription)

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the \"Query String Request Authentication Alternative\" section of the
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests>
-- topic in the /Amazon Simple Storage Service Developer Guide/.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
diskImageDescription_importManifestUrl :: Lens.Lens' DiskImageDescription (Core.Maybe Core.Text)
diskImageDescription_importManifestUrl = Lens.lens (\DiskImageDescription' {importManifestUrl} -> importManifestUrl) (\s@DiskImageDescription' {} a -> s {importManifestUrl = a} :: DiskImageDescription)

-- | The checksum computed for the disk image.
diskImageDescription_checksum :: Lens.Lens' DiskImageDescription (Core.Maybe Core.Text)
diskImageDescription_checksum = Lens.lens (\DiskImageDescription' {checksum} -> checksum) (\s@DiskImageDescription' {} a -> s {checksum = a} :: DiskImageDescription)

-- | The size of the disk image, in GiB.
diskImageDescription_size :: Lens.Lens' DiskImageDescription (Core.Maybe Core.Integer)
diskImageDescription_size = Lens.lens (\DiskImageDescription' {size} -> size) (\s@DiskImageDescription' {} a -> s {size = a} :: DiskImageDescription)

instance Core.FromXML DiskImageDescription where
  parseXML x =
    DiskImageDescription'
      Core.<$> (x Core..@? "format")
      Core.<*> (x Core..@? "importManifestUrl")
      Core.<*> (x Core..@? "checksum")
      Core.<*> (x Core..@? "size")

instance Core.Hashable DiskImageDescription

instance Core.NFData DiskImageDescription
