{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageDescription
  ( DiskImageDescription (..),

    -- * Smart constructor
    mkDiskImageDescription,

    -- * Lenses
    didSize,
    didChecksum,
    didFormat,
    didImportManifestURL,
  )
where

import Network.AWS.EC2.Types.DiskImageFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a disk image.
--
-- /See:/ 'mkDiskImageDescription' smart constructor.
data DiskImageDescription = DiskImageDescription'
  { -- | The size of the disk image, in GiB.
    size :: Lude.Maybe Lude.Integer,
    -- | The checksum computed for the disk image.
    checksum :: Lude.Maybe Lude.Text,
    -- | The disk image format.
    format :: Lude.Maybe DiskImageFormat,
    -- | A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
    --
    -- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
    importManifestURL :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiskImageDescription' with the minimum fields required to make a request.
--
-- * 'size' - The size of the disk image, in GiB.
-- * 'checksum' - The checksum computed for the disk image.
-- * 'format' - The disk image format.
-- * 'importManifestURL' - A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
mkDiskImageDescription ::
  DiskImageDescription
mkDiskImageDescription =
  DiskImageDescription'
    { size = Lude.Nothing,
      checksum = Lude.Nothing,
      format = Lude.Nothing,
      importManifestURL = Lude.Nothing
    }

-- | The size of the disk image, in GiB.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didSize :: Lens.Lens' DiskImageDescription (Lude.Maybe Lude.Integer)
didSize = Lens.lens (size :: DiskImageDescription -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: DiskImageDescription)
{-# DEPRECATED didSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The checksum computed for the disk image.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didChecksum :: Lens.Lens' DiskImageDescription (Lude.Maybe Lude.Text)
didChecksum = Lens.lens (checksum :: DiskImageDescription -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: DiskImageDescription)
{-# DEPRECATED didChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The disk image format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didFormat :: Lens.Lens' DiskImageDescription (Lude.Maybe DiskImageFormat)
didFormat = Lens.lens (format :: DiskImageDescription -> Lude.Maybe DiskImageFormat) (\s a -> s {format = a} :: DiskImageDescription)
{-# DEPRECATED didFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
--
-- /Note:/ Consider using 'importManifestURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didImportManifestURL :: Lens.Lens' DiskImageDescription (Lude.Maybe Lude.Text)
didImportManifestURL = Lens.lens (importManifestURL :: DiskImageDescription -> Lude.Maybe Lude.Text) (\s a -> s {importManifestURL = a} :: DiskImageDescription)
{-# DEPRECATED didImportManifestURL "Use generic-lens or generic-optics with 'importManifestURL' instead." #-}

instance Lude.FromXML DiskImageDescription where
  parseXML x =
    DiskImageDescription'
      Lude.<$> (x Lude..@? "size")
      Lude.<*> (x Lude..@? "checksum")
      Lude.<*> (x Lude..@? "format")
      Lude.<*> (x Lude..@? "importManifestUrl")
