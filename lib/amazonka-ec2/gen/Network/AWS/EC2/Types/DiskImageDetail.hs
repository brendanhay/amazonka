{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageDetail
  ( DiskImageDetail (..),

    -- * Smart constructor
    mkDiskImageDetail,

    -- * Lenses
    dFormat,
    dImportManifestURL,
    dBytes,
  )
where

import Network.AWS.EC2.Types.DiskImageFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a disk image.
--
-- /See:/ 'mkDiskImageDetail' smart constructor.
data DiskImageDetail = DiskImageDetail'
  { -- | The disk image format.
    format :: DiskImageFormat,
    -- | A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
    --
    -- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
    importManifestURL :: Lude.Text,
    -- | The size of the disk image, in GiB.
    bytes :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiskImageDetail' with the minimum fields required to make a request.
--
-- * 'format' - The disk image format.
-- * 'importManifestURL' - A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
-- * 'bytes' - The size of the disk image, in GiB.
mkDiskImageDetail ::
  -- | 'format'
  DiskImageFormat ->
  -- | 'importManifestURL'
  Lude.Text ->
  -- | 'bytes'
  Lude.Integer ->
  DiskImageDetail
mkDiskImageDetail pFormat_ pImportManifestURL_ pBytes_ =
  DiskImageDetail'
    { format = pFormat_,
      importManifestURL = pImportManifestURL_,
      bytes = pBytes_
    }

-- | The disk image format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFormat :: Lens.Lens' DiskImageDetail DiskImageFormat
dFormat = Lens.lens (format :: DiskImageDetail -> DiskImageFormat) (\s a -> s {format = a} :: DiskImageDetail)
{-# DEPRECATED dFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
--
-- /Note:/ Consider using 'importManifestURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImportManifestURL :: Lens.Lens' DiskImageDetail Lude.Text
dImportManifestURL = Lens.lens (importManifestURL :: DiskImageDetail -> Lude.Text) (\s a -> s {importManifestURL = a} :: DiskImageDetail)
{-# DEPRECATED dImportManifestURL "Use generic-lens or generic-optics with 'importManifestURL' instead." #-}

-- | The size of the disk image, in GiB.
--
-- /Note:/ Consider using 'bytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBytes :: Lens.Lens' DiskImageDetail Lude.Integer
dBytes = Lens.lens (bytes :: DiskImageDetail -> Lude.Integer) (\s a -> s {bytes = a} :: DiskImageDetail)
{-# DEPRECATED dBytes "Use generic-lens or generic-optics with 'bytes' instead." #-}

instance Lude.ToQuery DiskImageDetail where
  toQuery DiskImageDetail' {..} =
    Lude.mconcat
      [ "Format" Lude.=: format,
        "ImportManifestUrl" Lude.=: importManifestURL,
        "Bytes" Lude.=: bytes
      ]
