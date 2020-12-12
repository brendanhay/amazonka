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
    didBytes,
    didFormat,
    didImportManifestURL,
  )
where

import Network.AWS.EC2.Types.DiskImageFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a disk image.
--
-- /See:/ 'mkDiskImageDetail' smart constructor.
data DiskImageDetail = DiskImageDetail'
  { bytes :: Lude.Integer,
    format :: DiskImageFormat,
    importManifestURL :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiskImageDetail' with the minimum fields required to make a request.
--
-- * 'bytes' - The size of the disk image, in GiB.
-- * 'format' - The disk image format.
-- * 'importManifestURL' - A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
mkDiskImageDetail ::
  -- | 'bytes'
  Lude.Integer ->
  -- | 'format'
  DiskImageFormat ->
  -- | 'importManifestURL'
  Lude.Text ->
  DiskImageDetail
mkDiskImageDetail pBytes_ pFormat_ pImportManifestURL_ =
  DiskImageDetail'
    { bytes = pBytes_,
      format = pFormat_,
      importManifestURL = pImportManifestURL_
    }

-- | The size of the disk image, in GiB.
--
-- /Note:/ Consider using 'bytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didBytes :: Lens.Lens' DiskImageDetail Lude.Integer
didBytes = Lens.lens (bytes :: DiskImageDetail -> Lude.Integer) (\s a -> s {bytes = a} :: DiskImageDetail)
{-# DEPRECATED didBytes "Use generic-lens or generic-optics with 'bytes' instead." #-}

-- | The disk image format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didFormat :: Lens.Lens' DiskImageDetail DiskImageFormat
didFormat = Lens.lens (format :: DiskImageDetail -> DiskImageFormat) (\s a -> s {format = a} :: DiskImageDetail)
{-# DEPRECATED didFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
--
-- /Note:/ Consider using 'importManifestURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didImportManifestURL :: Lens.Lens' DiskImageDetail Lude.Text
didImportManifestURL = Lens.lens (importManifestURL :: DiskImageDetail -> Lude.Text) (\s a -> s {importManifestURL = a} :: DiskImageDetail)
{-# DEPRECATED didImportManifestURL "Use generic-lens or generic-optics with 'importManifestURL' instead." #-}

instance Lude.ToQuery DiskImageDetail where
  toQuery DiskImageDetail' {..} =
    Lude.mconcat
      [ "Bytes" Lude.=: bytes,
        "Format" Lude.=: format,
        "ImportManifestUrl" Lude.=: importManifestURL
      ]
