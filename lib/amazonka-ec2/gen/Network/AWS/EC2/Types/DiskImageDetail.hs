{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DiskImageDetail
  ( DiskImageDetail (..)
  -- * Smart constructor
  , mkDiskImageDetail
  -- * Lenses
  , dBytes
  , dFormat
  , dImportManifestUrl
  ) where

import qualified Network.AWS.EC2.Types.DiskImageFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a disk image.
--
-- /See:/ 'mkDiskImageDetail' smart constructor.
data DiskImageDetail = DiskImageDetail'
  { bytes :: Core.Integer
    -- ^ The size of the disk image, in GiB.
  , format :: Types.DiskImageFormat
    -- ^ The disk image format.
  , importManifestUrl :: Core.Text
    -- ^ A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiskImageDetail' value with any optional fields omitted.
mkDiskImageDetail
    :: Core.Integer -- ^ 'bytes'
    -> Types.DiskImageFormat -- ^ 'format'
    -> Core.Text -- ^ 'importManifestUrl'
    -> DiskImageDetail
mkDiskImageDetail bytes format importManifestUrl
  = DiskImageDetail'{bytes, format, importManifestUrl}

-- | The size of the disk image, in GiB.
--
-- /Note:/ Consider using 'bytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBytes :: Lens.Lens' DiskImageDetail Core.Integer
dBytes = Lens.field @"bytes"
{-# INLINEABLE dBytes #-}
{-# DEPRECATED bytes "Use generic-lens or generic-optics with 'bytes' instead"  #-}

-- | The disk image format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFormat :: Lens.Lens' DiskImageDetail Types.DiskImageFormat
dFormat = Lens.field @"format"
{-# INLINEABLE dFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
--
-- /Note:/ Consider using 'importManifestUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImportManifestUrl :: Lens.Lens' DiskImageDetail Core.Text
dImportManifestUrl = Lens.field @"importManifestUrl"
{-# INLINEABLE dImportManifestUrl #-}
{-# DEPRECATED importManifestUrl "Use generic-lens or generic-optics with 'importManifestUrl' instead"  #-}

instance Core.ToQuery DiskImageDetail where
        toQuery DiskImageDetail{..}
          = Core.toQueryPair "Bytes" bytes Core.<>
              Core.toQueryPair "Format" format
              Core.<> Core.toQueryPair "ImportManifestUrl" importManifestUrl
