{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DiskImageDescription
  ( DiskImageDescription (..)
  -- * Smart constructor
  , mkDiskImageDescription
  -- * Lenses
  , didChecksum
  , didFormat
  , didImportManifestUrl
  , didSize
  ) where

import qualified Network.AWS.EC2.Types.DiskImageFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a disk image.
--
-- /See:/ 'mkDiskImageDescription' smart constructor.
data DiskImageDescription = DiskImageDescription'
  { checksum :: Core.Maybe Core.Text
    -- ^ The checksum computed for the disk image.
  , format :: Core.Maybe Types.DiskImageFormat
    -- ^ The disk image format.
  , importManifestUrl :: Core.Maybe Core.Text
    -- ^ A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
  , size :: Core.Maybe Core.Integer
    -- ^ The size of the disk image, in GiB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiskImageDescription' value with any optional fields omitted.
mkDiskImageDescription
    :: DiskImageDescription
mkDiskImageDescription
  = DiskImageDescription'{checksum = Core.Nothing,
                          format = Core.Nothing, importManifestUrl = Core.Nothing,
                          size = Core.Nothing}

-- | The checksum computed for the disk image.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didChecksum :: Lens.Lens' DiskImageDescription (Core.Maybe Core.Text)
didChecksum = Lens.field @"checksum"
{-# INLINEABLE didChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | The disk image format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didFormat :: Lens.Lens' DiskImageDescription (Core.Maybe Types.DiskImageFormat)
didFormat = Lens.field @"format"
{-# INLINEABLE didFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
--
-- /Note:/ Consider using 'importManifestUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didImportManifestUrl :: Lens.Lens' DiskImageDescription (Core.Maybe Core.Text)
didImportManifestUrl = Lens.field @"importManifestUrl"
{-# INLINEABLE didImportManifestUrl #-}
{-# DEPRECATED importManifestUrl "Use generic-lens or generic-optics with 'importManifestUrl' instead"  #-}

-- | The size of the disk image, in GiB.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didSize :: Lens.Lens' DiskImageDescription (Core.Maybe Core.Integer)
didSize = Lens.field @"size"
{-# INLINEABLE didSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

instance Core.FromXML DiskImageDescription where
        parseXML x
          = DiskImageDescription' Core.<$>
              (x Core..@? "checksum") Core.<*> x Core..@? "format" Core.<*>
                x Core..@? "importManifestUrl"
                Core.<*> x Core..@? "size"
