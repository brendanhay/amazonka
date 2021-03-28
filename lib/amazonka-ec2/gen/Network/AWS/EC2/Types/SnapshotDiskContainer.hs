{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotDiskContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SnapshotDiskContainer
  ( SnapshotDiskContainer (..)
  -- * Smart constructor
  , mkSnapshotDiskContainer
  -- * Lenses
  , sdcDescription
  , sdcFormat
  , sdcUrl
  , sdcUserBucket
  ) where

import qualified Network.AWS.EC2.Types.UserBucket as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The disk container object for the import snapshot request.
--
-- /See:/ 'mkSnapshotDiskContainer' smart constructor.
data SnapshotDiskContainer = SnapshotDiskContainer'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the disk image being imported.
  , format :: Core.Maybe Core.Text
    -- ^ The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@ 
  , url :: Core.Maybe Core.Text
    -- ^ The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
  , userBucket :: Core.Maybe Types.UserBucket
    -- ^ The Amazon S3 bucket for the disk image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotDiskContainer' value with any optional fields omitted.
mkSnapshotDiskContainer
    :: SnapshotDiskContainer
mkSnapshotDiskContainer
  = SnapshotDiskContainer'{description = Core.Nothing,
                           format = Core.Nothing, url = Core.Nothing,
                           userBucket = Core.Nothing}

-- | The description of the disk image being imported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcDescription :: Lens.Lens' SnapshotDiskContainer (Core.Maybe Core.Text)
sdcDescription = Lens.field @"description"
{-# INLINEABLE sdcDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@ 
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcFormat :: Lens.Lens' SnapshotDiskContainer (Core.Maybe Core.Text)
sdcFormat = Lens.field @"format"
{-# INLINEABLE sdcFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcUrl :: Lens.Lens' SnapshotDiskContainer (Core.Maybe Core.Text)
sdcUrl = Lens.field @"url"
{-# INLINEABLE sdcUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The Amazon S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcUserBucket :: Lens.Lens' SnapshotDiskContainer (Core.Maybe Types.UserBucket)
sdcUserBucket = Lens.field @"userBucket"
{-# INLINEABLE sdcUserBucket #-}
{-# DEPRECATED userBucket "Use generic-lens or generic-optics with 'userBucket' instead"  #-}

instance Core.ToQuery SnapshotDiskContainer where
        toQuery SnapshotDiskContainer{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Description")
              description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Format") format
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Url") url
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserBucket") userBucket
