{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryDestination
  ( InventoryDestination (..),

    -- * Smart constructor
    mkInventoryDestination,

    -- * Lenses
    idS3BucketDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.InventoryS3BucketDestination as Types

-- | Specifies the inventory configuration for an Amazon S3 bucket.
--
-- /See:/ 'mkInventoryDestination' smart constructor.
newtype InventoryDestination = InventoryDestination'
  { -- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
    s3BucketDestination :: Types.InventoryS3BucketDestination
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryDestination' value with any optional fields omitted.
mkInventoryDestination ::
  -- | 's3BucketDestination'
  Types.InventoryS3BucketDestination ->
  InventoryDestination
mkInventoryDestination s3BucketDestination =
  InventoryDestination' {s3BucketDestination}

-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
--
-- /Note:/ Consider using 's3BucketDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idS3BucketDestination :: Lens.Lens' InventoryDestination Types.InventoryS3BucketDestination
idS3BucketDestination = Lens.field @"s3BucketDestination"
{-# DEPRECATED idS3BucketDestination "Use generic-lens or generic-optics with 's3BucketDestination' instead." #-}

instance Core.ToXML InventoryDestination where
  toXML InventoryDestination {..} =
    Core.toXMLNode "S3BucketDestination" s3BucketDestination

instance Core.FromXML InventoryDestination where
  parseXML x =
    InventoryDestination' Core.<$> (x Core..@ "S3BucketDestination")
