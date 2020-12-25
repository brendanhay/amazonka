{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StorageLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StorageLocation
  ( StorageLocation (..),

    -- * Smart constructor
    mkStorageLocation,

    -- * Lenses
    slBucket,
    slKey,
  )
where

import qualified Network.AWS.EC2.Types.Bucket as Types
import qualified Network.AWS.EC2.Types.Key as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a storage location in Amazon S3.
--
-- /See:/ 'mkStorageLocation' smart constructor.
data StorageLocation = StorageLocation'
  { -- | The name of the S3 bucket.
    bucket :: Core.Maybe Types.Bucket,
    -- | The key.
    key :: Core.Maybe Types.Key
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StorageLocation' value with any optional fields omitted.
mkStorageLocation ::
  StorageLocation
mkStorageLocation =
  StorageLocation' {bucket = Core.Nothing, key = Core.Nothing}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' StorageLocation (Core.Maybe Types.Bucket)
slBucket = Lens.field @"bucket"
{-# DEPRECATED slBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' StorageLocation (Core.Maybe Types.Key)
slKey = Lens.field @"key"
{-# DEPRECATED slKey "Use generic-lens or generic-optics with 'key' instead." #-}
