{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.S3Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.S3Resource
  ( S3Resource (..)
  -- * Smart constructor
  , mkS3Resource
  -- * Lenses
  , srBucketArn
  , srKeyRange
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.BucketArn as Types
import qualified Network.AWS.Snowball.Types.KeyRange as Types

-- | Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into. For export jobs, this object can have an optional @KeyRange@ value. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
-- /See:/ 'mkS3Resource' smart constructor.
data S3Resource = S3Resource'
  { bucketArn :: Core.Maybe Types.BucketArn
    -- ^ The Amazon Resource Name (ARN) of an Amazon S3 bucket.
  , keyRange :: Core.Maybe Types.KeyRange
    -- ^ For export jobs, you can provide an optional @KeyRange@ within a specific Amazon S3 bucket. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Resource' value with any optional fields omitted.
mkS3Resource
    :: S3Resource
mkS3Resource
  = S3Resource'{bucketArn = Core.Nothing, keyRange = Core.Nothing}

-- | The Amazon Resource Name (ARN) of an Amazon S3 bucket.
--
-- /Note:/ Consider using 'bucketArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srBucketArn :: Lens.Lens' S3Resource (Core.Maybe Types.BucketArn)
srBucketArn = Lens.field @"bucketArn"
{-# INLINEABLE srBucketArn #-}
{-# DEPRECATED bucketArn "Use generic-lens or generic-optics with 'bucketArn' instead"  #-}

-- | For export jobs, you can provide an optional @KeyRange@ within a specific Amazon S3 bucket. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
-- /Note:/ Consider using 'keyRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srKeyRange :: Lens.Lens' S3Resource (Core.Maybe Types.KeyRange)
srKeyRange = Lens.field @"keyRange"
{-# INLINEABLE srKeyRange #-}
{-# DEPRECATED keyRange "Use generic-lens or generic-optics with 'keyRange' instead"  #-}

instance Core.FromJSON S3Resource where
        toJSON S3Resource{..}
          = Core.object
              (Core.catMaybes
                 [("BucketArn" Core..=) Core.<$> bucketArn,
                  ("KeyRange" Core..=) Core.<$> keyRange])

instance Core.FromJSON S3Resource where
        parseJSON
          = Core.withObject "S3Resource" Core.$
              \ x ->
                S3Resource' Core.<$>
                  (x Core..:? "BucketArn") Core.<*> x Core..:? "KeyRange"
