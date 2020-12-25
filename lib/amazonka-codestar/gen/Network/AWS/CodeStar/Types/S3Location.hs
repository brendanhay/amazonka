{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slBucketKey,
    slBucketName,
  )
where

import qualified Network.AWS.CodeStar.Types.BucketKey as Types
import qualified Network.AWS.CodeStar.Types.BucketName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon S3 location where the source code files provided with the project request are stored.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The Amazon S3 object key where the source code files provided with the project request are stored.
    bucketKey :: Core.Maybe Types.BucketKey,
    -- | The Amazon S3 bucket name where the source code files provided with the project request are stored.
    bucketName :: Core.Maybe Types.BucketName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Location' value with any optional fields omitted.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location' {bucketKey = Core.Nothing, bucketName = Core.Nothing}

-- | The Amazon S3 object key where the source code files provided with the project request are stored.
--
-- /Note:/ Consider using 'bucketKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucketKey :: Lens.Lens' S3Location (Core.Maybe Types.BucketKey)
slBucketKey = Lens.field @"bucketKey"
{-# DEPRECATED slBucketKey "Use generic-lens or generic-optics with 'bucketKey' instead." #-}

-- | The Amazon S3 bucket name where the source code files provided with the project request are stored.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucketName :: Lens.Lens' S3Location (Core.Maybe Types.BucketName)
slBucketName = Lens.field @"bucketName"
{-# DEPRECATED slBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

instance Core.FromJSON S3Location where
  toJSON S3Location {..} =
    Core.object
      ( Core.catMaybes
          [ ("bucketKey" Core..=) Core.<$> bucketKey,
            ("bucketName" Core..=) Core.<$> bucketName
          ]
      )
