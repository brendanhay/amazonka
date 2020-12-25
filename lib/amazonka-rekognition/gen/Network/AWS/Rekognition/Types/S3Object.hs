{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.S3Object
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.S3Object
  ( S3Object (..),

    -- * Smart constructor
    mkS3Object,

    -- * Lenses
    soBucket,
    soName,
    soVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Bucket as Types
import qualified Network.AWS.Rekognition.Types.Name as Types
import qualified Network.AWS.Rekognition.Types.S3ObjectVersion as Types

-- | Provides the S3 bucket name and object name.
--
-- The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.
-- For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see Resource-Based Policies in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkS3Object' smart constructor.
data S3Object = S3Object'
  { -- | Name of the S3 bucket.
    bucket :: Core.Maybe Types.Bucket,
    -- | S3 object key name.
    name :: Core.Maybe Types.Name,
    -- | If the bucket is versioning enabled, you can specify the object version.
    version :: Core.Maybe Types.S3ObjectVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Object' value with any optional fields omitted.
mkS3Object ::
  S3Object
mkS3Object =
  S3Object'
    { bucket = Core.Nothing,
      name = Core.Nothing,
      version = Core.Nothing
    }

-- | Name of the S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soBucket :: Lens.Lens' S3Object (Core.Maybe Types.Bucket)
soBucket = Lens.field @"bucket"
{-# DEPRECATED soBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | S3 object key name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soName :: Lens.Lens' S3Object (Core.Maybe Types.Name)
soName = Lens.field @"name"
{-# DEPRECATED soName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If the bucket is versioning enabled, you can specify the object version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soVersion :: Lens.Lens' S3Object (Core.Maybe Types.S3ObjectVersion)
soVersion = Lens.field @"version"
{-# DEPRECATED soVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON S3Object where
  toJSON S3Object {..} =
    Core.object
      ( Core.catMaybes
          [ ("Bucket" Core..=) Core.<$> bucket,
            ("Name" Core..=) Core.<$> name,
            ("Version" Core..=) Core.<$> version
          ]
      )

instance Core.FromJSON S3Object where
  parseJSON =
    Core.withObject "S3Object" Core.$
      \x ->
        S3Object'
          Core.<$> (x Core..:? "Bucket")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Version")
