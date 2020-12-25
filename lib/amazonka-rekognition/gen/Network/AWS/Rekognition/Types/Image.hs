{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Image
  ( Image (..),

    -- * Smart constructor
    mkImage,

    -- * Lenses
    iBytes,
    iS3Object,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.S3Object as Types

-- | Provides the input image either as bytes or an S3 object.
--
-- You pass image bytes to an Amazon Rekognition API operation by using the @Bytes@ property. For example, you would use the @Bytes@ property to pass an image loaded from a local file system. Image bytes passed by using the @Bytes@ property must be base64-encoded. Your code may not need to encode image bytes if you are using an AWS SDK to call Amazon Rekognition API operations.
-- For more information, see Analyzing an Image Loaded from a Local File System in the Amazon Rekognition Developer Guide.
-- You pass images stored in an S3 bucket to an Amazon Rekognition API operation by using the @S3Object@ property. Images stored in an S3 bucket do not need to be base64-encoded.
-- The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.
-- If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes using the Bytes property is not supported. You must first upload the image to an Amazon S3 bucket and then call the operation using the S3Object property.
-- For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see Resource Based Policies in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { -- | Blob of image bytes up to 5 MBs.
    bytes :: Core.Maybe Core.Base64,
    -- | Identifies an S3 object as the image source.
    s3Object :: Core.Maybe Types.S3Object
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Image' value with any optional fields omitted.
mkImage ::
  Image
mkImage = Image' {bytes = Core.Nothing, s3Object = Core.Nothing}

-- | Blob of image bytes up to 5 MBs.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'bytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBytes :: Lens.Lens' Image (Core.Maybe Core.Base64)
iBytes = Lens.field @"bytes"
{-# DEPRECATED iBytes "Use generic-lens or generic-optics with 'bytes' instead." #-}

-- | Identifies an S3 object as the image source.
--
-- /Note:/ Consider using 's3Object' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iS3Object :: Lens.Lens' Image (Core.Maybe Types.S3Object)
iS3Object = Lens.field @"s3Object"
{-# DEPRECATED iS3Object "Use generic-lens or generic-optics with 's3Object' instead." #-}

instance Core.FromJSON Image where
  toJSON Image {..} =
    Core.object
      ( Core.catMaybes
          [ ("Bytes" Core..=) Core.<$> bytes,
            ("S3Object" Core..=) Core.<$> s3Object
          ]
      )
