{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayerVersionContentInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayerVersionContentInput
  ( LayerVersionContentInput (..),

    -- * Smart constructor
    mkLayerVersionContentInput,

    -- * Lenses
    lvciS3Bucket,
    lvciS3Key,
    lvciS3ObjectVersion,
    lvciZipFile,
  )
where

import qualified Network.AWS.Lambda.Types.S3Bucket as Types
import qualified Network.AWS.Lambda.Types.S3Key as Types
import qualified Network.AWS.Lambda.Types.S3ObjectVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A ZIP archive that contains the contents of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . You can specify either an Amazon S3 location, or upload a layer archive directly.
--
-- /See:/ 'mkLayerVersionContentInput' smart constructor.
data LayerVersionContentInput = LayerVersionContentInput'
  { -- | The Amazon S3 bucket of the layer archive.
    s3Bucket :: Core.Maybe Types.S3Bucket,
    -- | The Amazon S3 key of the layer archive.
    s3Key :: Core.Maybe Types.S3Key,
    -- | For versioned objects, the version of the layer archive object to use.
    s3ObjectVersion :: Core.Maybe Types.S3ObjectVersion,
    -- | The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.
    zipFile :: Core.Maybe (Core.Sensitive Core.Base64)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LayerVersionContentInput' value with any optional fields omitted.
mkLayerVersionContentInput ::
  LayerVersionContentInput
mkLayerVersionContentInput =
  LayerVersionContentInput'
    { s3Bucket = Core.Nothing,
      s3Key = Core.Nothing,
      s3ObjectVersion = Core.Nothing,
      zipFile = Core.Nothing
    }

-- | The Amazon S3 bucket of the layer archive.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvciS3Bucket :: Lens.Lens' LayerVersionContentInput (Core.Maybe Types.S3Bucket)
lvciS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED lvciS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The Amazon S3 key of the layer archive.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvciS3Key :: Lens.Lens' LayerVersionContentInput (Core.Maybe Types.S3Key)
lvciS3Key = Lens.field @"s3Key"
{-# DEPRECATED lvciS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | For versioned objects, the version of the layer archive object to use.
--
-- /Note:/ Consider using 's3ObjectVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvciS3ObjectVersion :: Lens.Lens' LayerVersionContentInput (Core.Maybe Types.S3ObjectVersion)
lvciS3ObjectVersion = Lens.field @"s3ObjectVersion"
{-# DEPRECATED lvciS3ObjectVersion "Use generic-lens or generic-optics with 's3ObjectVersion' instead." #-}

-- | The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvciZipFile :: Lens.Lens' LayerVersionContentInput (Core.Maybe (Core.Sensitive Core.Base64))
lvciZipFile = Lens.field @"zipFile"
{-# DEPRECATED lvciZipFile "Use generic-lens or generic-optics with 'zipFile' instead." #-}

instance Core.FromJSON LayerVersionContentInput where
  toJSON LayerVersionContentInput {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3Bucket" Core..=) Core.<$> s3Bucket,
            ("S3Key" Core..=) Core.<$> s3Key,
            ("S3ObjectVersion" Core..=) Core.<$> s3ObjectVersion,
            ("ZipFile" Core..=) Core.<$> zipFile
          ]
      )
