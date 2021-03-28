{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TransformInput
  ( TransformInput (..)
  -- * Smart constructor
  , mkTransformInput
  -- * Lenses
  , tiDataSource
  , tiCompressionType
  , tiContentType
  , tiSplitType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CompressionType as Types
import qualified Network.AWS.SageMaker.Types.ContentType as Types
import qualified Network.AWS.SageMaker.Types.SplitType as Types
import qualified Network.AWS.SageMaker.Types.TransformDataSource as Types

-- | Describes the input source of a transform job and the way the transform job consumes it.
--
-- /See:/ 'mkTransformInput' smart constructor.
data TransformInput = TransformInput'
  { dataSource :: Types.TransformDataSource
    -- ^ Describes the location of the channel data, which is, the S3 location of the input data that the model can consume.
  , compressionType :: Core.Maybe Types.CompressionType
    -- ^ If your transform data is compressed, specify the compression type. Amazon SageMaker automatically decompresses the data for the transform job accordingly. The default value is @None@ .
  , contentType :: Core.Maybe Types.ContentType
    -- ^ The multipurpose internet mail extension (MIME) type of the data. Amazon SageMaker uses the MIME type with each http call to transfer data to the transform job.
  , splitType :: Core.Maybe Types.SplitType
    -- ^ The method to use to split the transform job's data files into smaller batches. Splitting is necessary when the total size of each object is too large to fit in a single request. You can also use data splitting to improve performance by processing multiple concurrent mini-batches. The default value for @SplitType@ is @None@ , which indicates that input data files are not split, and request payloads contain the entire contents of an input object. Set the value of this parameter to @Line@ to split records on a newline character boundary. @SplitType@ also supports a number of record-oriented binary data formats. Currently, the supported record formats are:
--
--
--     * RecordIO
--
--
--     * TFRecord
--
--
-- When splitting is enabled, the size of a mini-batch depends on the values of the @BatchStrategy@ and @MaxPayloadInMB@ parameters. When the value of @BatchStrategy@ is @MultiRecord@ , Amazon SageMaker sends the maximum number of records in each request, up to the @MaxPayloadInMB@ limit. If the value of @BatchStrategy@ is @SingleRecord@ , Amazon SageMaker sends individual records in each request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransformInput' value with any optional fields omitted.
mkTransformInput
    :: Types.TransformDataSource -- ^ 'dataSource'
    -> TransformInput
mkTransformInput dataSource
  = TransformInput'{dataSource, compressionType = Core.Nothing,
                    contentType = Core.Nothing, splitType = Core.Nothing}

-- | Describes the location of the channel data, which is, the S3 location of the input data that the model can consume.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiDataSource :: Lens.Lens' TransformInput Types.TransformDataSource
tiDataSource = Lens.field @"dataSource"
{-# INLINEABLE tiDataSource #-}
{-# DEPRECATED dataSource "Use generic-lens or generic-optics with 'dataSource' instead"  #-}

-- | If your transform data is compressed, specify the compression type. Amazon SageMaker automatically decompresses the data for the transform job accordingly. The default value is @None@ .
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiCompressionType :: Lens.Lens' TransformInput (Core.Maybe Types.CompressionType)
tiCompressionType = Lens.field @"compressionType"
{-# INLINEABLE tiCompressionType #-}
{-# DEPRECATED compressionType "Use generic-lens or generic-optics with 'compressionType' instead"  #-}

-- | The multipurpose internet mail extension (MIME) type of the data. Amazon SageMaker uses the MIME type with each http call to transfer data to the transform job.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiContentType :: Lens.Lens' TransformInput (Core.Maybe Types.ContentType)
tiContentType = Lens.field @"contentType"
{-# INLINEABLE tiContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The method to use to split the transform job's data files into smaller batches. Splitting is necessary when the total size of each object is too large to fit in a single request. You can also use data splitting to improve performance by processing multiple concurrent mini-batches. The default value for @SplitType@ is @None@ , which indicates that input data files are not split, and request payloads contain the entire contents of an input object. Set the value of this parameter to @Line@ to split records on a newline character boundary. @SplitType@ also supports a number of record-oriented binary data formats. Currently, the supported record formats are:
--
--
--     * RecordIO
--
--
--     * TFRecord
--
--
-- When splitting is enabled, the size of a mini-batch depends on the values of the @BatchStrategy@ and @MaxPayloadInMB@ parameters. When the value of @BatchStrategy@ is @MultiRecord@ , Amazon SageMaker sends the maximum number of records in each request, up to the @MaxPayloadInMB@ limit. If the value of @BatchStrategy@ is @SingleRecord@ , Amazon SageMaker sends individual records in each request.
--
-- /Note:/ Consider using 'splitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiSplitType :: Lens.Lens' TransformInput (Core.Maybe Types.SplitType)
tiSplitType = Lens.field @"splitType"
{-# INLINEABLE tiSplitType #-}
{-# DEPRECATED splitType "Use generic-lens or generic-optics with 'splitType' instead"  #-}

instance Core.FromJSON TransformInput where
        toJSON TransformInput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DataSource" Core..= dataSource),
                  ("CompressionType" Core..=) Core.<$> compressionType,
                  ("ContentType" Core..=) Core.<$> contentType,
                  ("SplitType" Core..=) Core.<$> splitType])

instance Core.FromJSON TransformInput where
        parseJSON
          = Core.withObject "TransformInput" Core.$
              \ x ->
                TransformInput' Core.<$>
                  (x Core..: "DataSource") Core.<*> x Core..:? "CompressionType"
                    Core.<*> x Core..:? "ContentType"
                    Core.<*> x Core..:? "SplitType"
