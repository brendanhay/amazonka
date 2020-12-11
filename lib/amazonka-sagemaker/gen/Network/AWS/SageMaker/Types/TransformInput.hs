-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformInput
  ( TransformInput (..),

    -- * Smart constructor
    mkTransformInput,

    -- * Lenses
    tiSplitType,
    tiCompressionType,
    tiContentType,
    tiDataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.SplitType
import Network.AWS.SageMaker.Types.TransformDataSource

-- | Describes the input source of a transform job and the way the transform job consumes it.
--
-- /See:/ 'mkTransformInput' smart constructor.
data TransformInput = TransformInput'
  { splitType ::
      Lude.Maybe SplitType,
    compressionType :: Lude.Maybe CompressionType,
    contentType :: Lude.Maybe Lude.Text,
    dataSource :: TransformDataSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformInput' with the minimum fields required to make a request.
--
-- * 'compressionType' - If your transform data is compressed, specify the compression type. Amazon SageMaker automatically decompresses the data for the transform job accordingly. The default value is @None@ .
-- * 'contentType' - The multipurpose internet mail extension (MIME) type of the data. Amazon SageMaker uses the MIME type with each http call to transfer data to the transform job.
-- * 'dataSource' - Describes the location of the channel data, which is, the S3 location of the input data that the model can consume.
-- * 'splitType' - The method to use to split the transform job's data files into smaller batches. Splitting is necessary when the total size of each object is too large to fit in a single request. You can also use data splitting to improve performance by processing multiple concurrent mini-batches. The default value for @SplitType@ is @None@ , which indicates that input data files are not split, and request payloads contain the entire contents of an input object. Set the value of this parameter to @Line@ to split records on a newline character boundary. @SplitType@ also supports a number of record-oriented binary data formats. Currently, the supported record formats are:
--
--
--     * RecordIO
--
--
--     * TFRecord
--
--
-- When splitting is enabled, the size of a mini-batch depends on the values of the @BatchStrategy@ and @MaxPayloadInMB@ parameters. When the value of @BatchStrategy@ is @MultiRecord@ , Amazon SageMaker sends the maximum number of records in each request, up to the @MaxPayloadInMB@ limit. If the value of @BatchStrategy@ is @SingleRecord@ , Amazon SageMaker sends individual records in each request.
mkTransformInput ::
  -- | 'dataSource'
  TransformDataSource ->
  TransformInput
mkTransformInput pDataSource_ =
  TransformInput'
    { splitType = Lude.Nothing,
      compressionType = Lude.Nothing,
      contentType = Lude.Nothing,
      dataSource = pDataSource_
    }

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
tiSplitType :: Lens.Lens' TransformInput (Lude.Maybe SplitType)
tiSplitType = Lens.lens (splitType :: TransformInput -> Lude.Maybe SplitType) (\s a -> s {splitType = a} :: TransformInput)
{-# DEPRECATED tiSplitType "Use generic-lens or generic-optics with 'splitType' instead." #-}

-- | If your transform data is compressed, specify the compression type. Amazon SageMaker automatically decompresses the data for the transform job accordingly. The default value is @None@ .
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiCompressionType :: Lens.Lens' TransformInput (Lude.Maybe CompressionType)
tiCompressionType = Lens.lens (compressionType :: TransformInput -> Lude.Maybe CompressionType) (\s a -> s {compressionType = a} :: TransformInput)
{-# DEPRECATED tiCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

-- | The multipurpose internet mail extension (MIME) type of the data. Amazon SageMaker uses the MIME type with each http call to transfer data to the transform job.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiContentType :: Lens.Lens' TransformInput (Lude.Maybe Lude.Text)
tiContentType = Lens.lens (contentType :: TransformInput -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: TransformInput)
{-# DEPRECATED tiContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Describes the location of the channel data, which is, the S3 location of the input data that the model can consume.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiDataSource :: Lens.Lens' TransformInput TransformDataSource
tiDataSource = Lens.lens (dataSource :: TransformInput -> TransformDataSource) (\s a -> s {dataSource = a} :: TransformInput)
{-# DEPRECATED tiDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

instance Lude.FromJSON TransformInput where
  parseJSON =
    Lude.withObject
      "TransformInput"
      ( \x ->
          TransformInput'
            Lude.<$> (x Lude..:? "SplitType")
            Lude.<*> (x Lude..:? "CompressionType")
            Lude.<*> (x Lude..:? "ContentType")
            Lude.<*> (x Lude..: "DataSource")
      )

instance Lude.ToJSON TransformInput where
  toJSON TransformInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SplitType" Lude..=) Lude.<$> splitType,
            ("CompressionType" Lude..=) Lude.<$> compressionType,
            ("ContentType" Lude..=) Lude.<$> contentType,
            Lude.Just ("DataSource" Lude..= dataSource)
          ]
      )
