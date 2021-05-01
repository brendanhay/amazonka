{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.SplitType
import Network.AWS.SageMaker.Types.TransformDataSource

-- | Describes the input source of a transform job and the way the transform
-- job consumes it.
--
-- /See:/ 'newTransformInput' smart constructor.
data TransformInput = TransformInput'
  { -- | The multipurpose internet mail extension (MIME) type of the data. Amazon
    -- SageMaker uses the MIME type with each http call to transfer data to the
    -- transform job.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The method to use to split the transform job\'s data files into smaller
    -- batches. Splitting is necessary when the total size of each object is
    -- too large to fit in a single request. You can also use data splitting to
    -- improve performance by processing multiple concurrent mini-batches. The
    -- default value for @SplitType@ is @None@, which indicates that input data
    -- files are not split, and request payloads contain the entire contents of
    -- an input object. Set the value of this parameter to @Line@ to split
    -- records on a newline character boundary. @SplitType@ also supports a
    -- number of record-oriented binary data formats. Currently, the supported
    -- record formats are:
    --
    -- -   RecordIO
    --
    -- -   TFRecord
    --
    -- When splitting is enabled, the size of a mini-batch depends on the
    -- values of the @BatchStrategy@ and @MaxPayloadInMB@ parameters. When the
    -- value of @BatchStrategy@ is @MultiRecord@, Amazon SageMaker sends the
    -- maximum number of records in each request, up to the @MaxPayloadInMB@
    -- limit. If the value of @BatchStrategy@ is @SingleRecord@, Amazon
    -- SageMaker sends individual records in each request.
    --
    -- Some data formats represent a record as a binary payload wrapped with
    -- extra padding bytes. When splitting is applied to a binary data format,
    -- padding is removed if the value of @BatchStrategy@ is set to
    -- @SingleRecord@. Padding is not removed if the value of @BatchStrategy@
    -- is set to @MultiRecord@.
    --
    -- For more information about @RecordIO@, see
    -- <https://mxnet.apache.org/api/faq/recordio Create a Dataset Using RecordIO>
    -- in the MXNet documentation. For more information about @TFRecord@, see
    -- <https://www.tensorflow.org/guide/datasets#consuming_tfrecord_data Consuming TFRecord data>
    -- in the TensorFlow documentation.
    splitType :: Prelude.Maybe SplitType,
    -- | If your transform data is compressed, specify the compression type.
    -- Amazon SageMaker automatically decompresses the data for the transform
    -- job accordingly. The default value is @None@.
    compressionType :: Prelude.Maybe CompressionType,
    -- | Describes the location of the channel data, which is, the S3 location of
    -- the input data that the model can consume.
    dataSource :: TransformDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransformInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'transformInput_contentType' - The multipurpose internet mail extension (MIME) type of the data. Amazon
-- SageMaker uses the MIME type with each http call to transfer data to the
-- transform job.
--
-- 'splitType', 'transformInput_splitType' - The method to use to split the transform job\'s data files into smaller
-- batches. Splitting is necessary when the total size of each object is
-- too large to fit in a single request. You can also use data splitting to
-- improve performance by processing multiple concurrent mini-batches. The
-- default value for @SplitType@ is @None@, which indicates that input data
-- files are not split, and request payloads contain the entire contents of
-- an input object. Set the value of this parameter to @Line@ to split
-- records on a newline character boundary. @SplitType@ also supports a
-- number of record-oriented binary data formats. Currently, the supported
-- record formats are:
--
-- -   RecordIO
--
-- -   TFRecord
--
-- When splitting is enabled, the size of a mini-batch depends on the
-- values of the @BatchStrategy@ and @MaxPayloadInMB@ parameters. When the
-- value of @BatchStrategy@ is @MultiRecord@, Amazon SageMaker sends the
-- maximum number of records in each request, up to the @MaxPayloadInMB@
-- limit. If the value of @BatchStrategy@ is @SingleRecord@, Amazon
-- SageMaker sends individual records in each request.
--
-- Some data formats represent a record as a binary payload wrapped with
-- extra padding bytes. When splitting is applied to a binary data format,
-- padding is removed if the value of @BatchStrategy@ is set to
-- @SingleRecord@. Padding is not removed if the value of @BatchStrategy@
-- is set to @MultiRecord@.
--
-- For more information about @RecordIO@, see
-- <https://mxnet.apache.org/api/faq/recordio Create a Dataset Using RecordIO>
-- in the MXNet documentation. For more information about @TFRecord@, see
-- <https://www.tensorflow.org/guide/datasets#consuming_tfrecord_data Consuming TFRecord data>
-- in the TensorFlow documentation.
--
-- 'compressionType', 'transformInput_compressionType' - If your transform data is compressed, specify the compression type.
-- Amazon SageMaker automatically decompresses the data for the transform
-- job accordingly. The default value is @None@.
--
-- 'dataSource', 'transformInput_dataSource' - Describes the location of the channel data, which is, the S3 location of
-- the input data that the model can consume.
newTransformInput ::
  -- | 'dataSource'
  TransformDataSource ->
  TransformInput
newTransformInput pDataSource_ =
  TransformInput'
    { contentType = Prelude.Nothing,
      splitType = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      dataSource = pDataSource_
    }

-- | The multipurpose internet mail extension (MIME) type of the data. Amazon
-- SageMaker uses the MIME type with each http call to transfer data to the
-- transform job.
transformInput_contentType :: Lens.Lens' TransformInput (Prelude.Maybe Prelude.Text)
transformInput_contentType = Lens.lens (\TransformInput' {contentType} -> contentType) (\s@TransformInput' {} a -> s {contentType = a} :: TransformInput)

-- | The method to use to split the transform job\'s data files into smaller
-- batches. Splitting is necessary when the total size of each object is
-- too large to fit in a single request. You can also use data splitting to
-- improve performance by processing multiple concurrent mini-batches. The
-- default value for @SplitType@ is @None@, which indicates that input data
-- files are not split, and request payloads contain the entire contents of
-- an input object. Set the value of this parameter to @Line@ to split
-- records on a newline character boundary. @SplitType@ also supports a
-- number of record-oriented binary data formats. Currently, the supported
-- record formats are:
--
-- -   RecordIO
--
-- -   TFRecord
--
-- When splitting is enabled, the size of a mini-batch depends on the
-- values of the @BatchStrategy@ and @MaxPayloadInMB@ parameters. When the
-- value of @BatchStrategy@ is @MultiRecord@, Amazon SageMaker sends the
-- maximum number of records in each request, up to the @MaxPayloadInMB@
-- limit. If the value of @BatchStrategy@ is @SingleRecord@, Amazon
-- SageMaker sends individual records in each request.
--
-- Some data formats represent a record as a binary payload wrapped with
-- extra padding bytes. When splitting is applied to a binary data format,
-- padding is removed if the value of @BatchStrategy@ is set to
-- @SingleRecord@. Padding is not removed if the value of @BatchStrategy@
-- is set to @MultiRecord@.
--
-- For more information about @RecordIO@, see
-- <https://mxnet.apache.org/api/faq/recordio Create a Dataset Using RecordIO>
-- in the MXNet documentation. For more information about @TFRecord@, see
-- <https://www.tensorflow.org/guide/datasets#consuming_tfrecord_data Consuming TFRecord data>
-- in the TensorFlow documentation.
transformInput_splitType :: Lens.Lens' TransformInput (Prelude.Maybe SplitType)
transformInput_splitType = Lens.lens (\TransformInput' {splitType} -> splitType) (\s@TransformInput' {} a -> s {splitType = a} :: TransformInput)

-- | If your transform data is compressed, specify the compression type.
-- Amazon SageMaker automatically decompresses the data for the transform
-- job accordingly. The default value is @None@.
transformInput_compressionType :: Lens.Lens' TransformInput (Prelude.Maybe CompressionType)
transformInput_compressionType = Lens.lens (\TransformInput' {compressionType} -> compressionType) (\s@TransformInput' {} a -> s {compressionType = a} :: TransformInput)

-- | Describes the location of the channel data, which is, the S3 location of
-- the input data that the model can consume.
transformInput_dataSource :: Lens.Lens' TransformInput TransformDataSource
transformInput_dataSource = Lens.lens (\TransformInput' {dataSource} -> dataSource) (\s@TransformInput' {} a -> s {dataSource = a} :: TransformInput)

instance Prelude.FromJSON TransformInput where
  parseJSON =
    Prelude.withObject
      "TransformInput"
      ( \x ->
          TransformInput'
            Prelude.<$> (x Prelude..:? "ContentType")
            Prelude.<*> (x Prelude..:? "SplitType")
            Prelude.<*> (x Prelude..:? "CompressionType")
            Prelude.<*> (x Prelude..: "DataSource")
      )

instance Prelude.Hashable TransformInput

instance Prelude.NFData TransformInput

instance Prelude.ToJSON TransformInput where
  toJSON TransformInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ContentType" Prelude..=) Prelude.<$> contentType,
            ("SplitType" Prelude..=) Prelude.<$> splitType,
            ("CompressionType" Prelude..=)
              Prelude.<$> compressionType,
            Prelude.Just ("DataSource" Prelude..= dataSource)
          ]
      )
