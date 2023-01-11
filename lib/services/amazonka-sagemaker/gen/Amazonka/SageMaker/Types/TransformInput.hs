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
-- Module      : Amazonka.SageMaker.Types.TransformInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TransformInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CompressionType
import Amazonka.SageMaker.Types.SplitType
import Amazonka.SageMaker.Types.TransformDataSource

-- | Describes the input source of a transform job and the way the transform
-- job consumes it.
--
-- /See:/ 'newTransformInput' smart constructor.
data TransformInput = TransformInput'
  { -- | If your transform data is compressed, specify the compression type.
    -- Amazon SageMaker automatically decompresses the data for the transform
    -- job accordingly. The default value is @None@.
    compressionType :: Prelude.Maybe CompressionType,
    -- | The multipurpose internet mail extension (MIME) type of the data. Amazon
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
    -- <https://www.tensorflow.org/guide/data#consuming_tfrecord_data Consuming TFRecord data>
    -- in the TensorFlow documentation.
    splitType :: Prelude.Maybe SplitType,
    -- | Describes the location of the channel data, which is, the S3 location of
    -- the input data that the model can consume.
    dataSource :: TransformDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compressionType', 'transformInput_compressionType' - If your transform data is compressed, specify the compression type.
-- Amazon SageMaker automatically decompresses the data for the transform
-- job accordingly. The default value is @None@.
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
-- <https://www.tensorflow.org/guide/data#consuming_tfrecord_data Consuming TFRecord data>
-- in the TensorFlow documentation.
--
-- 'dataSource', 'transformInput_dataSource' - Describes the location of the channel data, which is, the S3 location of
-- the input data that the model can consume.
newTransformInput ::
  -- | 'dataSource'
  TransformDataSource ->
  TransformInput
newTransformInput pDataSource_ =
  TransformInput'
    { compressionType = Prelude.Nothing,
      contentType = Prelude.Nothing,
      splitType = Prelude.Nothing,
      dataSource = pDataSource_
    }

-- | If your transform data is compressed, specify the compression type.
-- Amazon SageMaker automatically decompresses the data for the transform
-- job accordingly. The default value is @None@.
transformInput_compressionType :: Lens.Lens' TransformInput (Prelude.Maybe CompressionType)
transformInput_compressionType = Lens.lens (\TransformInput' {compressionType} -> compressionType) (\s@TransformInput' {} a -> s {compressionType = a} :: TransformInput)

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
-- <https://www.tensorflow.org/guide/data#consuming_tfrecord_data Consuming TFRecord data>
-- in the TensorFlow documentation.
transformInput_splitType :: Lens.Lens' TransformInput (Prelude.Maybe SplitType)
transformInput_splitType = Lens.lens (\TransformInput' {splitType} -> splitType) (\s@TransformInput' {} a -> s {splitType = a} :: TransformInput)

-- | Describes the location of the channel data, which is, the S3 location of
-- the input data that the model can consume.
transformInput_dataSource :: Lens.Lens' TransformInput TransformDataSource
transformInput_dataSource = Lens.lens (\TransformInput' {dataSource} -> dataSource) (\s@TransformInput' {} a -> s {dataSource = a} :: TransformInput)

instance Data.FromJSON TransformInput where
  parseJSON =
    Data.withObject
      "TransformInput"
      ( \x ->
          TransformInput'
            Prelude.<$> (x Data..:? "CompressionType")
            Prelude.<*> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "SplitType")
            Prelude.<*> (x Data..: "DataSource")
      )

instance Prelude.Hashable TransformInput where
  hashWithSalt _salt TransformInput' {..} =
    _salt `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` splitType
      `Prelude.hashWithSalt` dataSource

instance Prelude.NFData TransformInput where
  rnf TransformInput' {..} =
    Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf splitType
      `Prelude.seq` Prelude.rnf dataSource

instance Data.ToJSON TransformInput where
  toJSON TransformInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompressionType" Data..=)
              Prelude.<$> compressionType,
            ("ContentType" Data..=) Prelude.<$> contentType,
            ("SplitType" Data..=) Prelude.<$> splitType,
            Prelude.Just ("DataSource" Data..= dataSource)
          ]
      )
