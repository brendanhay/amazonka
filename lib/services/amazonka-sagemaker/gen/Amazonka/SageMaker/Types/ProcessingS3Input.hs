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
-- Module      : Amazonka.SageMaker.Types.ProcessingS3Input
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingS3Input where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingS3CompressionType
import Amazonka.SageMaker.Types.ProcessingS3DataDistributionType
import Amazonka.SageMaker.Types.ProcessingS3DataType
import Amazonka.SageMaker.Types.ProcessingS3InputMode

-- | Configuration for downloading input data from Amazon S3 into the
-- processing container.
--
-- /See:/ 'newProcessingS3Input' smart constructor.
data ProcessingS3Input = ProcessingS3Input'
  { -- | The local path in your container where you want Amazon SageMaker to
    -- write input data to. @LocalPath@ is an absolute path to the input data
    -- and must begin with @\/opt\/ml\/processing\/@. @LocalPath@ is a required
    -- parameter when @AppManaged@ is @False@ (default).
    localPath :: Prelude.Maybe Prelude.Text,
    -- | Whether to GZIP-decompress the data in Amazon S3 as it is streamed into
    -- the processing container. @Gzip@ can only be used when @Pipe@ mode is
    -- specified as the @S3InputMode@. In @Pipe@ mode, Amazon SageMaker streams
    -- input data from the source directly to your container without using the
    -- EBS volume.
    s3CompressionType :: Prelude.Maybe ProcessingS3CompressionType,
    -- | Whether to distribute the data from Amazon S3 to all processing
    -- instances with @FullyReplicated@, or whether the data from Amazon S3 is
    -- shared by Amazon S3 key, downloading one shard of data to each
    -- processing instance.
    s3DataDistributionType :: Prelude.Maybe ProcessingS3DataDistributionType,
    -- | Whether to use @File@ or @Pipe@ input mode. In File mode, Amazon
    -- SageMaker copies the data from the input source onto the local ML
    -- storage volume before starting your processing container. This is the
    -- most commonly used input mode. In @Pipe@ mode, Amazon SageMaker streams
    -- input data from the source directly to your processing container into
    -- named pipes without using the ML storage volume.
    s3InputMode :: Prelude.Maybe ProcessingS3InputMode,
    -- | The URI of the Amazon S3 prefix Amazon SageMaker downloads data required
    -- to run a processing job.
    s3Uri :: Prelude.Text,
    -- | Whether you use an @S3Prefix@ or a @ManifestFile@ for the data type. If
    -- you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
    -- SageMaker uses all objects with the specified key name prefix for the
    -- processing job. If you choose @ManifestFile@, @S3Uri@ identifies an
    -- object that is a manifest file containing a list of object keys that you
    -- want Amazon SageMaker to use for the processing job.
    s3DataType :: ProcessingS3DataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingS3Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localPath', 'processingS3Input_localPath' - The local path in your container where you want Amazon SageMaker to
-- write input data to. @LocalPath@ is an absolute path to the input data
-- and must begin with @\/opt\/ml\/processing\/@. @LocalPath@ is a required
-- parameter when @AppManaged@ is @False@ (default).
--
-- 's3CompressionType', 'processingS3Input_s3CompressionType' - Whether to GZIP-decompress the data in Amazon S3 as it is streamed into
-- the processing container. @Gzip@ can only be used when @Pipe@ mode is
-- specified as the @S3InputMode@. In @Pipe@ mode, Amazon SageMaker streams
-- input data from the source directly to your container without using the
-- EBS volume.
--
-- 's3DataDistributionType', 'processingS3Input_s3DataDistributionType' - Whether to distribute the data from Amazon S3 to all processing
-- instances with @FullyReplicated@, or whether the data from Amazon S3 is
-- shared by Amazon S3 key, downloading one shard of data to each
-- processing instance.
--
-- 's3InputMode', 'processingS3Input_s3InputMode' - Whether to use @File@ or @Pipe@ input mode. In File mode, Amazon
-- SageMaker copies the data from the input source onto the local ML
-- storage volume before starting your processing container. This is the
-- most commonly used input mode. In @Pipe@ mode, Amazon SageMaker streams
-- input data from the source directly to your processing container into
-- named pipes without using the ML storage volume.
--
-- 's3Uri', 'processingS3Input_s3Uri' - The URI of the Amazon S3 prefix Amazon SageMaker downloads data required
-- to run a processing job.
--
-- 's3DataType', 'processingS3Input_s3DataType' - Whether you use an @S3Prefix@ or a @ManifestFile@ for the data type. If
-- you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
-- SageMaker uses all objects with the specified key name prefix for the
-- processing job. If you choose @ManifestFile@, @S3Uri@ identifies an
-- object that is a manifest file containing a list of object keys that you
-- want Amazon SageMaker to use for the processing job.
newProcessingS3Input ::
  -- | 's3Uri'
  Prelude.Text ->
  -- | 's3DataType'
  ProcessingS3DataType ->
  ProcessingS3Input
newProcessingS3Input pS3Uri_ pS3DataType_ =
  ProcessingS3Input'
    { localPath = Prelude.Nothing,
      s3CompressionType = Prelude.Nothing,
      s3DataDistributionType = Prelude.Nothing,
      s3InputMode = Prelude.Nothing,
      s3Uri = pS3Uri_,
      s3DataType = pS3DataType_
    }

-- | The local path in your container where you want Amazon SageMaker to
-- write input data to. @LocalPath@ is an absolute path to the input data
-- and must begin with @\/opt\/ml\/processing\/@. @LocalPath@ is a required
-- parameter when @AppManaged@ is @False@ (default).
processingS3Input_localPath :: Lens.Lens' ProcessingS3Input (Prelude.Maybe Prelude.Text)
processingS3Input_localPath = Lens.lens (\ProcessingS3Input' {localPath} -> localPath) (\s@ProcessingS3Input' {} a -> s {localPath = a} :: ProcessingS3Input)

-- | Whether to GZIP-decompress the data in Amazon S3 as it is streamed into
-- the processing container. @Gzip@ can only be used when @Pipe@ mode is
-- specified as the @S3InputMode@. In @Pipe@ mode, Amazon SageMaker streams
-- input data from the source directly to your container without using the
-- EBS volume.
processingS3Input_s3CompressionType :: Lens.Lens' ProcessingS3Input (Prelude.Maybe ProcessingS3CompressionType)
processingS3Input_s3CompressionType = Lens.lens (\ProcessingS3Input' {s3CompressionType} -> s3CompressionType) (\s@ProcessingS3Input' {} a -> s {s3CompressionType = a} :: ProcessingS3Input)

-- | Whether to distribute the data from Amazon S3 to all processing
-- instances with @FullyReplicated@, or whether the data from Amazon S3 is
-- shared by Amazon S3 key, downloading one shard of data to each
-- processing instance.
processingS3Input_s3DataDistributionType :: Lens.Lens' ProcessingS3Input (Prelude.Maybe ProcessingS3DataDistributionType)
processingS3Input_s3DataDistributionType = Lens.lens (\ProcessingS3Input' {s3DataDistributionType} -> s3DataDistributionType) (\s@ProcessingS3Input' {} a -> s {s3DataDistributionType = a} :: ProcessingS3Input)

-- | Whether to use @File@ or @Pipe@ input mode. In File mode, Amazon
-- SageMaker copies the data from the input source onto the local ML
-- storage volume before starting your processing container. This is the
-- most commonly used input mode. In @Pipe@ mode, Amazon SageMaker streams
-- input data from the source directly to your processing container into
-- named pipes without using the ML storage volume.
processingS3Input_s3InputMode :: Lens.Lens' ProcessingS3Input (Prelude.Maybe ProcessingS3InputMode)
processingS3Input_s3InputMode = Lens.lens (\ProcessingS3Input' {s3InputMode} -> s3InputMode) (\s@ProcessingS3Input' {} a -> s {s3InputMode = a} :: ProcessingS3Input)

-- | The URI of the Amazon S3 prefix Amazon SageMaker downloads data required
-- to run a processing job.
processingS3Input_s3Uri :: Lens.Lens' ProcessingS3Input Prelude.Text
processingS3Input_s3Uri = Lens.lens (\ProcessingS3Input' {s3Uri} -> s3Uri) (\s@ProcessingS3Input' {} a -> s {s3Uri = a} :: ProcessingS3Input)

-- | Whether you use an @S3Prefix@ or a @ManifestFile@ for the data type. If
-- you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
-- SageMaker uses all objects with the specified key name prefix for the
-- processing job. If you choose @ManifestFile@, @S3Uri@ identifies an
-- object that is a manifest file containing a list of object keys that you
-- want Amazon SageMaker to use for the processing job.
processingS3Input_s3DataType :: Lens.Lens' ProcessingS3Input ProcessingS3DataType
processingS3Input_s3DataType = Lens.lens (\ProcessingS3Input' {s3DataType} -> s3DataType) (\s@ProcessingS3Input' {} a -> s {s3DataType = a} :: ProcessingS3Input)

instance Data.FromJSON ProcessingS3Input where
  parseJSON =
    Data.withObject
      "ProcessingS3Input"
      ( \x ->
          ProcessingS3Input'
            Prelude.<$> (x Data..:? "LocalPath")
            Prelude.<*> (x Data..:? "S3CompressionType")
            Prelude.<*> (x Data..:? "S3DataDistributionType")
            Prelude.<*> (x Data..:? "S3InputMode")
            Prelude.<*> (x Data..: "S3Uri")
            Prelude.<*> (x Data..: "S3DataType")
      )

instance Prelude.Hashable ProcessingS3Input where
  hashWithSalt _salt ProcessingS3Input' {..} =
    _salt
      `Prelude.hashWithSalt` localPath
      `Prelude.hashWithSalt` s3CompressionType
      `Prelude.hashWithSalt` s3DataDistributionType
      `Prelude.hashWithSalt` s3InputMode
      `Prelude.hashWithSalt` s3Uri
      `Prelude.hashWithSalt` s3DataType

instance Prelude.NFData ProcessingS3Input where
  rnf ProcessingS3Input' {..} =
    Prelude.rnf localPath
      `Prelude.seq` Prelude.rnf s3CompressionType
      `Prelude.seq` Prelude.rnf s3DataDistributionType
      `Prelude.seq` Prelude.rnf s3InputMode
      `Prelude.seq` Prelude.rnf s3Uri
      `Prelude.seq` Prelude.rnf s3DataType

instance Data.ToJSON ProcessingS3Input where
  toJSON ProcessingS3Input' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LocalPath" Data..=) Prelude.<$> localPath,
            ("S3CompressionType" Data..=)
              Prelude.<$> s3CompressionType,
            ("S3DataDistributionType" Data..=)
              Prelude.<$> s3DataDistributionType,
            ("S3InputMode" Data..=) Prelude.<$> s3InputMode,
            Prelude.Just ("S3Uri" Data..= s3Uri),
            Prelude.Just ("S3DataType" Data..= s3DataType)
          ]
      )
