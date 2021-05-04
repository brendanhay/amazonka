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
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3Output
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3Output where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ProcessingS3UploadMode

-- | Configuration for uploading output data to Amazon S3 from the processing
-- container.
--
-- /See:/ 'newProcessingS3Output' smart constructor.
data ProcessingS3Output = ProcessingS3Output'
  { -- | A URI that identifies the Amazon S3 bucket where you want Amazon
    -- SageMaker to save the results of a processing job.
    s3Uri :: Prelude.Text,
    -- | The local path of a directory where you want Amazon SageMaker to upload
    -- its contents to Amazon S3. @LocalPath@ is an absolute path to a
    -- directory containing output files. This directory will be created by the
    -- platform and exist when your container\'s entrypoint is invoked.
    localPath :: Prelude.Text,
    -- | Whether to upload the results of the processing job continuously or
    -- after the job completes.
    s3UploadMode :: ProcessingS3UploadMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProcessingS3Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'processingS3Output_s3Uri' - A URI that identifies the Amazon S3 bucket where you want Amazon
-- SageMaker to save the results of a processing job.
--
-- 'localPath', 'processingS3Output_localPath' - The local path of a directory where you want Amazon SageMaker to upload
-- its contents to Amazon S3. @LocalPath@ is an absolute path to a
-- directory containing output files. This directory will be created by the
-- platform and exist when your container\'s entrypoint is invoked.
--
-- 's3UploadMode', 'processingS3Output_s3UploadMode' - Whether to upload the results of the processing job continuously or
-- after the job completes.
newProcessingS3Output ::
  -- | 's3Uri'
  Prelude.Text ->
  -- | 'localPath'
  Prelude.Text ->
  -- | 's3UploadMode'
  ProcessingS3UploadMode ->
  ProcessingS3Output
newProcessingS3Output
  pS3Uri_
  pLocalPath_
  pS3UploadMode_ =
    ProcessingS3Output'
      { s3Uri = pS3Uri_,
        localPath = pLocalPath_,
        s3UploadMode = pS3UploadMode_
      }

-- | A URI that identifies the Amazon S3 bucket where you want Amazon
-- SageMaker to save the results of a processing job.
processingS3Output_s3Uri :: Lens.Lens' ProcessingS3Output Prelude.Text
processingS3Output_s3Uri = Lens.lens (\ProcessingS3Output' {s3Uri} -> s3Uri) (\s@ProcessingS3Output' {} a -> s {s3Uri = a} :: ProcessingS3Output)

-- | The local path of a directory where you want Amazon SageMaker to upload
-- its contents to Amazon S3. @LocalPath@ is an absolute path to a
-- directory containing output files. This directory will be created by the
-- platform and exist when your container\'s entrypoint is invoked.
processingS3Output_localPath :: Lens.Lens' ProcessingS3Output Prelude.Text
processingS3Output_localPath = Lens.lens (\ProcessingS3Output' {localPath} -> localPath) (\s@ProcessingS3Output' {} a -> s {localPath = a} :: ProcessingS3Output)

-- | Whether to upload the results of the processing job continuously or
-- after the job completes.
processingS3Output_s3UploadMode :: Lens.Lens' ProcessingS3Output ProcessingS3UploadMode
processingS3Output_s3UploadMode = Lens.lens (\ProcessingS3Output' {s3UploadMode} -> s3UploadMode) (\s@ProcessingS3Output' {} a -> s {s3UploadMode = a} :: ProcessingS3Output)

instance Prelude.FromJSON ProcessingS3Output where
  parseJSON =
    Prelude.withObject
      "ProcessingS3Output"
      ( \x ->
          ProcessingS3Output'
            Prelude.<$> (x Prelude..: "S3Uri")
            Prelude.<*> (x Prelude..: "LocalPath")
            Prelude.<*> (x Prelude..: "S3UploadMode")
      )

instance Prelude.Hashable ProcessingS3Output

instance Prelude.NFData ProcessingS3Output

instance Prelude.ToJSON ProcessingS3Output where
  toJSON ProcessingS3Output' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Uri" Prelude..= s3Uri),
            Prelude.Just ("LocalPath" Prelude..= localPath),
            Prelude.Just
              ("S3UploadMode" Prelude..= s3UploadMode)
          ]
      )
