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
-- Module      : Network.AWS.SageMaker.Types.MonitoringS3Output
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringS3Output where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ProcessingS3UploadMode

-- | Information about where and how you want to store the results of a
-- monitoring job.
--
-- /See:/ 'newMonitoringS3Output' smart constructor.
data MonitoringS3Output = MonitoringS3Output'
  { -- | Whether to upload the results of the monitoring job continuously or
    -- after the job completes.
    s3UploadMode :: Core.Maybe ProcessingS3UploadMode,
    -- | A URI that identifies the Amazon S3 storage location where Amazon
    -- SageMaker saves the results of a monitoring job.
    s3Uri :: Core.Text,
    -- | The local path to the Amazon S3 storage location where Amazon SageMaker
    -- saves the results of a monitoring job. LocalPath is an absolute path for
    -- the output data.
    localPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MonitoringS3Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3UploadMode', 'monitoringS3Output_s3UploadMode' - Whether to upload the results of the monitoring job continuously or
-- after the job completes.
--
-- 's3Uri', 'monitoringS3Output_s3Uri' - A URI that identifies the Amazon S3 storage location where Amazon
-- SageMaker saves the results of a monitoring job.
--
-- 'localPath', 'monitoringS3Output_localPath' - The local path to the Amazon S3 storage location where Amazon SageMaker
-- saves the results of a monitoring job. LocalPath is an absolute path for
-- the output data.
newMonitoringS3Output ::
  -- | 's3Uri'
  Core.Text ->
  -- | 'localPath'
  Core.Text ->
  MonitoringS3Output
newMonitoringS3Output pS3Uri_ pLocalPath_ =
  MonitoringS3Output'
    { s3UploadMode = Core.Nothing,
      s3Uri = pS3Uri_,
      localPath = pLocalPath_
    }

-- | Whether to upload the results of the monitoring job continuously or
-- after the job completes.
monitoringS3Output_s3UploadMode :: Lens.Lens' MonitoringS3Output (Core.Maybe ProcessingS3UploadMode)
monitoringS3Output_s3UploadMode = Lens.lens (\MonitoringS3Output' {s3UploadMode} -> s3UploadMode) (\s@MonitoringS3Output' {} a -> s {s3UploadMode = a} :: MonitoringS3Output)

-- | A URI that identifies the Amazon S3 storage location where Amazon
-- SageMaker saves the results of a monitoring job.
monitoringS3Output_s3Uri :: Lens.Lens' MonitoringS3Output Core.Text
monitoringS3Output_s3Uri = Lens.lens (\MonitoringS3Output' {s3Uri} -> s3Uri) (\s@MonitoringS3Output' {} a -> s {s3Uri = a} :: MonitoringS3Output)

-- | The local path to the Amazon S3 storage location where Amazon SageMaker
-- saves the results of a monitoring job. LocalPath is an absolute path for
-- the output data.
monitoringS3Output_localPath :: Lens.Lens' MonitoringS3Output Core.Text
monitoringS3Output_localPath = Lens.lens (\MonitoringS3Output' {localPath} -> localPath) (\s@MonitoringS3Output' {} a -> s {localPath = a} :: MonitoringS3Output)

instance Core.FromJSON MonitoringS3Output where
  parseJSON =
    Core.withObject
      "MonitoringS3Output"
      ( \x ->
          MonitoringS3Output'
            Core.<$> (x Core..:? "S3UploadMode")
            Core.<*> (x Core..: "S3Uri")
            Core.<*> (x Core..: "LocalPath")
      )

instance Core.Hashable MonitoringS3Output

instance Core.NFData MonitoringS3Output

instance Core.ToJSON MonitoringS3Output where
  toJSON MonitoringS3Output' {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3UploadMode" Core..=) Core.<$> s3UploadMode,
            Core.Just ("S3Uri" Core..= s3Uri),
            Core.Just ("LocalPath" Core..= localPath)
          ]
      )
