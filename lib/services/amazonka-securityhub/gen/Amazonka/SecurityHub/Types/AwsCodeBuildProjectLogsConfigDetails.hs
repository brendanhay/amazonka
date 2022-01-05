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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigS3LogsDetails

-- | Information about logs for the build project.
--
-- /See:/ 'newAwsCodeBuildProjectLogsConfigDetails' smart constructor.
data AwsCodeBuildProjectLogsConfigDetails = AwsCodeBuildProjectLogsConfigDetails'
  { -- | Information about logs built to an S3 bucket for a build project.
    s3Logs :: Prelude.Maybe AwsCodeBuildProjectLogsConfigS3LogsDetails,
    -- | Information about CloudWatch Logs for the build project.
    cloudWatchLogs :: Prelude.Maybe AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectLogsConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Logs', 'awsCodeBuildProjectLogsConfigDetails_s3Logs' - Information about logs built to an S3 bucket for a build project.
--
-- 'cloudWatchLogs', 'awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs' - Information about CloudWatch Logs for the build project.
newAwsCodeBuildProjectLogsConfigDetails ::
  AwsCodeBuildProjectLogsConfigDetails
newAwsCodeBuildProjectLogsConfigDetails =
  AwsCodeBuildProjectLogsConfigDetails'
    { s3Logs =
        Prelude.Nothing,
      cloudWatchLogs = Prelude.Nothing
    }

-- | Information about logs built to an S3 bucket for a build project.
awsCodeBuildProjectLogsConfigDetails_s3Logs :: Lens.Lens' AwsCodeBuildProjectLogsConfigDetails (Prelude.Maybe AwsCodeBuildProjectLogsConfigS3LogsDetails)
awsCodeBuildProjectLogsConfigDetails_s3Logs = Lens.lens (\AwsCodeBuildProjectLogsConfigDetails' {s3Logs} -> s3Logs) (\s@AwsCodeBuildProjectLogsConfigDetails' {} a -> s {s3Logs = a} :: AwsCodeBuildProjectLogsConfigDetails)

-- | Information about CloudWatch Logs for the build project.
awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs :: Lens.Lens' AwsCodeBuildProjectLogsConfigDetails (Prelude.Maybe AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails)
awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs = Lens.lens (\AwsCodeBuildProjectLogsConfigDetails' {cloudWatchLogs} -> cloudWatchLogs) (\s@AwsCodeBuildProjectLogsConfigDetails' {} a -> s {cloudWatchLogs = a} :: AwsCodeBuildProjectLogsConfigDetails)

instance
  Core.FromJSON
    AwsCodeBuildProjectLogsConfigDetails
  where
  parseJSON =
    Core.withObject
      "AwsCodeBuildProjectLogsConfigDetails"
      ( \x ->
          AwsCodeBuildProjectLogsConfigDetails'
            Prelude.<$> (x Core..:? "S3Logs")
            Prelude.<*> (x Core..:? "CloudWatchLogs")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectLogsConfigDetails
  where
  hashWithSalt
    _salt
    AwsCodeBuildProjectLogsConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` s3Logs
        `Prelude.hashWithSalt` cloudWatchLogs

instance
  Prelude.NFData
    AwsCodeBuildProjectLogsConfigDetails
  where
  rnf AwsCodeBuildProjectLogsConfigDetails' {..} =
    Prelude.rnf s3Logs
      `Prelude.seq` Prelude.rnf cloudWatchLogs

instance
  Core.ToJSON
    AwsCodeBuildProjectLogsConfigDetails
  where
  toJSON AwsCodeBuildProjectLogsConfigDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3Logs" Core..=) Prelude.<$> s3Logs,
            ("CloudWatchLogs" Core..=)
              Prelude.<$> cloudWatchLogs
          ]
      )
