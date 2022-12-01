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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigS3LogsDetails

-- | Information about logs for the build project.
--
-- /See:/ 'newAwsCodeBuildProjectLogsConfigDetails' smart constructor.
data AwsCodeBuildProjectLogsConfigDetails = AwsCodeBuildProjectLogsConfigDetails'
  { -- | Information about CloudWatch Logs for the build project.
    cloudWatchLogs :: Prelude.Maybe AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails,
    -- | Information about logs built to an S3 bucket for a build project.
    s3Logs :: Prelude.Maybe AwsCodeBuildProjectLogsConfigS3LogsDetails
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
-- 'cloudWatchLogs', 'awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs' - Information about CloudWatch Logs for the build project.
--
-- 's3Logs', 'awsCodeBuildProjectLogsConfigDetails_s3Logs' - Information about logs built to an S3 bucket for a build project.
newAwsCodeBuildProjectLogsConfigDetails ::
  AwsCodeBuildProjectLogsConfigDetails
newAwsCodeBuildProjectLogsConfigDetails =
  AwsCodeBuildProjectLogsConfigDetails'
    { cloudWatchLogs =
        Prelude.Nothing,
      s3Logs = Prelude.Nothing
    }

-- | Information about CloudWatch Logs for the build project.
awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs :: Lens.Lens' AwsCodeBuildProjectLogsConfigDetails (Prelude.Maybe AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails)
awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs = Lens.lens (\AwsCodeBuildProjectLogsConfigDetails' {cloudWatchLogs} -> cloudWatchLogs) (\s@AwsCodeBuildProjectLogsConfigDetails' {} a -> s {cloudWatchLogs = a} :: AwsCodeBuildProjectLogsConfigDetails)

-- | Information about logs built to an S3 bucket for a build project.
awsCodeBuildProjectLogsConfigDetails_s3Logs :: Lens.Lens' AwsCodeBuildProjectLogsConfigDetails (Prelude.Maybe AwsCodeBuildProjectLogsConfigS3LogsDetails)
awsCodeBuildProjectLogsConfigDetails_s3Logs = Lens.lens (\AwsCodeBuildProjectLogsConfigDetails' {s3Logs} -> s3Logs) (\s@AwsCodeBuildProjectLogsConfigDetails' {} a -> s {s3Logs = a} :: AwsCodeBuildProjectLogsConfigDetails)

instance
  Core.FromJSON
    AwsCodeBuildProjectLogsConfigDetails
  where
  parseJSON =
    Core.withObject
      "AwsCodeBuildProjectLogsConfigDetails"
      ( \x ->
          AwsCodeBuildProjectLogsConfigDetails'
            Prelude.<$> (x Core..:? "CloudWatchLogs")
            Prelude.<*> (x Core..:? "S3Logs")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectLogsConfigDetails
  where
  hashWithSalt
    _salt
    AwsCodeBuildProjectLogsConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` cloudWatchLogs
        `Prelude.hashWithSalt` s3Logs

instance
  Prelude.NFData
    AwsCodeBuildProjectLogsConfigDetails
  where
  rnf AwsCodeBuildProjectLogsConfigDetails' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf s3Logs

instance
  Core.ToJSON
    AwsCodeBuildProjectLogsConfigDetails
  where
  toJSON AwsCodeBuildProjectLogsConfigDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogs" Core..=)
              Prelude.<$> cloudWatchLogs,
            ("S3Logs" Core..=) Prelude.<$> s3Logs
          ]
      )
