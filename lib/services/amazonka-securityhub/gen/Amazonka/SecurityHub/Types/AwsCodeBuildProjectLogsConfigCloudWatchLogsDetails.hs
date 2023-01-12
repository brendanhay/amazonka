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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about CloudWatch Logs for the build project.
--
-- /See:/ 'newAwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' smart constructor.
data AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails = AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails'
  { -- | The group name of the logs in CloudWatch Logs.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the logs in CloudWatch Logs for a build project.
    status :: Prelude.Maybe Prelude.Text,
    -- | The prefix of the stream name of the CloudWatch Logs.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName' - The group name of the logs in CloudWatch Logs.
--
-- 'status', 'awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status' - The current status of the logs in CloudWatch Logs for a build project.
--
-- 'streamName', 'awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName' - The prefix of the stream name of the CloudWatch Logs.
newAwsCodeBuildProjectLogsConfigCloudWatchLogsDetails ::
  AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
newAwsCodeBuildProjectLogsConfigCloudWatchLogsDetails =
  AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails'
    { groupName =
        Prelude.Nothing,
      status =
        Prelude.Nothing,
      streamName =
        Prelude.Nothing
    }

-- | The group name of the logs in CloudWatch Logs.
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName :: Lens.Lens' AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName = Lens.lens (\AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {groupName} -> groupName) (\s@AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {} a -> s {groupName = a} :: AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails)

-- | The current status of the logs in CloudWatch Logs for a build project.
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status :: Lens.Lens' AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status = Lens.lens (\AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {status} -> status) (\s@AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {} a -> s {status = a} :: AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails)

-- | The prefix of the stream name of the CloudWatch Logs.
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName :: Lens.Lens' AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName = Lens.lens (\AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {streamName} -> streamName) (\s@AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {} a -> s {streamName = a} :: AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails)

instance
  Data.FromJSON
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  where
  parseJSON =
    Data.withObject
      "AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails"
      ( \x ->
          AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails'
            Prelude.<$> (x Data..:? "GroupName")
              Prelude.<*> (x Data..:? "Status")
              Prelude.<*> (x Data..:? "StreamName")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  where
  hashWithSalt
    _salt
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {..} =
      _salt `Prelude.hashWithSalt` groupName
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` streamName

instance
  Prelude.NFData
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  where
  rnf
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {..} =
      Prelude.rnf groupName
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf streamName

instance
  Data.ToJSON
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
  where
  toJSON
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("GroupName" Data..=) Prelude.<$> groupName,
              ("Status" Data..=) Prelude.<$> status,
              ("StreamName" Data..=) Prelude.<$> streamName
            ]
        )
