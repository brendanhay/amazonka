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
-- Module      : Amazonka.SimSpaceWeaver.Types.LogDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.LogDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SimSpaceWeaver.Types.CloudWatchLogsLogGroup

-- | The location where SimSpace Weaver sends simulation log data.
--
-- /See:/ 'newLogDestination' smart constructor.
data LogDestination = LogDestination'
  { -- | An Amazon CloudWatch Logs log group that stores simulation log data. For
    -- more information about log groups, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with log groups and log streams>
    -- in the /Amazon CloudWatch Logs User Guide/.
    cloudWatchLogsLogGroup :: Prelude.Maybe CloudWatchLogsLogGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsLogGroup', 'logDestination_cloudWatchLogsLogGroup' - An Amazon CloudWatch Logs log group that stores simulation log data. For
-- more information about log groups, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with log groups and log streams>
-- in the /Amazon CloudWatch Logs User Guide/.
newLogDestination ::
  LogDestination
newLogDestination =
  LogDestination'
    { cloudWatchLogsLogGroup =
        Prelude.Nothing
    }

-- | An Amazon CloudWatch Logs log group that stores simulation log data. For
-- more information about log groups, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with log groups and log streams>
-- in the /Amazon CloudWatch Logs User Guide/.
logDestination_cloudWatchLogsLogGroup :: Lens.Lens' LogDestination (Prelude.Maybe CloudWatchLogsLogGroup)
logDestination_cloudWatchLogsLogGroup = Lens.lens (\LogDestination' {cloudWatchLogsLogGroup} -> cloudWatchLogsLogGroup) (\s@LogDestination' {} a -> s {cloudWatchLogsLogGroup = a} :: LogDestination)

instance Data.FromJSON LogDestination where
  parseJSON =
    Data.withObject
      "LogDestination"
      ( \x ->
          LogDestination'
            Prelude.<$> (x Data..:? "CloudWatchLogsLogGroup")
      )

instance Prelude.Hashable LogDestination where
  hashWithSalt _salt LogDestination' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchLogsLogGroup

instance Prelude.NFData LogDestination where
  rnf LogDestination' {..} =
    Prelude.rnf cloudWatchLogsLogGroup
