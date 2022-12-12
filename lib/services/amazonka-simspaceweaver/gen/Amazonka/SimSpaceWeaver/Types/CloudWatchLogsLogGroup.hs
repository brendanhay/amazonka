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
-- Module      : Amazonka.SimSpaceWeaver.Types.CloudWatchLogsLogGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.CloudWatchLogsLogGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon CloudWatch Logs log group for the simulation. For more
-- information about log groups, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with log groups and log streams>
-- in the /Amazon CloudWatch Logs User Guide/.
--
-- /See:/ 'newCloudWatchLogsLogGroup' smart constructor.
data CloudWatchLogsLogGroup = CloudWatchLogsLogGroup'
  { -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log group
    -- for the simulation. For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/. For more information
    -- about log groups, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with log groups and log streams>
    -- in the /Amazon CloudWatch Logs User Guide/.
    logGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'cloudWatchLogsLogGroup_logGroupArn' - The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log group
-- for the simulation. For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/. For more information
-- about log groups, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with log groups and log streams>
-- in the /Amazon CloudWatch Logs User Guide/.
newCloudWatchLogsLogGroup ::
  CloudWatchLogsLogGroup
newCloudWatchLogsLogGroup =
  CloudWatchLogsLogGroup'
    { logGroupArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log group
-- for the simulation. For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/. For more information
-- about log groups, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with log groups and log streams>
-- in the /Amazon CloudWatch Logs User Guide/.
cloudWatchLogsLogGroup_logGroupArn :: Lens.Lens' CloudWatchLogsLogGroup (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogGroup_logGroupArn = Lens.lens (\CloudWatchLogsLogGroup' {logGroupArn} -> logGroupArn) (\s@CloudWatchLogsLogGroup' {} a -> s {logGroupArn = a} :: CloudWatchLogsLogGroup)

instance Data.FromJSON CloudWatchLogsLogGroup where
  parseJSON =
    Data.withObject
      "CloudWatchLogsLogGroup"
      ( \x ->
          CloudWatchLogsLogGroup'
            Prelude.<$> (x Data..:? "LogGroupArn")
      )

instance Prelude.Hashable CloudWatchLogsLogGroup where
  hashWithSalt _salt CloudWatchLogsLogGroup' {..} =
    _salt `Prelude.hashWithSalt` logGroupArn

instance Prelude.NFData CloudWatchLogsLogGroup where
  rnf CloudWatchLogsLogGroup' {..} =
    Prelude.rnf logGroupArn
