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
-- Module      : Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB instance or DB cluster.
--
-- The @EnableLogTypes@ and @DisableLogTypes@ arrays determine which logs
-- will be exported (or not exported) to CloudWatch Logs. The values within
-- these arrays depend on the DB engine being used.
--
-- For more information about exporting CloudWatch Logs for Amazon RDS DB
-- instances, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora
-- DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newCloudwatchLogsExportConfiguration' smart constructor.
data CloudwatchLogsExportConfiguration = CloudwatchLogsExportConfiguration'
  { -- | The list of log types to enable.
    enableLogTypes :: Core.Maybe [Core.Text],
    -- | The list of log types to disable.
    disableLogTypes :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloudwatchLogsExportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableLogTypes', 'cloudwatchLogsExportConfiguration_enableLogTypes' - The list of log types to enable.
--
-- 'disableLogTypes', 'cloudwatchLogsExportConfiguration_disableLogTypes' - The list of log types to disable.
newCloudwatchLogsExportConfiguration ::
  CloudwatchLogsExportConfiguration
newCloudwatchLogsExportConfiguration =
  CloudwatchLogsExportConfiguration'
    { enableLogTypes =
        Core.Nothing,
      disableLogTypes = Core.Nothing
    }

-- | The list of log types to enable.
cloudwatchLogsExportConfiguration_enableLogTypes :: Lens.Lens' CloudwatchLogsExportConfiguration (Core.Maybe [Core.Text])
cloudwatchLogsExportConfiguration_enableLogTypes = Lens.lens (\CloudwatchLogsExportConfiguration' {enableLogTypes} -> enableLogTypes) (\s@CloudwatchLogsExportConfiguration' {} a -> s {enableLogTypes = a} :: CloudwatchLogsExportConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The list of log types to disable.
cloudwatchLogsExportConfiguration_disableLogTypes :: Lens.Lens' CloudwatchLogsExportConfiguration (Core.Maybe [Core.Text])
cloudwatchLogsExportConfiguration_disableLogTypes = Lens.lens (\CloudwatchLogsExportConfiguration' {disableLogTypes} -> disableLogTypes) (\s@CloudwatchLogsExportConfiguration' {} a -> s {disableLogTypes = a} :: CloudwatchLogsExportConfiguration) Core.. Lens.mapping Lens._Coerce

instance
  Core.Hashable
    CloudwatchLogsExportConfiguration

instance
  Core.NFData
    CloudwatchLogsExportConfiguration

instance
  Core.ToQuery
    CloudwatchLogsExportConfiguration
  where
  toQuery CloudwatchLogsExportConfiguration' {..} =
    Core.mconcat
      [ "EnableLogTypes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> enableLogTypes),
        "DisableLogTypes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> disableLogTypes)
      ]
