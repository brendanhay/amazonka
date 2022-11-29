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
-- Module      : Amazonka.RDS.Types.CloudwatchLogsExportConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.CloudwatchLogsExportConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
    enableLogTypes :: Prelude.Maybe [Prelude.Text],
    -- | The list of log types to disable.
    disableLogTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      disableLogTypes = Prelude.Nothing
    }

-- | The list of log types to enable.
cloudwatchLogsExportConfiguration_enableLogTypes :: Lens.Lens' CloudwatchLogsExportConfiguration (Prelude.Maybe [Prelude.Text])
cloudwatchLogsExportConfiguration_enableLogTypes = Lens.lens (\CloudwatchLogsExportConfiguration' {enableLogTypes} -> enableLogTypes) (\s@CloudwatchLogsExportConfiguration' {} a -> s {enableLogTypes = a} :: CloudwatchLogsExportConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The list of log types to disable.
cloudwatchLogsExportConfiguration_disableLogTypes :: Lens.Lens' CloudwatchLogsExportConfiguration (Prelude.Maybe [Prelude.Text])
cloudwatchLogsExportConfiguration_disableLogTypes = Lens.lens (\CloudwatchLogsExportConfiguration' {disableLogTypes} -> disableLogTypes) (\s@CloudwatchLogsExportConfiguration' {} a -> s {disableLogTypes = a} :: CloudwatchLogsExportConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    CloudwatchLogsExportConfiguration
  where
  hashWithSalt
    _salt
    CloudwatchLogsExportConfiguration' {..} =
      _salt `Prelude.hashWithSalt` enableLogTypes
        `Prelude.hashWithSalt` disableLogTypes

instance
  Prelude.NFData
    CloudwatchLogsExportConfiguration
  where
  rnf CloudwatchLogsExportConfiguration' {..} =
    Prelude.rnf enableLogTypes
      `Prelude.seq` Prelude.rnf disableLogTypes

instance
  Core.ToQuery
    CloudwatchLogsExportConfiguration
  where
  toQuery CloudwatchLogsExportConfiguration' {..} =
    Prelude.mconcat
      [ "EnableLogTypes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableLogTypes
            ),
        "DisableLogTypes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> disableLogTypes
            )
      ]
