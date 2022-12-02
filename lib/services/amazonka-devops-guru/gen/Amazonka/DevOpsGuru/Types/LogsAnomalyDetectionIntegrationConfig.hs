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
-- Module      : Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegrationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.OptInStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the integration of DevOps Guru with CloudWatch log
-- groups for log anomaly detection. You can use this to update the
-- configuration.
--
-- /See:/ 'newLogsAnomalyDetectionIntegrationConfig' smart constructor.
data LogsAnomalyDetectionIntegrationConfig = LogsAnomalyDetectionIntegrationConfig'
  { -- | Specifies if DevOps Guru is configured to perform log anomaly detection
    -- on CloudWatch log groups.
    optInStatus :: Prelude.Maybe OptInStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogsAnomalyDetectionIntegrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optInStatus', 'logsAnomalyDetectionIntegrationConfig_optInStatus' - Specifies if DevOps Guru is configured to perform log anomaly detection
-- on CloudWatch log groups.
newLogsAnomalyDetectionIntegrationConfig ::
  LogsAnomalyDetectionIntegrationConfig
newLogsAnomalyDetectionIntegrationConfig =
  LogsAnomalyDetectionIntegrationConfig'
    { optInStatus =
        Prelude.Nothing
    }

-- | Specifies if DevOps Guru is configured to perform log anomaly detection
-- on CloudWatch log groups.
logsAnomalyDetectionIntegrationConfig_optInStatus :: Lens.Lens' LogsAnomalyDetectionIntegrationConfig (Prelude.Maybe OptInStatus)
logsAnomalyDetectionIntegrationConfig_optInStatus = Lens.lens (\LogsAnomalyDetectionIntegrationConfig' {optInStatus} -> optInStatus) (\s@LogsAnomalyDetectionIntegrationConfig' {} a -> s {optInStatus = a} :: LogsAnomalyDetectionIntegrationConfig)

instance
  Prelude.Hashable
    LogsAnomalyDetectionIntegrationConfig
  where
  hashWithSalt
    _salt
    LogsAnomalyDetectionIntegrationConfig' {..} =
      _salt `Prelude.hashWithSalt` optInStatus

instance
  Prelude.NFData
    LogsAnomalyDetectionIntegrationConfig
  where
  rnf LogsAnomalyDetectionIntegrationConfig' {..} =
    Prelude.rnf optInStatus

instance
  Data.ToJSON
    LogsAnomalyDetectionIntegrationConfig
  where
  toJSON LogsAnomalyDetectionIntegrationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("OptInStatus" Data..=) Prelude.<$> optInStatus]
      )
