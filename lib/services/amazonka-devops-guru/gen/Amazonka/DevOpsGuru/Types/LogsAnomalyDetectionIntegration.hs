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
-- Module      : Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.OptInStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the integration of DevOps Guru with CloudWatch log
-- groups for log anomaly detection.
--
-- /See:/ 'newLogsAnomalyDetectionIntegration' smart constructor.
data LogsAnomalyDetectionIntegration = LogsAnomalyDetectionIntegration'
  { -- | Specifies if DevOps Guru is configured to perform log anomaly detection
    -- on CloudWatch log groups.
    optInStatus :: Prelude.Maybe OptInStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogsAnomalyDetectionIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optInStatus', 'logsAnomalyDetectionIntegration_optInStatus' - Specifies if DevOps Guru is configured to perform log anomaly detection
-- on CloudWatch log groups.
newLogsAnomalyDetectionIntegration ::
  LogsAnomalyDetectionIntegration
newLogsAnomalyDetectionIntegration =
  LogsAnomalyDetectionIntegration'
    { optInStatus =
        Prelude.Nothing
    }

-- | Specifies if DevOps Guru is configured to perform log anomaly detection
-- on CloudWatch log groups.
logsAnomalyDetectionIntegration_optInStatus :: Lens.Lens' LogsAnomalyDetectionIntegration (Prelude.Maybe OptInStatus)
logsAnomalyDetectionIntegration_optInStatus = Lens.lens (\LogsAnomalyDetectionIntegration' {optInStatus} -> optInStatus) (\s@LogsAnomalyDetectionIntegration' {} a -> s {optInStatus = a} :: LogsAnomalyDetectionIntegration)

instance
  Core.FromJSON
    LogsAnomalyDetectionIntegration
  where
  parseJSON =
    Core.withObject
      "LogsAnomalyDetectionIntegration"
      ( \x ->
          LogsAnomalyDetectionIntegration'
            Prelude.<$> (x Core..:? "OptInStatus")
      )

instance
  Prelude.Hashable
    LogsAnomalyDetectionIntegration
  where
  hashWithSalt
    _salt
    LogsAnomalyDetectionIntegration' {..} =
      _salt `Prelude.hashWithSalt` optInStatus

instance
  Prelude.NFData
    LogsAnomalyDetectionIntegration
  where
  rnf LogsAnomalyDetectionIntegration' {..} =
    Prelude.rnf optInStatus
