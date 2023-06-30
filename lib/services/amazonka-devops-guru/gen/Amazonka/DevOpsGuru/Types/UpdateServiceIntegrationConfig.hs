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
-- Module      : Amazonka.DevOpsGuru.Types.UpdateServiceIntegrationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.UpdateServiceIntegrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegrationConfig
import Amazonka.DevOpsGuru.Types.OpsCenterIntegrationConfig
import qualified Amazonka.Prelude as Prelude

-- | Information about updating the integration status of an Amazon Web
-- Services service, such as Amazon Web Services Systems Manager, with
-- DevOps Guru.
--
-- /See:/ 'newUpdateServiceIntegrationConfig' smart constructor.
data UpdateServiceIntegrationConfig = UpdateServiceIntegrationConfig'
  { -- | Information about whether DevOps Guru is configured to perform log
    -- anomaly detection on Amazon CloudWatch log groups.
    logsAnomalyDetection :: Prelude.Maybe LogsAnomalyDetectionIntegrationConfig,
    opsCenter :: Prelude.Maybe OpsCenterIntegrationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceIntegrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logsAnomalyDetection', 'updateServiceIntegrationConfig_logsAnomalyDetection' - Information about whether DevOps Guru is configured to perform log
-- anomaly detection on Amazon CloudWatch log groups.
--
-- 'opsCenter', 'updateServiceIntegrationConfig_opsCenter' - Undocumented member.
newUpdateServiceIntegrationConfig ::
  UpdateServiceIntegrationConfig
newUpdateServiceIntegrationConfig =
  UpdateServiceIntegrationConfig'
    { logsAnomalyDetection =
        Prelude.Nothing,
      opsCenter = Prelude.Nothing
    }

-- | Information about whether DevOps Guru is configured to perform log
-- anomaly detection on Amazon CloudWatch log groups.
updateServiceIntegrationConfig_logsAnomalyDetection :: Lens.Lens' UpdateServiceIntegrationConfig (Prelude.Maybe LogsAnomalyDetectionIntegrationConfig)
updateServiceIntegrationConfig_logsAnomalyDetection = Lens.lens (\UpdateServiceIntegrationConfig' {logsAnomalyDetection} -> logsAnomalyDetection) (\s@UpdateServiceIntegrationConfig' {} a -> s {logsAnomalyDetection = a} :: UpdateServiceIntegrationConfig)

-- | Undocumented member.
updateServiceIntegrationConfig_opsCenter :: Lens.Lens' UpdateServiceIntegrationConfig (Prelude.Maybe OpsCenterIntegrationConfig)
updateServiceIntegrationConfig_opsCenter = Lens.lens (\UpdateServiceIntegrationConfig' {opsCenter} -> opsCenter) (\s@UpdateServiceIntegrationConfig' {} a -> s {opsCenter = a} :: UpdateServiceIntegrationConfig)

instance
  Prelude.Hashable
    UpdateServiceIntegrationConfig
  where
  hashWithSalt
    _salt
    UpdateServiceIntegrationConfig' {..} =
      _salt
        `Prelude.hashWithSalt` logsAnomalyDetection
        `Prelude.hashWithSalt` opsCenter

instance
  Prelude.NFData
    UpdateServiceIntegrationConfig
  where
  rnf UpdateServiceIntegrationConfig' {..} =
    Prelude.rnf logsAnomalyDetection
      `Prelude.seq` Prelude.rnf opsCenter

instance Data.ToJSON UpdateServiceIntegrationConfig where
  toJSON UpdateServiceIntegrationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogsAnomalyDetection" Data..=)
              Prelude.<$> logsAnomalyDetection,
            ("OpsCenter" Data..=) Prelude.<$> opsCenter
          ]
      )
