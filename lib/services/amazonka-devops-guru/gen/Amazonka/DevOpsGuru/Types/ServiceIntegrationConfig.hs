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
-- Module      : Amazonka.DevOpsGuru.Types.ServiceIntegrationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ServiceIntegrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegration
import Amazonka.DevOpsGuru.Types.OpsCenterIntegration
import qualified Amazonka.Prelude as Prelude

-- | Information about the integration of DevOps Guru with another Amazon Web
-- Services service, such as Amazon Web Services Systems Manager.
--
-- /See:/ 'newServiceIntegrationConfig' smart constructor.
data ServiceIntegrationConfig = ServiceIntegrationConfig'
  { -- | Information about whether DevOps Guru is configured to create an OpsItem
    -- in Amazon Web Services Systems Manager OpsCenter for each created
    -- insight.
    opsCenter :: Prelude.Maybe OpsCenterIntegration,
    -- | Information about whether DevOps Guru is configured to perform log
    -- anomaly detection on Amazon CloudWatch log groups.
    logsAnomalyDetection :: Prelude.Maybe LogsAnomalyDetectionIntegration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceIntegrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsCenter', 'serviceIntegrationConfig_opsCenter' - Information about whether DevOps Guru is configured to create an OpsItem
-- in Amazon Web Services Systems Manager OpsCenter for each created
-- insight.
--
-- 'logsAnomalyDetection', 'serviceIntegrationConfig_logsAnomalyDetection' - Information about whether DevOps Guru is configured to perform log
-- anomaly detection on Amazon CloudWatch log groups.
newServiceIntegrationConfig ::
  ServiceIntegrationConfig
newServiceIntegrationConfig =
  ServiceIntegrationConfig'
    { opsCenter =
        Prelude.Nothing,
      logsAnomalyDetection = Prelude.Nothing
    }

-- | Information about whether DevOps Guru is configured to create an OpsItem
-- in Amazon Web Services Systems Manager OpsCenter for each created
-- insight.
serviceIntegrationConfig_opsCenter :: Lens.Lens' ServiceIntegrationConfig (Prelude.Maybe OpsCenterIntegration)
serviceIntegrationConfig_opsCenter = Lens.lens (\ServiceIntegrationConfig' {opsCenter} -> opsCenter) (\s@ServiceIntegrationConfig' {} a -> s {opsCenter = a} :: ServiceIntegrationConfig)

-- | Information about whether DevOps Guru is configured to perform log
-- anomaly detection on Amazon CloudWatch log groups.
serviceIntegrationConfig_logsAnomalyDetection :: Lens.Lens' ServiceIntegrationConfig (Prelude.Maybe LogsAnomalyDetectionIntegration)
serviceIntegrationConfig_logsAnomalyDetection = Lens.lens (\ServiceIntegrationConfig' {logsAnomalyDetection} -> logsAnomalyDetection) (\s@ServiceIntegrationConfig' {} a -> s {logsAnomalyDetection = a} :: ServiceIntegrationConfig)

instance Core.FromJSON ServiceIntegrationConfig where
  parseJSON =
    Core.withObject
      "ServiceIntegrationConfig"
      ( \x ->
          ServiceIntegrationConfig'
            Prelude.<$> (x Core..:? "OpsCenter")
            Prelude.<*> (x Core..:? "LogsAnomalyDetection")
      )

instance Prelude.Hashable ServiceIntegrationConfig where
  hashWithSalt _salt ServiceIntegrationConfig' {..} =
    _salt `Prelude.hashWithSalt` opsCenter
      `Prelude.hashWithSalt` logsAnomalyDetection

instance Prelude.NFData ServiceIntegrationConfig where
  rnf ServiceIntegrationConfig' {..} =
    Prelude.rnf opsCenter
      `Prelude.seq` Prelude.rnf logsAnomalyDetection
