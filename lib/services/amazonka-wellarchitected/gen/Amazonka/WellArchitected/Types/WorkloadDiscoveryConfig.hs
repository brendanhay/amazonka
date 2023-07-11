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
-- Module      : Amazonka.WellArchitected.Types.WorkloadDiscoveryConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.WorkloadDiscoveryConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.TrustedAdvisorIntegrationStatus

-- | Discovery configuration associated to the workload.
--
-- /See:/ 'newWorkloadDiscoveryConfig' smart constructor.
data WorkloadDiscoveryConfig = WorkloadDiscoveryConfig'
  { -- | Discovery integration status in respect to Trusted Advisor for the
    -- workload.
    trustedAdvisorIntegrationStatus :: Prelude.Maybe TrustedAdvisorIntegrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkloadDiscoveryConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustedAdvisorIntegrationStatus', 'workloadDiscoveryConfig_trustedAdvisorIntegrationStatus' - Discovery integration status in respect to Trusted Advisor for the
-- workload.
newWorkloadDiscoveryConfig ::
  WorkloadDiscoveryConfig
newWorkloadDiscoveryConfig =
  WorkloadDiscoveryConfig'
    { trustedAdvisorIntegrationStatus =
        Prelude.Nothing
    }

-- | Discovery integration status in respect to Trusted Advisor for the
-- workload.
workloadDiscoveryConfig_trustedAdvisorIntegrationStatus :: Lens.Lens' WorkloadDiscoveryConfig (Prelude.Maybe TrustedAdvisorIntegrationStatus)
workloadDiscoveryConfig_trustedAdvisorIntegrationStatus = Lens.lens (\WorkloadDiscoveryConfig' {trustedAdvisorIntegrationStatus} -> trustedAdvisorIntegrationStatus) (\s@WorkloadDiscoveryConfig' {} a -> s {trustedAdvisorIntegrationStatus = a} :: WorkloadDiscoveryConfig)

instance Data.FromJSON WorkloadDiscoveryConfig where
  parseJSON =
    Data.withObject
      "WorkloadDiscoveryConfig"
      ( \x ->
          WorkloadDiscoveryConfig'
            Prelude.<$> (x Data..:? "TrustedAdvisorIntegrationStatus")
      )

instance Prelude.Hashable WorkloadDiscoveryConfig where
  hashWithSalt _salt WorkloadDiscoveryConfig' {..} =
    _salt
      `Prelude.hashWithSalt` trustedAdvisorIntegrationStatus

instance Prelude.NFData WorkloadDiscoveryConfig where
  rnf WorkloadDiscoveryConfig' {..} =
    Prelude.rnf trustedAdvisorIntegrationStatus

instance Data.ToJSON WorkloadDiscoveryConfig where
  toJSON WorkloadDiscoveryConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TrustedAdvisorIntegrationStatus" Data..=)
              Prelude.<$> trustedAdvisorIntegrationStatus
          ]
      )
