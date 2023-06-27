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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ServiceIntegrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.KMSServerSideEncryptionIntegration
import Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegration
import Amazonka.DevOpsGuru.Types.OpsCenterIntegration
import qualified Amazonka.Prelude as Prelude

-- | Information about the integration of DevOps Guru with another Amazon Web
-- Services service, such as Amazon Web Services Systems Manager.
--
-- /See:/ 'newServiceIntegrationConfig' smart constructor.
data ServiceIntegrationConfig = ServiceIntegrationConfig'
  { -- | Information about whether DevOps Guru is configured to encrypt
    -- server-side data using KMS.
    kmsServerSideEncryption :: Prelude.Maybe KMSServerSideEncryptionIntegration,
    -- | Information about whether DevOps Guru is configured to perform log
    -- anomaly detection on Amazon CloudWatch log groups.
    logsAnomalyDetection :: Prelude.Maybe LogsAnomalyDetectionIntegration,
    -- | Information about whether DevOps Guru is configured to create an OpsItem
    -- in Amazon Web Services Systems Manager OpsCenter for each created
    -- insight.
    opsCenter :: Prelude.Maybe OpsCenterIntegration
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
-- 'kmsServerSideEncryption', 'serviceIntegrationConfig_kmsServerSideEncryption' - Information about whether DevOps Guru is configured to encrypt
-- server-side data using KMS.
--
-- 'logsAnomalyDetection', 'serviceIntegrationConfig_logsAnomalyDetection' - Information about whether DevOps Guru is configured to perform log
-- anomaly detection on Amazon CloudWatch log groups.
--
-- 'opsCenter', 'serviceIntegrationConfig_opsCenter' - Information about whether DevOps Guru is configured to create an OpsItem
-- in Amazon Web Services Systems Manager OpsCenter for each created
-- insight.
newServiceIntegrationConfig ::
  ServiceIntegrationConfig
newServiceIntegrationConfig =
  ServiceIntegrationConfig'
    { kmsServerSideEncryption =
        Prelude.Nothing,
      logsAnomalyDetection = Prelude.Nothing,
      opsCenter = Prelude.Nothing
    }

-- | Information about whether DevOps Guru is configured to encrypt
-- server-side data using KMS.
serviceIntegrationConfig_kmsServerSideEncryption :: Lens.Lens' ServiceIntegrationConfig (Prelude.Maybe KMSServerSideEncryptionIntegration)
serviceIntegrationConfig_kmsServerSideEncryption = Lens.lens (\ServiceIntegrationConfig' {kmsServerSideEncryption} -> kmsServerSideEncryption) (\s@ServiceIntegrationConfig' {} a -> s {kmsServerSideEncryption = a} :: ServiceIntegrationConfig)

-- | Information about whether DevOps Guru is configured to perform log
-- anomaly detection on Amazon CloudWatch log groups.
serviceIntegrationConfig_logsAnomalyDetection :: Lens.Lens' ServiceIntegrationConfig (Prelude.Maybe LogsAnomalyDetectionIntegration)
serviceIntegrationConfig_logsAnomalyDetection = Lens.lens (\ServiceIntegrationConfig' {logsAnomalyDetection} -> logsAnomalyDetection) (\s@ServiceIntegrationConfig' {} a -> s {logsAnomalyDetection = a} :: ServiceIntegrationConfig)

-- | Information about whether DevOps Guru is configured to create an OpsItem
-- in Amazon Web Services Systems Manager OpsCenter for each created
-- insight.
serviceIntegrationConfig_opsCenter :: Lens.Lens' ServiceIntegrationConfig (Prelude.Maybe OpsCenterIntegration)
serviceIntegrationConfig_opsCenter = Lens.lens (\ServiceIntegrationConfig' {opsCenter} -> opsCenter) (\s@ServiceIntegrationConfig' {} a -> s {opsCenter = a} :: ServiceIntegrationConfig)

instance Data.FromJSON ServiceIntegrationConfig where
  parseJSON =
    Data.withObject
      "ServiceIntegrationConfig"
      ( \x ->
          ServiceIntegrationConfig'
            Prelude.<$> (x Data..:? "KMSServerSideEncryption")
            Prelude.<*> (x Data..:? "LogsAnomalyDetection")
            Prelude.<*> (x Data..:? "OpsCenter")
      )

instance Prelude.Hashable ServiceIntegrationConfig where
  hashWithSalt _salt ServiceIntegrationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kmsServerSideEncryption
      `Prelude.hashWithSalt` logsAnomalyDetection
      `Prelude.hashWithSalt` opsCenter

instance Prelude.NFData ServiceIntegrationConfig where
  rnf ServiceIntegrationConfig' {..} =
    Prelude.rnf kmsServerSideEncryption
      `Prelude.seq` Prelude.rnf logsAnomalyDetection
      `Prelude.seq` Prelude.rnf opsCenter
