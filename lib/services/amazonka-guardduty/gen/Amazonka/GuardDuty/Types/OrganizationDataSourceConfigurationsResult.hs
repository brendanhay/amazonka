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
-- Module      : Amazonka.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationDataSourceConfigurationsResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrganizationKubernetesConfigurationResult
import Amazonka.GuardDuty.Types.OrganizationMalwareProtectionConfigurationResult
import Amazonka.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on which data sources are
-- automatically enabled for new members within the organization.
--
-- /See:/ 'newOrganizationDataSourceConfigurationsResult' smart constructor.
data OrganizationDataSourceConfigurationsResult = OrganizationDataSourceConfigurationsResult'
  { -- | Describes the configuration of Kubernetes data sources.
    kubernetes :: Prelude.Maybe OrganizationKubernetesConfigurationResult,
    -- | Describes the configuration of Malware Protection data source for an
    -- organization.
    malwareProtection :: Prelude.Maybe OrganizationMalwareProtectionConfigurationResult,
    -- | Describes whether S3 data event logs are enabled as a data source.
    s3Logs :: OrganizationS3LogsConfigurationResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationDataSourceConfigurationsResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kubernetes', 'organizationDataSourceConfigurationsResult_kubernetes' - Describes the configuration of Kubernetes data sources.
--
-- 'malwareProtection', 'organizationDataSourceConfigurationsResult_malwareProtection' - Describes the configuration of Malware Protection data source for an
-- organization.
--
-- 's3Logs', 'organizationDataSourceConfigurationsResult_s3Logs' - Describes whether S3 data event logs are enabled as a data source.
newOrganizationDataSourceConfigurationsResult ::
  -- | 's3Logs'
  OrganizationS3LogsConfigurationResult ->
  OrganizationDataSourceConfigurationsResult
newOrganizationDataSourceConfigurationsResult
  pS3Logs_ =
    OrganizationDataSourceConfigurationsResult'
      { kubernetes =
          Prelude.Nothing,
        malwareProtection =
          Prelude.Nothing,
        s3Logs = pS3Logs_
      }

-- | Describes the configuration of Kubernetes data sources.
organizationDataSourceConfigurationsResult_kubernetes :: Lens.Lens' OrganizationDataSourceConfigurationsResult (Prelude.Maybe OrganizationKubernetesConfigurationResult)
organizationDataSourceConfigurationsResult_kubernetes = Lens.lens (\OrganizationDataSourceConfigurationsResult' {kubernetes} -> kubernetes) (\s@OrganizationDataSourceConfigurationsResult' {} a -> s {kubernetes = a} :: OrganizationDataSourceConfigurationsResult)

-- | Describes the configuration of Malware Protection data source for an
-- organization.
organizationDataSourceConfigurationsResult_malwareProtection :: Lens.Lens' OrganizationDataSourceConfigurationsResult (Prelude.Maybe OrganizationMalwareProtectionConfigurationResult)
organizationDataSourceConfigurationsResult_malwareProtection = Lens.lens (\OrganizationDataSourceConfigurationsResult' {malwareProtection} -> malwareProtection) (\s@OrganizationDataSourceConfigurationsResult' {} a -> s {malwareProtection = a} :: OrganizationDataSourceConfigurationsResult)

-- | Describes whether S3 data event logs are enabled as a data source.
organizationDataSourceConfigurationsResult_s3Logs :: Lens.Lens' OrganizationDataSourceConfigurationsResult OrganizationS3LogsConfigurationResult
organizationDataSourceConfigurationsResult_s3Logs = Lens.lens (\OrganizationDataSourceConfigurationsResult' {s3Logs} -> s3Logs) (\s@OrganizationDataSourceConfigurationsResult' {} a -> s {s3Logs = a} :: OrganizationDataSourceConfigurationsResult)

instance
  Data.FromJSON
    OrganizationDataSourceConfigurationsResult
  where
  parseJSON =
    Data.withObject
      "OrganizationDataSourceConfigurationsResult"
      ( \x ->
          OrganizationDataSourceConfigurationsResult'
            Prelude.<$> (x Data..:? "kubernetes")
              Prelude.<*> (x Data..:? "malwareProtection")
              Prelude.<*> (x Data..: "s3Logs")
      )

instance
  Prelude.Hashable
    OrganizationDataSourceConfigurationsResult
  where
  hashWithSalt
    _salt
    OrganizationDataSourceConfigurationsResult' {..} =
      _salt `Prelude.hashWithSalt` kubernetes
        `Prelude.hashWithSalt` malwareProtection
        `Prelude.hashWithSalt` s3Logs

instance
  Prelude.NFData
    OrganizationDataSourceConfigurationsResult
  where
  rnf OrganizationDataSourceConfigurationsResult' {..} =
    Prelude.rnf kubernetes
      `Prelude.seq` Prelude.rnf malwareProtection
      `Prelude.seq` Prelude.rnf s3Logs
