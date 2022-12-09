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
-- Module      : Amazonka.GuardDuty.Types.OrganizationDataSourceConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationDataSourceConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrganizationKubernetesConfiguration
import Amazonka.GuardDuty.Types.OrganizationMalwareProtectionConfiguration
import Amazonka.GuardDuty.Types.OrganizationS3LogsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on which data sources will be
-- configured to be automatically enabled for new members within the
-- organization.
--
-- /See:/ 'newOrganizationDataSourceConfigurations' smart constructor.
data OrganizationDataSourceConfigurations = OrganizationDataSourceConfigurations'
  { -- | Describes the configuration of Kubernetes data sources for new members
    -- of the organization.
    kubernetes :: Prelude.Maybe OrganizationKubernetesConfiguration,
    -- | Describes the configuration of Malware Protection for new members of the
    -- organization.
    malwareProtection :: Prelude.Maybe OrganizationMalwareProtectionConfiguration,
    -- | Describes whether S3 data event logs are enabled for new members of the
    -- organization.
    s3Logs :: Prelude.Maybe OrganizationS3LogsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationDataSourceConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kubernetes', 'organizationDataSourceConfigurations_kubernetes' - Describes the configuration of Kubernetes data sources for new members
-- of the organization.
--
-- 'malwareProtection', 'organizationDataSourceConfigurations_malwareProtection' - Describes the configuration of Malware Protection for new members of the
-- organization.
--
-- 's3Logs', 'organizationDataSourceConfigurations_s3Logs' - Describes whether S3 data event logs are enabled for new members of the
-- organization.
newOrganizationDataSourceConfigurations ::
  OrganizationDataSourceConfigurations
newOrganizationDataSourceConfigurations =
  OrganizationDataSourceConfigurations'
    { kubernetes =
        Prelude.Nothing,
      malwareProtection = Prelude.Nothing,
      s3Logs = Prelude.Nothing
    }

-- | Describes the configuration of Kubernetes data sources for new members
-- of the organization.
organizationDataSourceConfigurations_kubernetes :: Lens.Lens' OrganizationDataSourceConfigurations (Prelude.Maybe OrganizationKubernetesConfiguration)
organizationDataSourceConfigurations_kubernetes = Lens.lens (\OrganizationDataSourceConfigurations' {kubernetes} -> kubernetes) (\s@OrganizationDataSourceConfigurations' {} a -> s {kubernetes = a} :: OrganizationDataSourceConfigurations)

-- | Describes the configuration of Malware Protection for new members of the
-- organization.
organizationDataSourceConfigurations_malwareProtection :: Lens.Lens' OrganizationDataSourceConfigurations (Prelude.Maybe OrganizationMalwareProtectionConfiguration)
organizationDataSourceConfigurations_malwareProtection = Lens.lens (\OrganizationDataSourceConfigurations' {malwareProtection} -> malwareProtection) (\s@OrganizationDataSourceConfigurations' {} a -> s {malwareProtection = a} :: OrganizationDataSourceConfigurations)

-- | Describes whether S3 data event logs are enabled for new members of the
-- organization.
organizationDataSourceConfigurations_s3Logs :: Lens.Lens' OrganizationDataSourceConfigurations (Prelude.Maybe OrganizationS3LogsConfiguration)
organizationDataSourceConfigurations_s3Logs = Lens.lens (\OrganizationDataSourceConfigurations' {s3Logs} -> s3Logs) (\s@OrganizationDataSourceConfigurations' {} a -> s {s3Logs = a} :: OrganizationDataSourceConfigurations)

instance
  Prelude.Hashable
    OrganizationDataSourceConfigurations
  where
  hashWithSalt
    _salt
    OrganizationDataSourceConfigurations' {..} =
      _salt `Prelude.hashWithSalt` kubernetes
        `Prelude.hashWithSalt` malwareProtection
        `Prelude.hashWithSalt` s3Logs

instance
  Prelude.NFData
    OrganizationDataSourceConfigurations
  where
  rnf OrganizationDataSourceConfigurations' {..} =
    Prelude.rnf kubernetes
      `Prelude.seq` Prelude.rnf malwareProtection
      `Prelude.seq` Prelude.rnf s3Logs

instance
  Data.ToJSON
    OrganizationDataSourceConfigurations
  where
  toJSON OrganizationDataSourceConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kubernetes" Data..=) Prelude.<$> kubernetes,
            ("malwareProtection" Data..=)
              Prelude.<$> malwareProtection,
            ("s3Logs" Data..=) Prelude.<$> s3Logs
          ]
      )
