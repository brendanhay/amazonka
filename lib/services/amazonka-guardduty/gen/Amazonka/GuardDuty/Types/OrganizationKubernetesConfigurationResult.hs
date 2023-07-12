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
-- Module      : Amazonka.GuardDuty.Types.OrganizationKubernetesConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationKubernetesConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrganizationKubernetesAuditLogsConfigurationResult
import qualified Amazonka.Prelude as Prelude

-- | The current configuration of all Kubernetes data sources for the
-- organization.
--
-- /See:/ 'newOrganizationKubernetesConfigurationResult' smart constructor.
data OrganizationKubernetesConfigurationResult = OrganizationKubernetesConfigurationResult'
  { -- | The current configuration of Kubernetes audit logs as a data source for
    -- the organization.
    auditLogs :: OrganizationKubernetesAuditLogsConfigurationResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationKubernetesConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogs', 'organizationKubernetesConfigurationResult_auditLogs' - The current configuration of Kubernetes audit logs as a data source for
-- the organization.
newOrganizationKubernetesConfigurationResult ::
  -- | 'auditLogs'
  OrganizationKubernetesAuditLogsConfigurationResult ->
  OrganizationKubernetesConfigurationResult
newOrganizationKubernetesConfigurationResult
  pAuditLogs_ =
    OrganizationKubernetesConfigurationResult'
      { auditLogs =
          pAuditLogs_
      }

-- | The current configuration of Kubernetes audit logs as a data source for
-- the organization.
organizationKubernetesConfigurationResult_auditLogs :: Lens.Lens' OrganizationKubernetesConfigurationResult OrganizationKubernetesAuditLogsConfigurationResult
organizationKubernetesConfigurationResult_auditLogs = Lens.lens (\OrganizationKubernetesConfigurationResult' {auditLogs} -> auditLogs) (\s@OrganizationKubernetesConfigurationResult' {} a -> s {auditLogs = a} :: OrganizationKubernetesConfigurationResult)

instance
  Data.FromJSON
    OrganizationKubernetesConfigurationResult
  where
  parseJSON =
    Data.withObject
      "OrganizationKubernetesConfigurationResult"
      ( \x ->
          OrganizationKubernetesConfigurationResult'
            Prelude.<$> (x Data..: "auditLogs")
      )

instance
  Prelude.Hashable
    OrganizationKubernetesConfigurationResult
  where
  hashWithSalt
    _salt
    OrganizationKubernetesConfigurationResult' {..} =
      _salt `Prelude.hashWithSalt` auditLogs

instance
  Prelude.NFData
    OrganizationKubernetesConfigurationResult
  where
  rnf OrganizationKubernetesConfigurationResult' {..} =
    Prelude.rnf auditLogs
