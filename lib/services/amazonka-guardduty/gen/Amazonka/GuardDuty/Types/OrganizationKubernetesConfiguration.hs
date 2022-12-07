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
-- Module      : Amazonka.GuardDuty.Types.OrganizationKubernetesConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationKubernetesConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrganizationKubernetesAuditLogsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Organization-wide Kubernetes data sources configurations.
--
-- /See:/ 'newOrganizationKubernetesConfiguration' smart constructor.
data OrganizationKubernetesConfiguration = OrganizationKubernetesConfiguration'
  { -- | Whether Kubernetes audit logs data source should be auto-enabled for new
    -- members joining the organization.
    auditLogs :: OrganizationKubernetesAuditLogsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationKubernetesConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogs', 'organizationKubernetesConfiguration_auditLogs' - Whether Kubernetes audit logs data source should be auto-enabled for new
-- members joining the organization.
newOrganizationKubernetesConfiguration ::
  -- | 'auditLogs'
  OrganizationKubernetesAuditLogsConfiguration ->
  OrganizationKubernetesConfiguration
newOrganizationKubernetesConfiguration pAuditLogs_ =
  OrganizationKubernetesConfiguration'
    { auditLogs =
        pAuditLogs_
    }

-- | Whether Kubernetes audit logs data source should be auto-enabled for new
-- members joining the organization.
organizationKubernetesConfiguration_auditLogs :: Lens.Lens' OrganizationKubernetesConfiguration OrganizationKubernetesAuditLogsConfiguration
organizationKubernetesConfiguration_auditLogs = Lens.lens (\OrganizationKubernetesConfiguration' {auditLogs} -> auditLogs) (\s@OrganizationKubernetesConfiguration' {} a -> s {auditLogs = a} :: OrganizationKubernetesConfiguration)

instance
  Prelude.Hashable
    OrganizationKubernetesConfiguration
  where
  hashWithSalt
    _salt
    OrganizationKubernetesConfiguration' {..} =
      _salt `Prelude.hashWithSalt` auditLogs

instance
  Prelude.NFData
    OrganizationKubernetesConfiguration
  where
  rnf OrganizationKubernetesConfiguration' {..} =
    Prelude.rnf auditLogs

instance
  Data.ToJSON
    OrganizationKubernetesConfiguration
  where
  toJSON OrganizationKubernetesConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("auditLogs" Data..= auditLogs)]
      )
