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
-- Module      : Amazonka.GuardDuty.Types.OrganizationKubernetesAuditLogsConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationKubernetesAuditLogsConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current configuration of Kubernetes audit logs as a data source for
-- the organization.
--
-- /See:/ 'newOrganizationKubernetesAuditLogsConfigurationResult' smart constructor.
data OrganizationKubernetesAuditLogsConfigurationResult = OrganizationKubernetesAuditLogsConfigurationResult'
  { -- | Whether Kubernetes audit logs data source should be auto-enabled for new
    -- members joining the organization.
    autoEnable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationKubernetesAuditLogsConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'organizationKubernetesAuditLogsConfigurationResult_autoEnable' - Whether Kubernetes audit logs data source should be auto-enabled for new
-- members joining the organization.
newOrganizationKubernetesAuditLogsConfigurationResult ::
  -- | 'autoEnable'
  Prelude.Bool ->
  OrganizationKubernetesAuditLogsConfigurationResult
newOrganizationKubernetesAuditLogsConfigurationResult
  pAutoEnable_ =
    OrganizationKubernetesAuditLogsConfigurationResult'
      { autoEnable =
          pAutoEnable_
      }

-- | Whether Kubernetes audit logs data source should be auto-enabled for new
-- members joining the organization.
organizationKubernetesAuditLogsConfigurationResult_autoEnable :: Lens.Lens' OrganizationKubernetesAuditLogsConfigurationResult Prelude.Bool
organizationKubernetesAuditLogsConfigurationResult_autoEnable = Lens.lens (\OrganizationKubernetesAuditLogsConfigurationResult' {autoEnable} -> autoEnable) (\s@OrganizationKubernetesAuditLogsConfigurationResult' {} a -> s {autoEnable = a} :: OrganizationKubernetesAuditLogsConfigurationResult)

instance
  Data.FromJSON
    OrganizationKubernetesAuditLogsConfigurationResult
  where
  parseJSON =
    Data.withObject
      "OrganizationKubernetesAuditLogsConfigurationResult"
      ( \x ->
          OrganizationKubernetesAuditLogsConfigurationResult'
            Prelude.<$> (x Data..: "autoEnable")
      )

instance
  Prelude.Hashable
    OrganizationKubernetesAuditLogsConfigurationResult
  where
  hashWithSalt
    _salt
    OrganizationKubernetesAuditLogsConfigurationResult' {..} =
      _salt `Prelude.hashWithSalt` autoEnable

instance
  Prelude.NFData
    OrganizationKubernetesAuditLogsConfigurationResult
  where
  rnf
    OrganizationKubernetesAuditLogsConfigurationResult' {..} =
      Prelude.rnf autoEnable
