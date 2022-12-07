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
-- Module      : Amazonka.GuardDuty.Types.OrganizationKubernetesAuditLogsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationKubernetesAuditLogsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Organization-wide Kubernetes audit logs configuration.
--
-- /See:/ 'newOrganizationKubernetesAuditLogsConfiguration' smart constructor.
data OrganizationKubernetesAuditLogsConfiguration = OrganizationKubernetesAuditLogsConfiguration'
  { -- | A value that contains information on whether Kubernetes audit logs
    -- should be enabled automatically as a data source for the organization.
    autoEnable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationKubernetesAuditLogsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'organizationKubernetesAuditLogsConfiguration_autoEnable' - A value that contains information on whether Kubernetes audit logs
-- should be enabled automatically as a data source for the organization.
newOrganizationKubernetesAuditLogsConfiguration ::
  -- | 'autoEnable'
  Prelude.Bool ->
  OrganizationKubernetesAuditLogsConfiguration
newOrganizationKubernetesAuditLogsConfiguration
  pAutoEnable_ =
    OrganizationKubernetesAuditLogsConfiguration'
      { autoEnable =
          pAutoEnable_
      }

-- | A value that contains information on whether Kubernetes audit logs
-- should be enabled automatically as a data source for the organization.
organizationKubernetesAuditLogsConfiguration_autoEnable :: Lens.Lens' OrganizationKubernetesAuditLogsConfiguration Prelude.Bool
organizationKubernetesAuditLogsConfiguration_autoEnable = Lens.lens (\OrganizationKubernetesAuditLogsConfiguration' {autoEnable} -> autoEnable) (\s@OrganizationKubernetesAuditLogsConfiguration' {} a -> s {autoEnable = a} :: OrganizationKubernetesAuditLogsConfiguration)

instance
  Prelude.Hashable
    OrganizationKubernetesAuditLogsConfiguration
  where
  hashWithSalt
    _salt
    OrganizationKubernetesAuditLogsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` autoEnable

instance
  Prelude.NFData
    OrganizationKubernetesAuditLogsConfiguration
  where
  rnf OrganizationKubernetesAuditLogsConfiguration' {..} =
    Prelude.rnf autoEnable

instance
  Data.ToJSON
    OrganizationKubernetesAuditLogsConfiguration
  where
  toJSON
    OrganizationKubernetesAuditLogsConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [Prelude.Just ("autoEnable" Data..= autoEnable)]
        )
