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
-- Module      : Amazonka.GuardDuty.Types.KubernetesAuditLogsConfigurationResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesAuditLogsConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DataSourceStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes whether Kubernetes audit logs are enabled as a data source.
--
-- /See:/ 'newKubernetesAuditLogsConfigurationResult' smart constructor.
data KubernetesAuditLogsConfigurationResult = KubernetesAuditLogsConfigurationResult'
  { -- | A value that describes whether Kubernetes audit logs are enabled as a
    -- data source.
    status :: DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesAuditLogsConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'kubernetesAuditLogsConfigurationResult_status' - A value that describes whether Kubernetes audit logs are enabled as a
-- data source.
newKubernetesAuditLogsConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  KubernetesAuditLogsConfigurationResult
newKubernetesAuditLogsConfigurationResult pStatus_ =
  KubernetesAuditLogsConfigurationResult'
    { status =
        pStatus_
    }

-- | A value that describes whether Kubernetes audit logs are enabled as a
-- data source.
kubernetesAuditLogsConfigurationResult_status :: Lens.Lens' KubernetesAuditLogsConfigurationResult DataSourceStatus
kubernetesAuditLogsConfigurationResult_status = Lens.lens (\KubernetesAuditLogsConfigurationResult' {status} -> status) (\s@KubernetesAuditLogsConfigurationResult' {} a -> s {status = a} :: KubernetesAuditLogsConfigurationResult)

instance
  Data.FromJSON
    KubernetesAuditLogsConfigurationResult
  where
  parseJSON =
    Data.withObject
      "KubernetesAuditLogsConfigurationResult"
      ( \x ->
          KubernetesAuditLogsConfigurationResult'
            Prelude.<$> (x Data..: "status")
      )

instance
  Prelude.Hashable
    KubernetesAuditLogsConfigurationResult
  where
  hashWithSalt
    _salt
    KubernetesAuditLogsConfigurationResult' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    KubernetesAuditLogsConfigurationResult
  where
  rnf KubernetesAuditLogsConfigurationResult' {..} =
    Prelude.rnf status
