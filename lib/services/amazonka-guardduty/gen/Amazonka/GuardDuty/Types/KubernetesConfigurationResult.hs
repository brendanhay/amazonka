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
-- Module      : Amazonka.GuardDuty.Types.KubernetesConfigurationResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.KubernetesAuditLogsConfigurationResult
import qualified Amazonka.Prelude as Prelude

-- | Describes whether any Kubernetes logs will be enabled as a data source.
--
-- /See:/ 'newKubernetesConfigurationResult' smart constructor.
data KubernetesConfigurationResult = KubernetesConfigurationResult'
  { -- | Describes whether Kubernetes audit logs are enabled as a data source.
    auditLogs :: KubernetesAuditLogsConfigurationResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogs', 'kubernetesConfigurationResult_auditLogs' - Describes whether Kubernetes audit logs are enabled as a data source.
newKubernetesConfigurationResult ::
  -- | 'auditLogs'
  KubernetesAuditLogsConfigurationResult ->
  KubernetesConfigurationResult
newKubernetesConfigurationResult pAuditLogs_ =
  KubernetesConfigurationResult'
    { auditLogs =
        pAuditLogs_
    }

-- | Describes whether Kubernetes audit logs are enabled as a data source.
kubernetesConfigurationResult_auditLogs :: Lens.Lens' KubernetesConfigurationResult KubernetesAuditLogsConfigurationResult
kubernetesConfigurationResult_auditLogs = Lens.lens (\KubernetesConfigurationResult' {auditLogs} -> auditLogs) (\s@KubernetesConfigurationResult' {} a -> s {auditLogs = a} :: KubernetesConfigurationResult)

instance Data.FromJSON KubernetesConfigurationResult where
  parseJSON =
    Data.withObject
      "KubernetesConfigurationResult"
      ( \x ->
          KubernetesConfigurationResult'
            Prelude.<$> (x Data..: "auditLogs")
      )

instance
  Prelude.Hashable
    KubernetesConfigurationResult
  where
  hashWithSalt _salt KubernetesConfigurationResult' {..} =
    _salt `Prelude.hashWithSalt` auditLogs

instance Prelude.NFData KubernetesConfigurationResult where
  rnf KubernetesConfigurationResult' {..} =
    Prelude.rnf auditLogs
