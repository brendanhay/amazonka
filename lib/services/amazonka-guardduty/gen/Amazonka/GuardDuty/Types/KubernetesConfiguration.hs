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
-- Module      : Amazonka.GuardDuty.Types.KubernetesConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.KubernetesAuditLogsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes whether any Kubernetes data sources are enabled.
--
-- /See:/ 'newKubernetesConfiguration' smart constructor.
data KubernetesConfiguration = KubernetesConfiguration'
  { -- | The status of Kubernetes audit logs as a data source.
    auditLogs :: KubernetesAuditLogsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogs', 'kubernetesConfiguration_auditLogs' - The status of Kubernetes audit logs as a data source.
newKubernetesConfiguration ::
  -- | 'auditLogs'
  KubernetesAuditLogsConfiguration ->
  KubernetesConfiguration
newKubernetesConfiguration pAuditLogs_ =
  KubernetesConfiguration' {auditLogs = pAuditLogs_}

-- | The status of Kubernetes audit logs as a data source.
kubernetesConfiguration_auditLogs :: Lens.Lens' KubernetesConfiguration KubernetesAuditLogsConfiguration
kubernetesConfiguration_auditLogs = Lens.lens (\KubernetesConfiguration' {auditLogs} -> auditLogs) (\s@KubernetesConfiguration' {} a -> s {auditLogs = a} :: KubernetesConfiguration)

instance Prelude.Hashable KubernetesConfiguration where
  hashWithSalt _salt KubernetesConfiguration' {..} =
    _salt `Prelude.hashWithSalt` auditLogs

instance Prelude.NFData KubernetesConfiguration where
  rnf KubernetesConfiguration' {..} =
    Prelude.rnf auditLogs

instance Data.ToJSON KubernetesConfiguration where
  toJSON KubernetesConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("auditLogs" Data..= auditLogs)]
      )
