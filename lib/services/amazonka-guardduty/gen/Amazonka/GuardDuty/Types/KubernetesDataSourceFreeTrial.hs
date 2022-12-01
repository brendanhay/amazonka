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
-- Module      : Amazonka.GuardDuty.Types.KubernetesDataSourceFreeTrial
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesDataSourceFreeTrial where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.DataSourceFreeTrial
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the Kubernetes resources when it is enabled as a
-- data source.
--
-- /See:/ 'newKubernetesDataSourceFreeTrial' smart constructor.
data KubernetesDataSourceFreeTrial = KubernetesDataSourceFreeTrial'
  { -- | Describes whether Kubernetes audit logs are enabled as a data source.
    auditLogs :: Prelude.Maybe DataSourceFreeTrial
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesDataSourceFreeTrial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogs', 'kubernetesDataSourceFreeTrial_auditLogs' - Describes whether Kubernetes audit logs are enabled as a data source.
newKubernetesDataSourceFreeTrial ::
  KubernetesDataSourceFreeTrial
newKubernetesDataSourceFreeTrial =
  KubernetesDataSourceFreeTrial'
    { auditLogs =
        Prelude.Nothing
    }

-- | Describes whether Kubernetes audit logs are enabled as a data source.
kubernetesDataSourceFreeTrial_auditLogs :: Lens.Lens' KubernetesDataSourceFreeTrial (Prelude.Maybe DataSourceFreeTrial)
kubernetesDataSourceFreeTrial_auditLogs = Lens.lens (\KubernetesDataSourceFreeTrial' {auditLogs} -> auditLogs) (\s@KubernetesDataSourceFreeTrial' {} a -> s {auditLogs = a} :: KubernetesDataSourceFreeTrial)

instance Core.FromJSON KubernetesDataSourceFreeTrial where
  parseJSON =
    Core.withObject
      "KubernetesDataSourceFreeTrial"
      ( \x ->
          KubernetesDataSourceFreeTrial'
            Prelude.<$> (x Core..:? "auditLogs")
      )

instance
  Prelude.Hashable
    KubernetesDataSourceFreeTrial
  where
  hashWithSalt _salt KubernetesDataSourceFreeTrial' {..} =
    _salt `Prelude.hashWithSalt` auditLogs

instance Prelude.NFData KubernetesDataSourceFreeTrial where
  rnf KubernetesDataSourceFreeTrial' {..} =
    Prelude.rnf auditLogs
