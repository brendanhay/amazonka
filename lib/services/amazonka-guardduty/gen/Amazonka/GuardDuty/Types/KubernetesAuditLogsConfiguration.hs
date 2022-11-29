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
-- Module      : Amazonka.GuardDuty.Types.KubernetesAuditLogsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesAuditLogsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes whether Kubernetes audit logs are enabled as a data source.
--
-- /See:/ 'newKubernetesAuditLogsConfiguration' smart constructor.
data KubernetesAuditLogsConfiguration = KubernetesAuditLogsConfiguration'
  { -- | The status of Kubernetes audit logs as a data source.
    enable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesAuditLogsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enable', 'kubernetesAuditLogsConfiguration_enable' - The status of Kubernetes audit logs as a data source.
newKubernetesAuditLogsConfiguration ::
  -- | 'enable'
  Prelude.Bool ->
  KubernetesAuditLogsConfiguration
newKubernetesAuditLogsConfiguration pEnable_ =
  KubernetesAuditLogsConfiguration'
    { enable =
        pEnable_
    }

-- | The status of Kubernetes audit logs as a data source.
kubernetesAuditLogsConfiguration_enable :: Lens.Lens' KubernetesAuditLogsConfiguration Prelude.Bool
kubernetesAuditLogsConfiguration_enable = Lens.lens (\KubernetesAuditLogsConfiguration' {enable} -> enable) (\s@KubernetesAuditLogsConfiguration' {} a -> s {enable = a} :: KubernetesAuditLogsConfiguration)

instance
  Prelude.Hashable
    KubernetesAuditLogsConfiguration
  where
  hashWithSalt
    _salt
    KubernetesAuditLogsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` enable

instance
  Prelude.NFData
    KubernetesAuditLogsConfiguration
  where
  rnf KubernetesAuditLogsConfiguration' {..} =
    Prelude.rnf enable

instance Core.ToJSON KubernetesAuditLogsConfiguration where
  toJSON KubernetesAuditLogsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("enable" Core..= enable)]
      )
