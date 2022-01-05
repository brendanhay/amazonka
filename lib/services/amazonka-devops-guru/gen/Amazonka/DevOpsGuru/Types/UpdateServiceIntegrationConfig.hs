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
-- Module      : Amazonka.DevOpsGuru.Types.UpdateServiceIntegrationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.UpdateServiceIntegrationConfig where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.OpsCenterIntegrationConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about updating the integration status of an AWS service,
-- such as AWS Systems Manager, with DevOps Guru.
--
-- /See:/ 'newUpdateServiceIntegrationConfig' smart constructor.
data UpdateServiceIntegrationConfig = UpdateServiceIntegrationConfig'
  { opsCenter :: Prelude.Maybe OpsCenterIntegrationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceIntegrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsCenter', 'updateServiceIntegrationConfig_opsCenter' - Undocumented member.
newUpdateServiceIntegrationConfig ::
  UpdateServiceIntegrationConfig
newUpdateServiceIntegrationConfig =
  UpdateServiceIntegrationConfig'
    { opsCenter =
        Prelude.Nothing
    }

-- | Undocumented member.
updateServiceIntegrationConfig_opsCenter :: Lens.Lens' UpdateServiceIntegrationConfig (Prelude.Maybe OpsCenterIntegrationConfig)
updateServiceIntegrationConfig_opsCenter = Lens.lens (\UpdateServiceIntegrationConfig' {opsCenter} -> opsCenter) (\s@UpdateServiceIntegrationConfig' {} a -> s {opsCenter = a} :: UpdateServiceIntegrationConfig)

instance
  Prelude.Hashable
    UpdateServiceIntegrationConfig
  where
  hashWithSalt
    _salt
    UpdateServiceIntegrationConfig' {..} =
      _salt `Prelude.hashWithSalt` opsCenter

instance
  Prelude.NFData
    UpdateServiceIntegrationConfig
  where
  rnf UpdateServiceIntegrationConfig' {..} =
    Prelude.rnf opsCenter

instance Core.ToJSON UpdateServiceIntegrationConfig where
  toJSON UpdateServiceIntegrationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [("OpsCenter" Core..=) Prelude.<$> opsCenter]
      )
