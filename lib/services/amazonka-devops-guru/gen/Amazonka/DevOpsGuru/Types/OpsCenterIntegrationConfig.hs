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
-- Module      : Amazonka.DevOpsGuru.Types.OpsCenterIntegrationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.OpsCenterIntegrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.OptInStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about whether DevOps Guru is configured to create an OpsItem
-- in Amazon Web Services Systems Manager OpsCenter for each created
-- insight. You can use this to update the configuration.
--
-- /See:/ 'newOpsCenterIntegrationConfig' smart constructor.
data OpsCenterIntegrationConfig = OpsCenterIntegrationConfig'
  { -- | Specifies if DevOps Guru is enabled to create an Amazon Web Services
    -- Systems Manager OpsItem for each created insight.
    optInStatus :: Prelude.Maybe OptInStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsCenterIntegrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optInStatus', 'opsCenterIntegrationConfig_optInStatus' - Specifies if DevOps Guru is enabled to create an Amazon Web Services
-- Systems Manager OpsItem for each created insight.
newOpsCenterIntegrationConfig ::
  OpsCenterIntegrationConfig
newOpsCenterIntegrationConfig =
  OpsCenterIntegrationConfig'
    { optInStatus =
        Prelude.Nothing
    }

-- | Specifies if DevOps Guru is enabled to create an Amazon Web Services
-- Systems Manager OpsItem for each created insight.
opsCenterIntegrationConfig_optInStatus :: Lens.Lens' OpsCenterIntegrationConfig (Prelude.Maybe OptInStatus)
opsCenterIntegrationConfig_optInStatus = Lens.lens (\OpsCenterIntegrationConfig' {optInStatus} -> optInStatus) (\s@OpsCenterIntegrationConfig' {} a -> s {optInStatus = a} :: OpsCenterIntegrationConfig)

instance Prelude.Hashable OpsCenterIntegrationConfig where
  hashWithSalt _salt OpsCenterIntegrationConfig' {..} =
    _salt `Prelude.hashWithSalt` optInStatus

instance Prelude.NFData OpsCenterIntegrationConfig where
  rnf OpsCenterIntegrationConfig' {..} =
    Prelude.rnf optInStatus

instance Data.ToJSON OpsCenterIntegrationConfig where
  toJSON OpsCenterIntegrationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("OptInStatus" Data..=) Prelude.<$> optInStatus]
      )
