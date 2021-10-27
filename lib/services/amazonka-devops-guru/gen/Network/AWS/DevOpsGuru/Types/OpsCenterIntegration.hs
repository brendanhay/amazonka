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
-- Module      : Network.AWS.DevOpsGuru.Types.OpsCenterIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.OpsCenterIntegration where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types.OptInStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about whether DevOps Guru is configured to create an OpsItem
-- in AWS Systems Manager OpsCenter for each created insight.
--
-- /See:/ 'newOpsCenterIntegration' smart constructor.
data OpsCenterIntegration = OpsCenterIntegration'
  { -- | Specifies if DevOps Guru is enabled to create an AWS Systems Manager
    -- OpsItem for each created insight.
    optInStatus :: Prelude.Maybe OptInStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsCenterIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optInStatus', 'opsCenterIntegration_optInStatus' - Specifies if DevOps Guru is enabled to create an AWS Systems Manager
-- OpsItem for each created insight.
newOpsCenterIntegration ::
  OpsCenterIntegration
newOpsCenterIntegration =
  OpsCenterIntegration'
    { optInStatus =
        Prelude.Nothing
    }

-- | Specifies if DevOps Guru is enabled to create an AWS Systems Manager
-- OpsItem for each created insight.
opsCenterIntegration_optInStatus :: Lens.Lens' OpsCenterIntegration (Prelude.Maybe OptInStatus)
opsCenterIntegration_optInStatus = Lens.lens (\OpsCenterIntegration' {optInStatus} -> optInStatus) (\s@OpsCenterIntegration' {} a -> s {optInStatus = a} :: OpsCenterIntegration)

instance Core.FromJSON OpsCenterIntegration where
  parseJSON =
    Core.withObject
      "OpsCenterIntegration"
      ( \x ->
          OpsCenterIntegration'
            Prelude.<$> (x Core..:? "OptInStatus")
      )

instance Prelude.Hashable OpsCenterIntegration

instance Prelude.NFData OpsCenterIntegration
