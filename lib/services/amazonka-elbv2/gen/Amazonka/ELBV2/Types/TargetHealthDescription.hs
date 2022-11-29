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
-- Module      : Amazonka.ELBV2.Types.TargetHealthDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.TargetHealthDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types.TargetDescription
import Amazonka.ELBV2.Types.TargetHealth
import qualified Amazonka.Prelude as Prelude

-- | Information about the health of a target.
--
-- /See:/ 'newTargetHealthDescription' smart constructor.
data TargetHealthDescription = TargetHealthDescription'
  { -- | The health information for the target.
    targetHealth :: Prelude.Maybe TargetHealth,
    -- | The description of the target.
    target :: Prelude.Maybe TargetDescription,
    -- | The port to use to connect with the target.
    healthCheckPort :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetHealthDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetHealth', 'targetHealthDescription_targetHealth' - The health information for the target.
--
-- 'target', 'targetHealthDescription_target' - The description of the target.
--
-- 'healthCheckPort', 'targetHealthDescription_healthCheckPort' - The port to use to connect with the target.
newTargetHealthDescription ::
  TargetHealthDescription
newTargetHealthDescription =
  TargetHealthDescription'
    { targetHealth =
        Prelude.Nothing,
      target = Prelude.Nothing,
      healthCheckPort = Prelude.Nothing
    }

-- | The health information for the target.
targetHealthDescription_targetHealth :: Lens.Lens' TargetHealthDescription (Prelude.Maybe TargetHealth)
targetHealthDescription_targetHealth = Lens.lens (\TargetHealthDescription' {targetHealth} -> targetHealth) (\s@TargetHealthDescription' {} a -> s {targetHealth = a} :: TargetHealthDescription)

-- | The description of the target.
targetHealthDescription_target :: Lens.Lens' TargetHealthDescription (Prelude.Maybe TargetDescription)
targetHealthDescription_target = Lens.lens (\TargetHealthDescription' {target} -> target) (\s@TargetHealthDescription' {} a -> s {target = a} :: TargetHealthDescription)

-- | The port to use to connect with the target.
targetHealthDescription_healthCheckPort :: Lens.Lens' TargetHealthDescription (Prelude.Maybe Prelude.Text)
targetHealthDescription_healthCheckPort = Lens.lens (\TargetHealthDescription' {healthCheckPort} -> healthCheckPort) (\s@TargetHealthDescription' {} a -> s {healthCheckPort = a} :: TargetHealthDescription)

instance Core.FromXML TargetHealthDescription where
  parseXML x =
    TargetHealthDescription'
      Prelude.<$> (x Core..@? "TargetHealth")
      Prelude.<*> (x Core..@? "Target")
      Prelude.<*> (x Core..@? "HealthCheckPort")

instance Prelude.Hashable TargetHealthDescription where
  hashWithSalt _salt TargetHealthDescription' {..} =
    _salt `Prelude.hashWithSalt` targetHealth
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` healthCheckPort

instance Prelude.NFData TargetHealthDescription where
  rnf TargetHealthDescription' {..} =
    Prelude.rnf targetHealth
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf healthCheckPort
