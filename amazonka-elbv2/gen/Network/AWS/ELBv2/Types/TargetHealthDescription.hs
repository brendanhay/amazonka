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
-- Module      : Network.AWS.ELBv2.Types.TargetHealthDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealthDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.TargetDescription
import Network.AWS.ELBv2.Types.TargetHealth
import qualified Network.AWS.Lens as Lens

-- | Information about the health of a target.
--
-- /See:/ 'newTargetHealthDescription' smart constructor.
data TargetHealthDescription = TargetHealthDescription'
  { -- | The port to use to connect with the target.
    healthCheckPort :: Core.Maybe Core.Text,
    -- | The description of the target.
    target :: Core.Maybe TargetDescription,
    -- | The health information for the target.
    targetHealth :: Core.Maybe TargetHealth
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetHealthDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckPort', 'targetHealthDescription_healthCheckPort' - The port to use to connect with the target.
--
-- 'target', 'targetHealthDescription_target' - The description of the target.
--
-- 'targetHealth', 'targetHealthDescription_targetHealth' - The health information for the target.
newTargetHealthDescription ::
  TargetHealthDescription
newTargetHealthDescription =
  TargetHealthDescription'
    { healthCheckPort =
        Core.Nothing,
      target = Core.Nothing,
      targetHealth = Core.Nothing
    }

-- | The port to use to connect with the target.
targetHealthDescription_healthCheckPort :: Lens.Lens' TargetHealthDescription (Core.Maybe Core.Text)
targetHealthDescription_healthCheckPort = Lens.lens (\TargetHealthDescription' {healthCheckPort} -> healthCheckPort) (\s@TargetHealthDescription' {} a -> s {healthCheckPort = a} :: TargetHealthDescription)

-- | The description of the target.
targetHealthDescription_target :: Lens.Lens' TargetHealthDescription (Core.Maybe TargetDescription)
targetHealthDescription_target = Lens.lens (\TargetHealthDescription' {target} -> target) (\s@TargetHealthDescription' {} a -> s {target = a} :: TargetHealthDescription)

-- | The health information for the target.
targetHealthDescription_targetHealth :: Lens.Lens' TargetHealthDescription (Core.Maybe TargetHealth)
targetHealthDescription_targetHealth = Lens.lens (\TargetHealthDescription' {targetHealth} -> targetHealth) (\s@TargetHealthDescription' {} a -> s {targetHealth = a} :: TargetHealthDescription)

instance Core.FromXML TargetHealthDescription where
  parseXML x =
    TargetHealthDescription'
      Core.<$> (x Core..@? "HealthCheckPort")
      Core.<*> (x Core..@? "Target")
      Core.<*> (x Core..@? "TargetHealth")

instance Core.Hashable TargetHealthDescription

instance Core.NFData TargetHealthDescription
