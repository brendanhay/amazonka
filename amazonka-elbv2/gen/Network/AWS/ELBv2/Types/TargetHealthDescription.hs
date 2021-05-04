{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ELBv2.Types.TargetDescription
import Network.AWS.ELBv2.Types.TargetHealth
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the health of a target.
--
-- /See:/ 'newTargetHealthDescription' smart constructor.
data TargetHealthDescription = TargetHealthDescription'
  { -- | The port to use to connect with the target.
    healthCheckPort :: Prelude.Maybe Prelude.Text,
    -- | The description of the target.
    target :: Prelude.Maybe TargetDescription,
    -- | The health information for the target.
    targetHealth :: Prelude.Maybe TargetHealth
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      target = Prelude.Nothing,
      targetHealth = Prelude.Nothing
    }

-- | The port to use to connect with the target.
targetHealthDescription_healthCheckPort :: Lens.Lens' TargetHealthDescription (Prelude.Maybe Prelude.Text)
targetHealthDescription_healthCheckPort = Lens.lens (\TargetHealthDescription' {healthCheckPort} -> healthCheckPort) (\s@TargetHealthDescription' {} a -> s {healthCheckPort = a} :: TargetHealthDescription)

-- | The description of the target.
targetHealthDescription_target :: Lens.Lens' TargetHealthDescription (Prelude.Maybe TargetDescription)
targetHealthDescription_target = Lens.lens (\TargetHealthDescription' {target} -> target) (\s@TargetHealthDescription' {} a -> s {target = a} :: TargetHealthDescription)

-- | The health information for the target.
targetHealthDescription_targetHealth :: Lens.Lens' TargetHealthDescription (Prelude.Maybe TargetHealth)
targetHealthDescription_targetHealth = Lens.lens (\TargetHealthDescription' {targetHealth} -> targetHealth) (\s@TargetHealthDescription' {} a -> s {targetHealth = a} :: TargetHealthDescription)

instance Prelude.FromXML TargetHealthDescription where
  parseXML x =
    TargetHealthDescription'
      Prelude.<$> (x Prelude..@? "HealthCheckPort")
      Prelude.<*> (x Prelude..@? "Target")
      Prelude.<*> (x Prelude..@? "TargetHealth")

instance Prelude.Hashable TargetHealthDescription

instance Prelude.NFData TargetHealthDescription
