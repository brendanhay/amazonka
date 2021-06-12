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
-- Module      : Network.AWS.AlexaBusiness.Types.GatewaySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewaySummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The summary of a gateway.
--
-- /See:/ 'newGatewaySummary' smart constructor.
data GatewaySummary = GatewaySummary'
  { -- | The ARN of the gateway.
    arn :: Core.Maybe Core.Text,
    -- | The ARN of the gateway group that the gateway is associated to.
    gatewayGroupArn :: Core.Maybe Core.Text,
    -- | The name of the gateway.
    name :: Core.Maybe Core.Text,
    -- | The description of the gateway.
    description :: Core.Maybe Core.Text,
    -- | The software version of the gateway. The gateway automatically updates
    -- its software version during normal operation.
    softwareVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GatewaySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'gatewaySummary_arn' - The ARN of the gateway.
--
-- 'gatewayGroupArn', 'gatewaySummary_gatewayGroupArn' - The ARN of the gateway group that the gateway is associated to.
--
-- 'name', 'gatewaySummary_name' - The name of the gateway.
--
-- 'description', 'gatewaySummary_description' - The description of the gateway.
--
-- 'softwareVersion', 'gatewaySummary_softwareVersion' - The software version of the gateway. The gateway automatically updates
-- its software version during normal operation.
newGatewaySummary ::
  GatewaySummary
newGatewaySummary =
  GatewaySummary'
    { arn = Core.Nothing,
      gatewayGroupArn = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      softwareVersion = Core.Nothing
    }

-- | The ARN of the gateway.
gatewaySummary_arn :: Lens.Lens' GatewaySummary (Core.Maybe Core.Text)
gatewaySummary_arn = Lens.lens (\GatewaySummary' {arn} -> arn) (\s@GatewaySummary' {} a -> s {arn = a} :: GatewaySummary)

-- | The ARN of the gateway group that the gateway is associated to.
gatewaySummary_gatewayGroupArn :: Lens.Lens' GatewaySummary (Core.Maybe Core.Text)
gatewaySummary_gatewayGroupArn = Lens.lens (\GatewaySummary' {gatewayGroupArn} -> gatewayGroupArn) (\s@GatewaySummary' {} a -> s {gatewayGroupArn = a} :: GatewaySummary)

-- | The name of the gateway.
gatewaySummary_name :: Lens.Lens' GatewaySummary (Core.Maybe Core.Text)
gatewaySummary_name = Lens.lens (\GatewaySummary' {name} -> name) (\s@GatewaySummary' {} a -> s {name = a} :: GatewaySummary)

-- | The description of the gateway.
gatewaySummary_description :: Lens.Lens' GatewaySummary (Core.Maybe Core.Text)
gatewaySummary_description = Lens.lens (\GatewaySummary' {description} -> description) (\s@GatewaySummary' {} a -> s {description = a} :: GatewaySummary)

-- | The software version of the gateway. The gateway automatically updates
-- its software version during normal operation.
gatewaySummary_softwareVersion :: Lens.Lens' GatewaySummary (Core.Maybe Core.Text)
gatewaySummary_softwareVersion = Lens.lens (\GatewaySummary' {softwareVersion} -> softwareVersion) (\s@GatewaySummary' {} a -> s {softwareVersion = a} :: GatewaySummary)

instance Core.FromJSON GatewaySummary where
  parseJSON =
    Core.withObject
      "GatewaySummary"
      ( \x ->
          GatewaySummary'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "GatewayGroupArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SoftwareVersion")
      )

instance Core.Hashable GatewaySummary

instance Core.NFData GatewaySummary
