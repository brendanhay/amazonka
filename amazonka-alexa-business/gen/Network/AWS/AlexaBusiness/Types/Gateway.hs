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
-- Module      : Network.AWS.AlexaBusiness.Types.Gateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Gateway where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details of the gateway.
--
-- /See:/ 'newGateway' smart constructor.
data Gateway = Gateway'
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
-- Create a value of 'Gateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'gateway_arn' - The ARN of the gateway.
--
-- 'gatewayGroupArn', 'gateway_gatewayGroupArn' - The ARN of the gateway group that the gateway is associated to.
--
-- 'name', 'gateway_name' - The name of the gateway.
--
-- 'description', 'gateway_description' - The description of the gateway.
--
-- 'softwareVersion', 'gateway_softwareVersion' - The software version of the gateway. The gateway automatically updates
-- its software version during normal operation.
newGateway ::
  Gateway
newGateway =
  Gateway'
    { arn = Core.Nothing,
      gatewayGroupArn = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      softwareVersion = Core.Nothing
    }

-- | The ARN of the gateway.
gateway_arn :: Lens.Lens' Gateway (Core.Maybe Core.Text)
gateway_arn = Lens.lens (\Gateway' {arn} -> arn) (\s@Gateway' {} a -> s {arn = a} :: Gateway)

-- | The ARN of the gateway group that the gateway is associated to.
gateway_gatewayGroupArn :: Lens.Lens' Gateway (Core.Maybe Core.Text)
gateway_gatewayGroupArn = Lens.lens (\Gateway' {gatewayGroupArn} -> gatewayGroupArn) (\s@Gateway' {} a -> s {gatewayGroupArn = a} :: Gateway)

-- | The name of the gateway.
gateway_name :: Lens.Lens' Gateway (Core.Maybe Core.Text)
gateway_name = Lens.lens (\Gateway' {name} -> name) (\s@Gateway' {} a -> s {name = a} :: Gateway)

-- | The description of the gateway.
gateway_description :: Lens.Lens' Gateway (Core.Maybe Core.Text)
gateway_description = Lens.lens (\Gateway' {description} -> description) (\s@Gateway' {} a -> s {description = a} :: Gateway)

-- | The software version of the gateway. The gateway automatically updates
-- its software version during normal operation.
gateway_softwareVersion :: Lens.Lens' Gateway (Core.Maybe Core.Text)
gateway_softwareVersion = Lens.lens (\Gateway' {softwareVersion} -> softwareVersion) (\s@Gateway' {} a -> s {softwareVersion = a} :: Gateway)

instance Core.FromJSON Gateway where
  parseJSON =
    Core.withObject
      "Gateway"
      ( \x ->
          Gateway'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "GatewayGroupArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SoftwareVersion")
      )

instance Core.Hashable Gateway

instance Core.NFData Gateway
