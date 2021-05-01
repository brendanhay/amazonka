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
-- Module      : Network.AWS.AlexaBusiness.Types.GatewaySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewaySummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary of a gateway.
--
-- /See:/ 'newGatewaySummary' smart constructor.
data GatewaySummary = GatewaySummary'
  { -- | The ARN of the gateway.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway group that the gateway is associated to.
    gatewayGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the gateway.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | The software version of the gateway. The gateway automatically updates
    -- its software version during normal operation.
    softwareVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { arn = Prelude.Nothing,
      gatewayGroupArn = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      softwareVersion = Prelude.Nothing
    }

-- | The ARN of the gateway.
gatewaySummary_arn :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_arn = Lens.lens (\GatewaySummary' {arn} -> arn) (\s@GatewaySummary' {} a -> s {arn = a} :: GatewaySummary)

-- | The ARN of the gateway group that the gateway is associated to.
gatewaySummary_gatewayGroupArn :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_gatewayGroupArn = Lens.lens (\GatewaySummary' {gatewayGroupArn} -> gatewayGroupArn) (\s@GatewaySummary' {} a -> s {gatewayGroupArn = a} :: GatewaySummary)

-- | The name of the gateway.
gatewaySummary_name :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_name = Lens.lens (\GatewaySummary' {name} -> name) (\s@GatewaySummary' {} a -> s {name = a} :: GatewaySummary)

-- | The description of the gateway.
gatewaySummary_description :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_description = Lens.lens (\GatewaySummary' {description} -> description) (\s@GatewaySummary' {} a -> s {description = a} :: GatewaySummary)

-- | The software version of the gateway. The gateway automatically updates
-- its software version during normal operation.
gatewaySummary_softwareVersion :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_softwareVersion = Lens.lens (\GatewaySummary' {softwareVersion} -> softwareVersion) (\s@GatewaySummary' {} a -> s {softwareVersion = a} :: GatewaySummary)

instance Prelude.FromJSON GatewaySummary where
  parseJSON =
    Prelude.withObject
      "GatewaySummary"
      ( \x ->
          GatewaySummary'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "GatewayGroupArn")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "SoftwareVersion")
      )

instance Prelude.Hashable GatewaySummary

instance Prelude.NFData GatewaySummary
