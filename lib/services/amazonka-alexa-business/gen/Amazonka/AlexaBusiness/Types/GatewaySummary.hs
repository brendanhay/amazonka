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
-- Module      : Amazonka.AlexaBusiness.Types.GatewaySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.GatewaySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary of a gateway.
--
-- /See:/ 'newGatewaySummary' smart constructor.
data GatewaySummary = GatewaySummary'
  { -- | The name of the gateway.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway group that the gateway is associated to.
    gatewayGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | The software version of the gateway. The gateway automatically updates
    -- its software version during normal operation.
    softwareVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewaySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'gatewaySummary_name' - The name of the gateway.
--
-- 'gatewayGroupArn', 'gatewaySummary_gatewayGroupArn' - The ARN of the gateway group that the gateway is associated to.
--
-- 'arn', 'gatewaySummary_arn' - The ARN of the gateway.
--
-- 'description', 'gatewaySummary_description' - The description of the gateway.
--
-- 'softwareVersion', 'gatewaySummary_softwareVersion' - The software version of the gateway. The gateway automatically updates
-- its software version during normal operation.
newGatewaySummary ::
  GatewaySummary
newGatewaySummary =
  GatewaySummary'
    { name = Prelude.Nothing,
      gatewayGroupArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      softwareVersion = Prelude.Nothing
    }

-- | The name of the gateway.
gatewaySummary_name :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_name = Lens.lens (\GatewaySummary' {name} -> name) (\s@GatewaySummary' {} a -> s {name = a} :: GatewaySummary)

-- | The ARN of the gateway group that the gateway is associated to.
gatewaySummary_gatewayGroupArn :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_gatewayGroupArn = Lens.lens (\GatewaySummary' {gatewayGroupArn} -> gatewayGroupArn) (\s@GatewaySummary' {} a -> s {gatewayGroupArn = a} :: GatewaySummary)

-- | The ARN of the gateway.
gatewaySummary_arn :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_arn = Lens.lens (\GatewaySummary' {arn} -> arn) (\s@GatewaySummary' {} a -> s {arn = a} :: GatewaySummary)

-- | The description of the gateway.
gatewaySummary_description :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_description = Lens.lens (\GatewaySummary' {description} -> description) (\s@GatewaySummary' {} a -> s {description = a} :: GatewaySummary)

-- | The software version of the gateway. The gateway automatically updates
-- its software version during normal operation.
gatewaySummary_softwareVersion :: Lens.Lens' GatewaySummary (Prelude.Maybe Prelude.Text)
gatewaySummary_softwareVersion = Lens.lens (\GatewaySummary' {softwareVersion} -> softwareVersion) (\s@GatewaySummary' {} a -> s {softwareVersion = a} :: GatewaySummary)

instance Core.FromJSON GatewaySummary where
  parseJSON =
    Core.withObject
      "GatewaySummary"
      ( \x ->
          GatewaySummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "GatewayGroupArn")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SoftwareVersion")
      )

instance Prelude.Hashable GatewaySummary where
  hashWithSalt _salt GatewaySummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` gatewayGroupArn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` softwareVersion

instance Prelude.NFData GatewaySummary where
  rnf GatewaySummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf gatewayGroupArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf softwareVersion
