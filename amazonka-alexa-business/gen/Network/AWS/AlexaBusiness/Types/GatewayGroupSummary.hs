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
-- Module      : Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewayGroupSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The summary of a gateway group.
--
-- /See:/ 'newGatewayGroupSummary' smart constructor.
data GatewayGroupSummary = GatewayGroupSummary'
  { -- | The ARN of the gateway group.
    arn :: Core.Maybe Core.Text,
    -- | The name of the gateway group.
    name :: Core.Maybe Core.Text,
    -- | The description of the gateway group.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GatewayGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'gatewayGroupSummary_arn' - The ARN of the gateway group.
--
-- 'name', 'gatewayGroupSummary_name' - The name of the gateway group.
--
-- 'description', 'gatewayGroupSummary_description' - The description of the gateway group.
newGatewayGroupSummary ::
  GatewayGroupSummary
newGatewayGroupSummary =
  GatewayGroupSummary'
    { arn = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing
    }

-- | The ARN of the gateway group.
gatewayGroupSummary_arn :: Lens.Lens' GatewayGroupSummary (Core.Maybe Core.Text)
gatewayGroupSummary_arn = Lens.lens (\GatewayGroupSummary' {arn} -> arn) (\s@GatewayGroupSummary' {} a -> s {arn = a} :: GatewayGroupSummary)

-- | The name of the gateway group.
gatewayGroupSummary_name :: Lens.Lens' GatewayGroupSummary (Core.Maybe Core.Text)
gatewayGroupSummary_name = Lens.lens (\GatewayGroupSummary' {name} -> name) (\s@GatewayGroupSummary' {} a -> s {name = a} :: GatewayGroupSummary)

-- | The description of the gateway group.
gatewayGroupSummary_description :: Lens.Lens' GatewayGroupSummary (Core.Maybe Core.Text)
gatewayGroupSummary_description = Lens.lens (\GatewayGroupSummary' {description} -> description) (\s@GatewayGroupSummary' {} a -> s {description = a} :: GatewayGroupSummary)

instance Core.FromJSON GatewayGroupSummary where
  parseJSON =
    Core.withObject
      "GatewayGroupSummary"
      ( \x ->
          GatewayGroupSummary'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable GatewayGroupSummary

instance Core.NFData GatewayGroupSummary
