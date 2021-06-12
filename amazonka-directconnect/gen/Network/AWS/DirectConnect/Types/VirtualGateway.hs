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
-- Module      : Network.AWS.DirectConnect.Types.VirtualGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualGateway where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a virtual private gateway for a private virtual
-- interface.
--
-- /See:/ 'newVirtualGateway' smart constructor.
data VirtualGateway = VirtualGateway'
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Core.Maybe Core.Text,
    -- | The state of the virtual private gateway. The following are the possible
    -- values:
    --
    -- -   @pending@: Initial state after creating the virtual private gateway.
    --
    -- -   @available@: Ready for use by a private virtual interface.
    --
    -- -   @deleting@: Initial state after deleting the virtual private
    --     gateway.
    --
    -- -   @deleted@: The virtual private gateway is deleted. The private
    --     virtual interface is unable to send traffic over this gateway.
    virtualGatewayState :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VirtualGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualGatewayId', 'virtualGateway_virtualGatewayId' - The ID of the virtual private gateway.
--
-- 'virtualGatewayState', 'virtualGateway_virtualGatewayState' - The state of the virtual private gateway. The following are the possible
-- values:
--
-- -   @pending@: Initial state after creating the virtual private gateway.
--
-- -   @available@: Ready for use by a private virtual interface.
--
-- -   @deleting@: Initial state after deleting the virtual private
--     gateway.
--
-- -   @deleted@: The virtual private gateway is deleted. The private
--     virtual interface is unable to send traffic over this gateway.
newVirtualGateway ::
  VirtualGateway
newVirtualGateway =
  VirtualGateway'
    { virtualGatewayId = Core.Nothing,
      virtualGatewayState = Core.Nothing
    }

-- | The ID of the virtual private gateway.
virtualGateway_virtualGatewayId :: Lens.Lens' VirtualGateway (Core.Maybe Core.Text)
virtualGateway_virtualGatewayId = Lens.lens (\VirtualGateway' {virtualGatewayId} -> virtualGatewayId) (\s@VirtualGateway' {} a -> s {virtualGatewayId = a} :: VirtualGateway)

-- | The state of the virtual private gateway. The following are the possible
-- values:
--
-- -   @pending@: Initial state after creating the virtual private gateway.
--
-- -   @available@: Ready for use by a private virtual interface.
--
-- -   @deleting@: Initial state after deleting the virtual private
--     gateway.
--
-- -   @deleted@: The virtual private gateway is deleted. The private
--     virtual interface is unable to send traffic over this gateway.
virtualGateway_virtualGatewayState :: Lens.Lens' VirtualGateway (Core.Maybe Core.Text)
virtualGateway_virtualGatewayState = Lens.lens (\VirtualGateway' {virtualGatewayState} -> virtualGatewayState) (\s@VirtualGateway' {} a -> s {virtualGatewayState = a} :: VirtualGateway)

instance Core.FromJSON VirtualGateway where
  parseJSON =
    Core.withObject
      "VirtualGateway"
      ( \x ->
          VirtualGateway'
            Core.<$> (x Core..:? "virtualGatewayId")
            Core.<*> (x Core..:? "virtualGatewayState")
      )

instance Core.Hashable VirtualGateway

instance Core.NFData VirtualGateway
