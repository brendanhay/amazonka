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
-- Module      : Amazonka.DirectConnect.Types.VirtualGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.VirtualGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a virtual private gateway for a private virtual
-- interface.
--
-- /See:/ 'newVirtualGateway' smart constructor.
data VirtualGateway = VirtualGateway'
  { -- | The state of the virtual private gateway. The following are the possible
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
    virtualGatewayState :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private gateway.
    virtualGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'virtualGatewayId', 'virtualGateway_virtualGatewayId' - The ID of the virtual private gateway.
newVirtualGateway ::
  VirtualGateway
newVirtualGateway =
  VirtualGateway'
    { virtualGatewayState =
        Prelude.Nothing,
      virtualGatewayId = Prelude.Nothing
    }

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
virtualGateway_virtualGatewayState :: Lens.Lens' VirtualGateway (Prelude.Maybe Prelude.Text)
virtualGateway_virtualGatewayState = Lens.lens (\VirtualGateway' {virtualGatewayState} -> virtualGatewayState) (\s@VirtualGateway' {} a -> s {virtualGatewayState = a} :: VirtualGateway)

-- | The ID of the virtual private gateway.
virtualGateway_virtualGatewayId :: Lens.Lens' VirtualGateway (Prelude.Maybe Prelude.Text)
virtualGateway_virtualGatewayId = Lens.lens (\VirtualGateway' {virtualGatewayId} -> virtualGatewayId) (\s@VirtualGateway' {} a -> s {virtualGatewayId = a} :: VirtualGateway)

instance Core.FromJSON VirtualGateway where
  parseJSON =
    Core.withObject
      "VirtualGateway"
      ( \x ->
          VirtualGateway'
            Prelude.<$> (x Core..:? "virtualGatewayState")
            Prelude.<*> (x Core..:? "virtualGatewayId")
      )

instance Prelude.Hashable VirtualGateway where
  hashWithSalt _salt VirtualGateway' {..} =
    _salt `Prelude.hashWithSalt` virtualGatewayState
      `Prelude.hashWithSalt` virtualGatewayId

instance Prelude.NFData VirtualGateway where
  rnf VirtualGateway' {..} =
    Prelude.rnf virtualGatewayState
      `Prelude.seq` Prelude.rnf virtualGatewayId
