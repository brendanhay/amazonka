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
-- Module      : Network.AWS.EC2.Types.ClientVpnEndpointStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnEndpointStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVpnEndpointStatusCode
import qualified Network.AWS.Lens as Lens

-- | Describes the state of a Client VPN endpoint.
--
-- /See:/ 'newClientVpnEndpointStatus' smart constructor.
data ClientVpnEndpointStatus = ClientVpnEndpointStatus'
  { -- | A message about the status of the Client VPN endpoint.
    message :: Core.Maybe Core.Text,
    -- | The state of the Client VPN endpoint. Possible states include:
    --
    -- -   @pending-associate@ - The Client VPN endpoint has been created but
    --     no target networks have been associated. The Client VPN endpoint
    --     cannot accept connections.
    --
    -- -   @available@ - The Client VPN endpoint has been created and a target
    --     network has been associated. The Client VPN endpoint can accept
    --     connections.
    --
    -- -   @deleting@ - The Client VPN endpoint is being deleted. The Client
    --     VPN endpoint cannot accept connections.
    --
    -- -   @deleted@ - The Client VPN endpoint has been deleted. The Client VPN
    --     endpoint cannot accept connections.
    code :: Core.Maybe ClientVpnEndpointStatusCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientVpnEndpointStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'clientVpnEndpointStatus_message' - A message about the status of the Client VPN endpoint.
--
-- 'code', 'clientVpnEndpointStatus_code' - The state of the Client VPN endpoint. Possible states include:
--
-- -   @pending-associate@ - The Client VPN endpoint has been created but
--     no target networks have been associated. The Client VPN endpoint
--     cannot accept connections.
--
-- -   @available@ - The Client VPN endpoint has been created and a target
--     network has been associated. The Client VPN endpoint can accept
--     connections.
--
-- -   @deleting@ - The Client VPN endpoint is being deleted. The Client
--     VPN endpoint cannot accept connections.
--
-- -   @deleted@ - The Client VPN endpoint has been deleted. The Client VPN
--     endpoint cannot accept connections.
newClientVpnEndpointStatus ::
  ClientVpnEndpointStatus
newClientVpnEndpointStatus =
  ClientVpnEndpointStatus'
    { message = Core.Nothing,
      code = Core.Nothing
    }

-- | A message about the status of the Client VPN endpoint.
clientVpnEndpointStatus_message :: Lens.Lens' ClientVpnEndpointStatus (Core.Maybe Core.Text)
clientVpnEndpointStatus_message = Lens.lens (\ClientVpnEndpointStatus' {message} -> message) (\s@ClientVpnEndpointStatus' {} a -> s {message = a} :: ClientVpnEndpointStatus)

-- | The state of the Client VPN endpoint. Possible states include:
--
-- -   @pending-associate@ - The Client VPN endpoint has been created but
--     no target networks have been associated. The Client VPN endpoint
--     cannot accept connections.
--
-- -   @available@ - The Client VPN endpoint has been created and a target
--     network has been associated. The Client VPN endpoint can accept
--     connections.
--
-- -   @deleting@ - The Client VPN endpoint is being deleted. The Client
--     VPN endpoint cannot accept connections.
--
-- -   @deleted@ - The Client VPN endpoint has been deleted. The Client VPN
--     endpoint cannot accept connections.
clientVpnEndpointStatus_code :: Lens.Lens' ClientVpnEndpointStatus (Core.Maybe ClientVpnEndpointStatusCode)
clientVpnEndpointStatus_code = Lens.lens (\ClientVpnEndpointStatus' {code} -> code) (\s@ClientVpnEndpointStatus' {} a -> s {code = a} :: ClientVpnEndpointStatus)

instance Core.FromXML ClientVpnEndpointStatus where
  parseXML x =
    ClientVpnEndpointStatus'
      Core.<$> (x Core..@? "message") Core.<*> (x Core..@? "code")

instance Core.Hashable ClientVpnEndpointStatus

instance Core.NFData ClientVpnEndpointStatus
