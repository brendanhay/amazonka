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
-- Module      : Amazonka.EC2.Types.ClientVpnEndpointStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnEndpointStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnEndpointStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a Client VPN endpoint.
--
-- /See:/ 'newClientVpnEndpointStatus' smart constructor.
data ClientVpnEndpointStatus = ClientVpnEndpointStatus'
  { -- | The state of the Client VPN endpoint. Possible states include:
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
    code :: Prelude.Maybe ClientVpnEndpointStatusCode,
    -- | A message about the status of the Client VPN endpoint.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnEndpointStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'message', 'clientVpnEndpointStatus_message' - A message about the status of the Client VPN endpoint.
newClientVpnEndpointStatus ::
  ClientVpnEndpointStatus
newClientVpnEndpointStatus =
  ClientVpnEndpointStatus'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

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
clientVpnEndpointStatus_code :: Lens.Lens' ClientVpnEndpointStatus (Prelude.Maybe ClientVpnEndpointStatusCode)
clientVpnEndpointStatus_code = Lens.lens (\ClientVpnEndpointStatus' {code} -> code) (\s@ClientVpnEndpointStatus' {} a -> s {code = a} :: ClientVpnEndpointStatus)

-- | A message about the status of the Client VPN endpoint.
clientVpnEndpointStatus_message :: Lens.Lens' ClientVpnEndpointStatus (Prelude.Maybe Prelude.Text)
clientVpnEndpointStatus_message = Lens.lens (\ClientVpnEndpointStatus' {message} -> message) (\s@ClientVpnEndpointStatus' {} a -> s {message = a} :: ClientVpnEndpointStatus)

instance Data.FromXML ClientVpnEndpointStatus where
  parseXML x =
    ClientVpnEndpointStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable ClientVpnEndpointStatus where
  hashWithSalt _salt ClientVpnEndpointStatus' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ClientVpnEndpointStatus where
  rnf ClientVpnEndpointStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
