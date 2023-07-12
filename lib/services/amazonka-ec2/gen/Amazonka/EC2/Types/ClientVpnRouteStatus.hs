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
-- Module      : Amazonka.EC2.Types.ClientVpnRouteStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnRouteStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnRouteStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a Client VPN endpoint route.
--
-- /See:/ 'newClientVpnRouteStatus' smart constructor.
data ClientVpnRouteStatus = ClientVpnRouteStatus'
  { -- | The state of the Client VPN endpoint route.
    code :: Prelude.Maybe ClientVpnRouteStatusCode,
    -- | A message about the status of the Client VPN endpoint route, if
    -- applicable.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnRouteStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'clientVpnRouteStatus_code' - The state of the Client VPN endpoint route.
--
-- 'message', 'clientVpnRouteStatus_message' - A message about the status of the Client VPN endpoint route, if
-- applicable.
newClientVpnRouteStatus ::
  ClientVpnRouteStatus
newClientVpnRouteStatus =
  ClientVpnRouteStatus'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The state of the Client VPN endpoint route.
clientVpnRouteStatus_code :: Lens.Lens' ClientVpnRouteStatus (Prelude.Maybe ClientVpnRouteStatusCode)
clientVpnRouteStatus_code = Lens.lens (\ClientVpnRouteStatus' {code} -> code) (\s@ClientVpnRouteStatus' {} a -> s {code = a} :: ClientVpnRouteStatus)

-- | A message about the status of the Client VPN endpoint route, if
-- applicable.
clientVpnRouteStatus_message :: Lens.Lens' ClientVpnRouteStatus (Prelude.Maybe Prelude.Text)
clientVpnRouteStatus_message = Lens.lens (\ClientVpnRouteStatus' {message} -> message) (\s@ClientVpnRouteStatus' {} a -> s {message = a} :: ClientVpnRouteStatus)

instance Data.FromXML ClientVpnRouteStatus where
  parseXML x =
    ClientVpnRouteStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable ClientVpnRouteStatus where
  hashWithSalt _salt ClientVpnRouteStatus' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ClientVpnRouteStatus where
  rnf ClientVpnRouteStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
