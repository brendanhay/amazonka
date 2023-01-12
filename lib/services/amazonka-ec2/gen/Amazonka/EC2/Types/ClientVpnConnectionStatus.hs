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
-- Module      : Amazonka.EC2.Types.ClientVpnConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnConnectionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnConnectionStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a client connection.
--
-- /See:/ 'newClientVpnConnectionStatus' smart constructor.
data ClientVpnConnectionStatus = ClientVpnConnectionStatus'
  { -- | The state of the client connection.
    code :: Prelude.Maybe ClientVpnConnectionStatusCode,
    -- | A message about the status of the client connection, if applicable.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'clientVpnConnectionStatus_code' - The state of the client connection.
--
-- 'message', 'clientVpnConnectionStatus_message' - A message about the status of the client connection, if applicable.
newClientVpnConnectionStatus ::
  ClientVpnConnectionStatus
newClientVpnConnectionStatus =
  ClientVpnConnectionStatus'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The state of the client connection.
clientVpnConnectionStatus_code :: Lens.Lens' ClientVpnConnectionStatus (Prelude.Maybe ClientVpnConnectionStatusCode)
clientVpnConnectionStatus_code = Lens.lens (\ClientVpnConnectionStatus' {code} -> code) (\s@ClientVpnConnectionStatus' {} a -> s {code = a} :: ClientVpnConnectionStatus)

-- | A message about the status of the client connection, if applicable.
clientVpnConnectionStatus_message :: Lens.Lens' ClientVpnConnectionStatus (Prelude.Maybe Prelude.Text)
clientVpnConnectionStatus_message = Lens.lens (\ClientVpnConnectionStatus' {message} -> message) (\s@ClientVpnConnectionStatus' {} a -> s {message = a} :: ClientVpnConnectionStatus)

instance Data.FromXML ClientVpnConnectionStatus where
  parseXML x =
    ClientVpnConnectionStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable ClientVpnConnectionStatus where
  hashWithSalt _salt ClientVpnConnectionStatus' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ClientVpnConnectionStatus where
  rnf ClientVpnConnectionStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
