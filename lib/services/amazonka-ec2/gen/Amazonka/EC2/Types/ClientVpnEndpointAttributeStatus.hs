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
-- Module      : Amazonka.EC2.Types.ClientVpnEndpointAttributeStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnEndpointAttributeStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnEndpointAttributeStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of the Client VPN endpoint attribute.
--
-- /See:/ 'newClientVpnEndpointAttributeStatus' smart constructor.
data ClientVpnEndpointAttributeStatus = ClientVpnEndpointAttributeStatus'
  { -- | The status code.
    code :: Prelude.Maybe ClientVpnEndpointAttributeStatusCode,
    -- | The status message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnEndpointAttributeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'clientVpnEndpointAttributeStatus_code' - The status code.
--
-- 'message', 'clientVpnEndpointAttributeStatus_message' - The status message.
newClientVpnEndpointAttributeStatus ::
  ClientVpnEndpointAttributeStatus
newClientVpnEndpointAttributeStatus =
  ClientVpnEndpointAttributeStatus'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status code.
clientVpnEndpointAttributeStatus_code :: Lens.Lens' ClientVpnEndpointAttributeStatus (Prelude.Maybe ClientVpnEndpointAttributeStatusCode)
clientVpnEndpointAttributeStatus_code = Lens.lens (\ClientVpnEndpointAttributeStatus' {code} -> code) (\s@ClientVpnEndpointAttributeStatus' {} a -> s {code = a} :: ClientVpnEndpointAttributeStatus)

-- | The status message.
clientVpnEndpointAttributeStatus_message :: Lens.Lens' ClientVpnEndpointAttributeStatus (Prelude.Maybe Prelude.Text)
clientVpnEndpointAttributeStatus_message = Lens.lens (\ClientVpnEndpointAttributeStatus' {message} -> message) (\s@ClientVpnEndpointAttributeStatus' {} a -> s {message = a} :: ClientVpnEndpointAttributeStatus)

instance
  Data.FromXML
    ClientVpnEndpointAttributeStatus
  where
  parseXML x =
    ClientVpnEndpointAttributeStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance
  Prelude.Hashable
    ClientVpnEndpointAttributeStatus
  where
  hashWithSalt
    _salt
    ClientVpnEndpointAttributeStatus' {..} =
      _salt
        `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    ClientVpnEndpointAttributeStatus
  where
  rnf ClientVpnEndpointAttributeStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
