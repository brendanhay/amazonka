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
-- Module      : Amazonka.NetworkManager.Types.TransitGatewayRegistrationStateReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.TransitGatewayRegistrationStateReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.TransitGatewayRegistrationState
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a transit gateway registration.
--
-- /See:/ 'newTransitGatewayRegistrationStateReason' smart constructor.
data TransitGatewayRegistrationStateReason = TransitGatewayRegistrationStateReason'
  { -- | The code for the state reason.
    code :: Prelude.Maybe TransitGatewayRegistrationState,
    -- | The message for the state reason.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRegistrationStateReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'transitGatewayRegistrationStateReason_code' - The code for the state reason.
--
-- 'message', 'transitGatewayRegistrationStateReason_message' - The message for the state reason.
newTransitGatewayRegistrationStateReason ::
  TransitGatewayRegistrationStateReason
newTransitGatewayRegistrationStateReason =
  TransitGatewayRegistrationStateReason'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The code for the state reason.
transitGatewayRegistrationStateReason_code :: Lens.Lens' TransitGatewayRegistrationStateReason (Prelude.Maybe TransitGatewayRegistrationState)
transitGatewayRegistrationStateReason_code = Lens.lens (\TransitGatewayRegistrationStateReason' {code} -> code) (\s@TransitGatewayRegistrationStateReason' {} a -> s {code = a} :: TransitGatewayRegistrationStateReason)

-- | The message for the state reason.
transitGatewayRegistrationStateReason_message :: Lens.Lens' TransitGatewayRegistrationStateReason (Prelude.Maybe Prelude.Text)
transitGatewayRegistrationStateReason_message = Lens.lens (\TransitGatewayRegistrationStateReason' {message} -> message) (\s@TransitGatewayRegistrationStateReason' {} a -> s {message = a} :: TransitGatewayRegistrationStateReason)

instance
  Data.FromJSON
    TransitGatewayRegistrationStateReason
  where
  parseJSON =
    Data.withObject
      "TransitGatewayRegistrationStateReason"
      ( \x ->
          TransitGatewayRegistrationStateReason'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance
  Prelude.Hashable
    TransitGatewayRegistrationStateReason
  where
  hashWithSalt
    _salt
    TransitGatewayRegistrationStateReason' {..} =
      _salt
        `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    TransitGatewayRegistrationStateReason
  where
  rnf TransitGatewayRegistrationStateReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
