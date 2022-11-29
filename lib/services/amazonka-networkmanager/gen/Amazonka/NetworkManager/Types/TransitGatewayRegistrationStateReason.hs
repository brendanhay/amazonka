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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.TransitGatewayRegistrationStateReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.TransitGatewayRegistrationState
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a transit gateway registration.
--
-- /See:/ 'newTransitGatewayRegistrationStateReason' smart constructor.
data TransitGatewayRegistrationStateReason = TransitGatewayRegistrationStateReason'
  { -- | The message for the state reason.
    message :: Prelude.Maybe Prelude.Text,
    -- | The code for the state reason.
    code :: Prelude.Maybe TransitGatewayRegistrationState
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
-- 'message', 'transitGatewayRegistrationStateReason_message' - The message for the state reason.
--
-- 'code', 'transitGatewayRegistrationStateReason_code' - The code for the state reason.
newTransitGatewayRegistrationStateReason ::
  TransitGatewayRegistrationStateReason
newTransitGatewayRegistrationStateReason =
  TransitGatewayRegistrationStateReason'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The message for the state reason.
transitGatewayRegistrationStateReason_message :: Lens.Lens' TransitGatewayRegistrationStateReason (Prelude.Maybe Prelude.Text)
transitGatewayRegistrationStateReason_message = Lens.lens (\TransitGatewayRegistrationStateReason' {message} -> message) (\s@TransitGatewayRegistrationStateReason' {} a -> s {message = a} :: TransitGatewayRegistrationStateReason)

-- | The code for the state reason.
transitGatewayRegistrationStateReason_code :: Lens.Lens' TransitGatewayRegistrationStateReason (Prelude.Maybe TransitGatewayRegistrationState)
transitGatewayRegistrationStateReason_code = Lens.lens (\TransitGatewayRegistrationStateReason' {code} -> code) (\s@TransitGatewayRegistrationStateReason' {} a -> s {code = a} :: TransitGatewayRegistrationStateReason)

instance
  Core.FromJSON
    TransitGatewayRegistrationStateReason
  where
  parseJSON =
    Core.withObject
      "TransitGatewayRegistrationStateReason"
      ( \x ->
          TransitGatewayRegistrationStateReason'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Code")
      )

instance
  Prelude.Hashable
    TransitGatewayRegistrationStateReason
  where
  hashWithSalt
    _salt
    TransitGatewayRegistrationStateReason' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` code

instance
  Prelude.NFData
    TransitGatewayRegistrationStateReason
  where
  rnf TransitGatewayRegistrationStateReason' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
