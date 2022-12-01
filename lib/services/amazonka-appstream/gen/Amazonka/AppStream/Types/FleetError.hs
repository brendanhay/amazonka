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
-- Module      : Amazonka.AppStream.Types.FleetError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.FleetError where

import Amazonka.AppStream.Types.FleetErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a fleet error.
--
-- /See:/ 'newFleetError' smart constructor.
data FleetError = FleetError'
  { -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe FleetErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'fleetError_errorMessage' - The error message.
--
-- 'errorCode', 'fleetError_errorCode' - The error code.
newFleetError ::
  FleetError
newFleetError =
  FleetError'
    { errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message.
fleetError_errorMessage :: Lens.Lens' FleetError (Prelude.Maybe Prelude.Text)
fleetError_errorMessage = Lens.lens (\FleetError' {errorMessage} -> errorMessage) (\s@FleetError' {} a -> s {errorMessage = a} :: FleetError)

-- | The error code.
fleetError_errorCode :: Lens.Lens' FleetError (Prelude.Maybe FleetErrorCode)
fleetError_errorCode = Lens.lens (\FleetError' {errorCode} -> errorCode) (\s@FleetError' {} a -> s {errorCode = a} :: FleetError)

instance Core.FromJSON FleetError where
  parseJSON =
    Core.withObject
      "FleetError"
      ( \x ->
          FleetError'
            Prelude.<$> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "ErrorCode")
      )

instance Prelude.Hashable FleetError where
  hashWithSalt _salt FleetError' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData FleetError where
  rnf FleetError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
