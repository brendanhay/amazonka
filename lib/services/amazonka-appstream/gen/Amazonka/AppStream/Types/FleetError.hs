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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.FleetError where

import Amazonka.AppStream.Types.FleetErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a fleet error.
--
-- /See:/ 'newFleetError' smart constructor.
data FleetError = FleetError'
  { -- | The error code.
    errorCode :: Prelude.Maybe FleetErrorCode,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'errorCode', 'fleetError_errorCode' - The error code.
--
-- 'errorMessage', 'fleetError_errorMessage' - The error message.
newFleetError ::
  FleetError
newFleetError =
  FleetError'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The error code.
fleetError_errorCode :: Lens.Lens' FleetError (Prelude.Maybe FleetErrorCode)
fleetError_errorCode = Lens.lens (\FleetError' {errorCode} -> errorCode) (\s@FleetError' {} a -> s {errorCode = a} :: FleetError)

-- | The error message.
fleetError_errorMessage :: Lens.Lens' FleetError (Prelude.Maybe Prelude.Text)
fleetError_errorMessage = Lens.lens (\FleetError' {errorMessage} -> errorMessage) (\s@FleetError' {} a -> s {errorMessage = a} :: FleetError)

instance Data.FromJSON FleetError where
  parseJSON =
    Data.withObject
      "FleetError"
      ( \x ->
          FleetError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
      )

instance Prelude.Hashable FleetError where
  hashWithSalt _salt FleetError' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData FleetError where
  rnf FleetError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
