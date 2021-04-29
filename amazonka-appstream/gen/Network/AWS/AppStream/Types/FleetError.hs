{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppStream.Types.FleetError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetError where

import Network.AWS.AppStream.Types.FleetErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a fleet error.
--
-- /See:/ 'newFleetError' smart constructor.
data FleetError = FleetError'
  { -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe FleetErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON FleetError where
  parseJSON =
    Prelude.withObject
      "FleetError"
      ( \x ->
          FleetError'
            Prelude.<$> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable FleetError

instance Prelude.NFData FleetError
