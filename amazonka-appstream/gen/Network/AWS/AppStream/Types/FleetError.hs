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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a fleet error.
--
-- /See:/ 'newFleetError' smart constructor.
data FleetError = FleetError'
  { -- | The error message.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code.
    errorCode :: Core.Maybe FleetErrorCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The error message.
fleetError_errorMessage :: Lens.Lens' FleetError (Core.Maybe Core.Text)
fleetError_errorMessage = Lens.lens (\FleetError' {errorMessage} -> errorMessage) (\s@FleetError' {} a -> s {errorMessage = a} :: FleetError)

-- | The error code.
fleetError_errorCode :: Lens.Lens' FleetError (Core.Maybe FleetErrorCode)
fleetError_errorCode = Lens.lens (\FleetError' {errorCode} -> errorCode) (\s@FleetError' {} a -> s {errorCode = a} :: FleetError)

instance Core.FromJSON FleetError where
  parseJSON =
    Core.withObject
      "FleetError"
      ( \x ->
          FleetError'
            Core.<$> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable FleetError

instance Core.NFData FleetError
