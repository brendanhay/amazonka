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
-- Module      : Amazonka.Location.Types.RouteMatrixEntryError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.RouteMatrixEntryError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.RouteMatrixErrorCode
import qualified Amazonka.Prelude as Prelude

-- | An error corresponding to the calculation of a route between the
-- @DeparturePosition@ and @DestinationPosition@.
--
-- The error code can be one of the following:
--
-- -   @RouteNotFound@ - Unable to find a valid route with the given
--     parameters.
--
-- -   @RouteTooLong@ - Route calculation went beyond the maximum size of a
--     route and was terminated before completion.
--
-- -   @PositionsNotFound@ - One or more of the input positions were not
--     found on the route network.
--
-- -   @DestinationPositionNotFound@ - The destination position was not
--     found on the route network.
--
-- -   @DeparturePositionNotFound@ - The departure position was not found
--     on the route network.
--
-- -   @OtherValidationError@ - The given inputs were not valid or a route
--     was not found. More information is given in the error @Message@
--
-- /See:/ 'newRouteMatrixEntryError' smart constructor.
data RouteMatrixEntryError = RouteMatrixEntryError'
  { -- | A message about the error that occurred for the route calculation.
    message :: Prelude.Maybe Prelude.Text,
    -- | The type of error which occurred for the route calculation.
    code :: RouteMatrixErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteMatrixEntryError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'routeMatrixEntryError_message' - A message about the error that occurred for the route calculation.
--
-- 'code', 'routeMatrixEntryError_code' - The type of error which occurred for the route calculation.
newRouteMatrixEntryError ::
  -- | 'code'
  RouteMatrixErrorCode ->
  RouteMatrixEntryError
newRouteMatrixEntryError pCode_ =
  RouteMatrixEntryError'
    { message = Prelude.Nothing,
      code = pCode_
    }

-- | A message about the error that occurred for the route calculation.
routeMatrixEntryError_message :: Lens.Lens' RouteMatrixEntryError (Prelude.Maybe Prelude.Text)
routeMatrixEntryError_message = Lens.lens (\RouteMatrixEntryError' {message} -> message) (\s@RouteMatrixEntryError' {} a -> s {message = a} :: RouteMatrixEntryError)

-- | The type of error which occurred for the route calculation.
routeMatrixEntryError_code :: Lens.Lens' RouteMatrixEntryError RouteMatrixErrorCode
routeMatrixEntryError_code = Lens.lens (\RouteMatrixEntryError' {code} -> code) (\s@RouteMatrixEntryError' {} a -> s {code = a} :: RouteMatrixEntryError)

instance Data.FromJSON RouteMatrixEntryError where
  parseJSON =
    Data.withObject
      "RouteMatrixEntryError"
      ( \x ->
          RouteMatrixEntryError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..: "Code")
      )

instance Prelude.Hashable RouteMatrixEntryError where
  hashWithSalt _salt RouteMatrixEntryError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData RouteMatrixEntryError where
  rnf RouteMatrixEntryError' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
