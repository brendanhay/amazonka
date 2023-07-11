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
-- Module      : Amazonka.IoTRoboRunner.Types.PositionCoordinates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.PositionCoordinates where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types.CartesianCoordinates
import qualified Amazonka.Prelude as Prelude

-- | Supported coordinates for worker position.
--
-- /See:/ 'newPositionCoordinates' smart constructor.
data PositionCoordinates = PositionCoordinates'
  { -- | Cartesian coordinates.
    cartesianCoordinates :: Prelude.Maybe CartesianCoordinates
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PositionCoordinates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cartesianCoordinates', 'positionCoordinates_cartesianCoordinates' - Cartesian coordinates.
newPositionCoordinates ::
  PositionCoordinates
newPositionCoordinates =
  PositionCoordinates'
    { cartesianCoordinates =
        Prelude.Nothing
    }

-- | Cartesian coordinates.
positionCoordinates_cartesianCoordinates :: Lens.Lens' PositionCoordinates (Prelude.Maybe CartesianCoordinates)
positionCoordinates_cartesianCoordinates = Lens.lens (\PositionCoordinates' {cartesianCoordinates} -> cartesianCoordinates) (\s@PositionCoordinates' {} a -> s {cartesianCoordinates = a} :: PositionCoordinates)

instance Data.FromJSON PositionCoordinates where
  parseJSON =
    Data.withObject
      "PositionCoordinates"
      ( \x ->
          PositionCoordinates'
            Prelude.<$> (x Data..:? "cartesianCoordinates")
      )

instance Prelude.Hashable PositionCoordinates where
  hashWithSalt _salt PositionCoordinates' {..} =
    _salt `Prelude.hashWithSalt` cartesianCoordinates

instance Prelude.NFData PositionCoordinates where
  rnf PositionCoordinates' {..} =
    Prelude.rnf cartesianCoordinates

instance Data.ToJSON PositionCoordinates where
  toJSON PositionCoordinates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cartesianCoordinates" Data..=)
              Prelude.<$> cartesianCoordinates
          ]
      )
