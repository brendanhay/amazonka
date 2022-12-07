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
-- Module      : Amazonka.IoTWireless.Types.PositionSolverConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.PositionSolverConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.SemtechGnssConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The wrapper for position solver configurations.
--
-- /See:/ 'newPositionSolverConfigurations' smart constructor.
data PositionSolverConfigurations = PositionSolverConfigurations'
  { -- | The Semtech GNSS solver configuration object.
    semtechGnss :: Prelude.Maybe SemtechGnssConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PositionSolverConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'semtechGnss', 'positionSolverConfigurations_semtechGnss' - The Semtech GNSS solver configuration object.
newPositionSolverConfigurations ::
  PositionSolverConfigurations
newPositionSolverConfigurations =
  PositionSolverConfigurations'
    { semtechGnss =
        Prelude.Nothing
    }

-- | The Semtech GNSS solver configuration object.
positionSolverConfigurations_semtechGnss :: Lens.Lens' PositionSolverConfigurations (Prelude.Maybe SemtechGnssConfiguration)
positionSolverConfigurations_semtechGnss = Lens.lens (\PositionSolverConfigurations' {semtechGnss} -> semtechGnss) (\s@PositionSolverConfigurations' {} a -> s {semtechGnss = a} :: PositionSolverConfigurations)

instance
  Prelude.Hashable
    PositionSolverConfigurations
  where
  hashWithSalt _salt PositionSolverConfigurations' {..} =
    _salt `Prelude.hashWithSalt` semtechGnss

instance Prelude.NFData PositionSolverConfigurations where
  rnf PositionSolverConfigurations' {..} =
    Prelude.rnf semtechGnss

instance Data.ToJSON PositionSolverConfigurations where
  toJSON PositionSolverConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [("SemtechGnss" Data..=) Prelude.<$> semtechGnss]
      )
