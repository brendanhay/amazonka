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
-- Module      : Amazonka.IoTWireless.Types.PositionSolverDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.PositionSolverDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.SemtechGnssDetail
import qualified Amazonka.Prelude as Prelude

-- | The wrapper for position solver details.
--
-- /See:/ 'newPositionSolverDetails' smart constructor.
data PositionSolverDetails = PositionSolverDetails'
  { -- | The Semtech GNSS solver object details.
    semtechGnss :: Prelude.Maybe SemtechGnssDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PositionSolverDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'semtechGnss', 'positionSolverDetails_semtechGnss' - The Semtech GNSS solver object details.
newPositionSolverDetails ::
  PositionSolverDetails
newPositionSolverDetails =
  PositionSolverDetails'
    { semtechGnss =
        Prelude.Nothing
    }

-- | The Semtech GNSS solver object details.
positionSolverDetails_semtechGnss :: Lens.Lens' PositionSolverDetails (Prelude.Maybe SemtechGnssDetail)
positionSolverDetails_semtechGnss = Lens.lens (\PositionSolverDetails' {semtechGnss} -> semtechGnss) (\s@PositionSolverDetails' {} a -> s {semtechGnss = a} :: PositionSolverDetails)

instance Data.FromJSON PositionSolverDetails where
  parseJSON =
    Data.withObject
      "PositionSolverDetails"
      ( \x ->
          PositionSolverDetails'
            Prelude.<$> (x Data..:? "SemtechGnss")
      )

instance Prelude.Hashable PositionSolverDetails where
  hashWithSalt _salt PositionSolverDetails' {..} =
    _salt `Prelude.hashWithSalt` semtechGnss

instance Prelude.NFData PositionSolverDetails where
  rnf PositionSolverDetails' {..} =
    Prelude.rnf semtechGnss
