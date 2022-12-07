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
-- Module      : Amazonka.IoTWireless.Types.SemtechGnssConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SemtechGnssConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.PositionConfigurationFec
import Amazonka.IoTWireless.Types.PositionConfigurationStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the Semtech GNSS solver configuration.
--
-- /See:/ 'newSemtechGnssConfiguration' smart constructor.
data SemtechGnssConfiguration = SemtechGnssConfiguration'
  { -- | The status indicating whether the solver is enabled.
    status :: PositionConfigurationStatus,
    -- | Whether forward error correction is enabled.
    fec :: PositionConfigurationFec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SemtechGnssConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'semtechGnssConfiguration_status' - The status indicating whether the solver is enabled.
--
-- 'fec', 'semtechGnssConfiguration_fec' - Whether forward error correction is enabled.
newSemtechGnssConfiguration ::
  -- | 'status'
  PositionConfigurationStatus ->
  -- | 'fec'
  PositionConfigurationFec ->
  SemtechGnssConfiguration
newSemtechGnssConfiguration pStatus_ pFec_ =
  SemtechGnssConfiguration'
    { status = pStatus_,
      fec = pFec_
    }

-- | The status indicating whether the solver is enabled.
semtechGnssConfiguration_status :: Lens.Lens' SemtechGnssConfiguration PositionConfigurationStatus
semtechGnssConfiguration_status = Lens.lens (\SemtechGnssConfiguration' {status} -> status) (\s@SemtechGnssConfiguration' {} a -> s {status = a} :: SemtechGnssConfiguration)

-- | Whether forward error correction is enabled.
semtechGnssConfiguration_fec :: Lens.Lens' SemtechGnssConfiguration PositionConfigurationFec
semtechGnssConfiguration_fec = Lens.lens (\SemtechGnssConfiguration' {fec} -> fec) (\s@SemtechGnssConfiguration' {} a -> s {fec = a} :: SemtechGnssConfiguration)

instance Prelude.Hashable SemtechGnssConfiguration where
  hashWithSalt _salt SemtechGnssConfiguration' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` fec

instance Prelude.NFData SemtechGnssConfiguration where
  rnf SemtechGnssConfiguration' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf fec

instance Data.ToJSON SemtechGnssConfiguration where
  toJSON SemtechGnssConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Status" Data..= status),
            Prelude.Just ("Fec" Data..= fec)
          ]
      )
