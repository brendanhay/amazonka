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
-- Module      : Amazonka.IoTWireless.Types.SemtechGnssDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SemtechGnssDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.PositionConfigurationFec
import Amazonka.IoTWireless.Types.PositionConfigurationStatus
import Amazonka.IoTWireless.Types.PositionSolverProvider
import Amazonka.IoTWireless.Types.PositionSolverType
import qualified Amazonka.Prelude as Prelude

-- | Details of the Semtech GNSS solver object.
--
-- /See:/ 'newSemtechGnssDetail' smart constructor.
data SemtechGnssDetail = SemtechGnssDetail'
  { -- | The type of positioning solver used.
    type' :: Prelude.Maybe PositionSolverType,
    -- | The vendor of the solver object.
    provider :: Prelude.Maybe PositionSolverProvider,
    -- | The status indicating whether the solver is enabled.
    status :: Prelude.Maybe PositionConfigurationStatus,
    -- | Whether forward error correction is enabled.
    fec :: Prelude.Maybe PositionConfigurationFec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SemtechGnssDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'semtechGnssDetail_type' - The type of positioning solver used.
--
-- 'provider', 'semtechGnssDetail_provider' - The vendor of the solver object.
--
-- 'status', 'semtechGnssDetail_status' - The status indicating whether the solver is enabled.
--
-- 'fec', 'semtechGnssDetail_fec' - Whether forward error correction is enabled.
newSemtechGnssDetail ::
  SemtechGnssDetail
newSemtechGnssDetail =
  SemtechGnssDetail'
    { type' = Prelude.Nothing,
      provider = Prelude.Nothing,
      status = Prelude.Nothing,
      fec = Prelude.Nothing
    }

-- | The type of positioning solver used.
semtechGnssDetail_type :: Lens.Lens' SemtechGnssDetail (Prelude.Maybe PositionSolverType)
semtechGnssDetail_type = Lens.lens (\SemtechGnssDetail' {type'} -> type') (\s@SemtechGnssDetail' {} a -> s {type' = a} :: SemtechGnssDetail)

-- | The vendor of the solver object.
semtechGnssDetail_provider :: Lens.Lens' SemtechGnssDetail (Prelude.Maybe PositionSolverProvider)
semtechGnssDetail_provider = Lens.lens (\SemtechGnssDetail' {provider} -> provider) (\s@SemtechGnssDetail' {} a -> s {provider = a} :: SemtechGnssDetail)

-- | The status indicating whether the solver is enabled.
semtechGnssDetail_status :: Lens.Lens' SemtechGnssDetail (Prelude.Maybe PositionConfigurationStatus)
semtechGnssDetail_status = Lens.lens (\SemtechGnssDetail' {status} -> status) (\s@SemtechGnssDetail' {} a -> s {status = a} :: SemtechGnssDetail)

-- | Whether forward error correction is enabled.
semtechGnssDetail_fec :: Lens.Lens' SemtechGnssDetail (Prelude.Maybe PositionConfigurationFec)
semtechGnssDetail_fec = Lens.lens (\SemtechGnssDetail' {fec} -> fec) (\s@SemtechGnssDetail' {} a -> s {fec = a} :: SemtechGnssDetail)

instance Data.FromJSON SemtechGnssDetail where
  parseJSON =
    Data.withObject
      "SemtechGnssDetail"
      ( \x ->
          SemtechGnssDetail'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Provider")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Fec")
      )

instance Prelude.Hashable SemtechGnssDetail where
  hashWithSalt _salt SemtechGnssDetail' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` fec

instance Prelude.NFData SemtechGnssDetail where
  rnf SemtechGnssDetail' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf fec
