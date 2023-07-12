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
-- Module      : Amazonka.GroundStation.Types.Eirp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.Eirp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.EirpUnits
import qualified Amazonka.Prelude as Prelude

-- | Object that represents EIRP.
--
-- /See:/ 'newEirp' smart constructor.
data Eirp = Eirp'
  { -- | Units of an EIRP.
    units :: EirpUnits,
    -- | Value of an EIRP. Valid values are between 20.0 to 50.0 dBW.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Eirp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'units', 'eirp_units' - Units of an EIRP.
--
-- 'value', 'eirp_value' - Value of an EIRP. Valid values are between 20.0 to 50.0 dBW.
newEirp ::
  -- | 'units'
  EirpUnits ->
  -- | 'value'
  Prelude.Double ->
  Eirp
newEirp pUnits_ pValue_ =
  Eirp' {units = pUnits_, value = pValue_}

-- | Units of an EIRP.
eirp_units :: Lens.Lens' Eirp EirpUnits
eirp_units = Lens.lens (\Eirp' {units} -> units) (\s@Eirp' {} a -> s {units = a} :: Eirp)

-- | Value of an EIRP. Valid values are between 20.0 to 50.0 dBW.
eirp_value :: Lens.Lens' Eirp Prelude.Double
eirp_value = Lens.lens (\Eirp' {value} -> value) (\s@Eirp' {} a -> s {value = a} :: Eirp)

instance Data.FromJSON Eirp where
  parseJSON =
    Data.withObject
      "Eirp"
      ( \x ->
          Eirp'
            Prelude.<$> (x Data..: "units")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable Eirp where
  hashWithSalt _salt Eirp' {..} =
    _salt
      `Prelude.hashWithSalt` units
      `Prelude.hashWithSalt` value

instance Prelude.NFData Eirp where
  rnf Eirp' {..} =
    Prelude.rnf units `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Eirp where
  toJSON Eirp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("units" Data..= units),
            Prelude.Just ("value" Data..= value)
          ]
      )
