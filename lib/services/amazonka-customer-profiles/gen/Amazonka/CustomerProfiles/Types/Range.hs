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
-- Module      : Amazonka.CustomerProfiles.Types.Range
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Range where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.Unit
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The relative time period over which data is included in the aggregation.
--
-- /See:/ 'newRange' smart constructor.
data Range = Range'
  { -- | The amount of time of the specified unit.
    value :: Prelude.Natural,
    -- | The unit of time.
    unit :: Unit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Range' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'range_value' - The amount of time of the specified unit.
--
-- 'unit', 'range_unit' - The unit of time.
newRange ::
  -- | 'value'
  Prelude.Natural ->
  -- | 'unit'
  Unit ->
  Range
newRange pValue_ pUnit_ =
  Range' {value = pValue_, unit = pUnit_}

-- | The amount of time of the specified unit.
range_value :: Lens.Lens' Range Prelude.Natural
range_value = Lens.lens (\Range' {value} -> value) (\s@Range' {} a -> s {value = a} :: Range)

-- | The unit of time.
range_unit :: Lens.Lens' Range Unit
range_unit = Lens.lens (\Range' {unit} -> unit) (\s@Range' {} a -> s {unit = a} :: Range)

instance Data.FromJSON Range where
  parseJSON =
    Data.withObject
      "Range"
      ( \x ->
          Range'
            Prelude.<$> (x Data..: "Value")
            Prelude.<*> (x Data..: "Unit")
      )

instance Prelude.Hashable Range where
  hashWithSalt _salt Range' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` unit

instance Prelude.NFData Range where
  rnf Range' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf unit

instance Data.ToJSON Range where
  toJSON Range' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Value" Data..= value),
            Prelude.Just ("Unit" Data..= unit)
          ]
      )
