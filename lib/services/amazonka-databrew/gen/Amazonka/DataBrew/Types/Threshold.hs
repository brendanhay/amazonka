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
-- Module      : Amazonka.DataBrew.Types.Threshold
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Threshold where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.ThresholdType
import Amazonka.DataBrew.Types.ThresholdUnit
import qualified Amazonka.Prelude as Prelude

-- | The threshold used with a non-aggregate check expression. The
-- non-aggregate check expression will be applied to each row in a specific
-- column. Then the threshold will be used to determine whether the
-- validation succeeds.
--
-- /See:/ 'newThreshold' smart constructor.
data Threshold = Threshold'
  { -- | The type of a threshold. Used for comparison of an actual count of rows
    -- that satisfy the rule to the threshold value.
    type' :: Prelude.Maybe ThresholdType,
    -- | Unit of threshold value. Can be either a COUNT or PERCENTAGE of the full
    -- sample size used for validation.
    unit :: Prelude.Maybe ThresholdUnit,
    -- | The value of a threshold.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Threshold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'threshold_type' - The type of a threshold. Used for comparison of an actual count of rows
-- that satisfy the rule to the threshold value.
--
-- 'unit', 'threshold_unit' - Unit of threshold value. Can be either a COUNT or PERCENTAGE of the full
-- sample size used for validation.
--
-- 'value', 'threshold_value' - The value of a threshold.
newThreshold ::
  -- | 'value'
  Prelude.Double ->
  Threshold
newThreshold pValue_ =
  Threshold'
    { type' = Prelude.Nothing,
      unit = Prelude.Nothing,
      value = pValue_
    }

-- | The type of a threshold. Used for comparison of an actual count of rows
-- that satisfy the rule to the threshold value.
threshold_type :: Lens.Lens' Threshold (Prelude.Maybe ThresholdType)
threshold_type = Lens.lens (\Threshold' {type'} -> type') (\s@Threshold' {} a -> s {type' = a} :: Threshold)

-- | Unit of threshold value. Can be either a COUNT or PERCENTAGE of the full
-- sample size used for validation.
threshold_unit :: Lens.Lens' Threshold (Prelude.Maybe ThresholdUnit)
threshold_unit = Lens.lens (\Threshold' {unit} -> unit) (\s@Threshold' {} a -> s {unit = a} :: Threshold)

-- | The value of a threshold.
threshold_value :: Lens.Lens' Threshold Prelude.Double
threshold_value = Lens.lens (\Threshold' {value} -> value) (\s@Threshold' {} a -> s {value = a} :: Threshold)

instance Data.FromJSON Threshold where
  parseJSON =
    Data.withObject
      "Threshold"
      ( \x ->
          Threshold'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Unit")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable Threshold where
  hashWithSalt _salt Threshold' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData Threshold where
  rnf Threshold' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Threshold where
  toJSON Threshold' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            ("Unit" Data..=) Prelude.<$> unit,
            Prelude.Just ("Value" Data..= value)
          ]
      )
