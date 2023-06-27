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
-- Module      : Amazonka.CustomerProfiles.Types.Conditions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Conditions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.Range
import Amazonka.CustomerProfiles.Types.Threshold
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The conditions including range, object count, and threshold for the
-- calculated attribute.
--
-- /See:/ 'newConditions' smart constructor.
data Conditions = Conditions'
  { -- | The number of profile objects used for the calculated attribute.
    objectCount :: Prelude.Maybe Prelude.Natural,
    -- | The relative time period over which data is included in the aggregation.
    range :: Prelude.Maybe Range,
    -- | The threshold for the calculated attribute.
    threshold :: Prelude.Maybe Threshold
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Conditions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectCount', 'conditions_objectCount' - The number of profile objects used for the calculated attribute.
--
-- 'range', 'conditions_range' - The relative time period over which data is included in the aggregation.
--
-- 'threshold', 'conditions_threshold' - The threshold for the calculated attribute.
newConditions ::
  Conditions
newConditions =
  Conditions'
    { objectCount = Prelude.Nothing,
      range = Prelude.Nothing,
      threshold = Prelude.Nothing
    }

-- | The number of profile objects used for the calculated attribute.
conditions_objectCount :: Lens.Lens' Conditions (Prelude.Maybe Prelude.Natural)
conditions_objectCount = Lens.lens (\Conditions' {objectCount} -> objectCount) (\s@Conditions' {} a -> s {objectCount = a} :: Conditions)

-- | The relative time period over which data is included in the aggregation.
conditions_range :: Lens.Lens' Conditions (Prelude.Maybe Range)
conditions_range = Lens.lens (\Conditions' {range} -> range) (\s@Conditions' {} a -> s {range = a} :: Conditions)

-- | The threshold for the calculated attribute.
conditions_threshold :: Lens.Lens' Conditions (Prelude.Maybe Threshold)
conditions_threshold = Lens.lens (\Conditions' {threshold} -> threshold) (\s@Conditions' {} a -> s {threshold = a} :: Conditions)

instance Data.FromJSON Conditions where
  parseJSON =
    Data.withObject
      "Conditions"
      ( \x ->
          Conditions'
            Prelude.<$> (x Data..:? "ObjectCount")
            Prelude.<*> (x Data..:? "Range")
            Prelude.<*> (x Data..:? "Threshold")
      )

instance Prelude.Hashable Conditions where
  hashWithSalt _salt Conditions' {..} =
    _salt
      `Prelude.hashWithSalt` objectCount
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData Conditions where
  rnf Conditions' {..} =
    Prelude.rnf objectCount
      `Prelude.seq` Prelude.rnf range
      `Prelude.seq` Prelude.rnf threshold

instance Data.ToJSON Conditions where
  toJSON Conditions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObjectCount" Data..=) Prelude.<$> objectCount,
            ("Range" Data..=) Prelude.<$> range,
            ("Threshold" Data..=) Prelude.<$> threshold
          ]
      )
