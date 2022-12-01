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
-- Module      : Amazonka.Glue.Types.BooleanColumnStatisticsData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BooleanColumnStatisticsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines column statistics supported for Boolean data columns.
--
-- /See:/ 'newBooleanColumnStatisticsData' smart constructor.
data BooleanColumnStatisticsData = BooleanColumnStatisticsData'
  { -- | The number of true values in the column.
    numberOfTrues :: Prelude.Natural,
    -- | The number of false values in the column.
    numberOfFalses :: Prelude.Natural,
    -- | The number of null values in the column.
    numberOfNulls :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BooleanColumnStatisticsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfTrues', 'booleanColumnStatisticsData_numberOfTrues' - The number of true values in the column.
--
-- 'numberOfFalses', 'booleanColumnStatisticsData_numberOfFalses' - The number of false values in the column.
--
-- 'numberOfNulls', 'booleanColumnStatisticsData_numberOfNulls' - The number of null values in the column.
newBooleanColumnStatisticsData ::
  -- | 'numberOfTrues'
  Prelude.Natural ->
  -- | 'numberOfFalses'
  Prelude.Natural ->
  -- | 'numberOfNulls'
  Prelude.Natural ->
  BooleanColumnStatisticsData
newBooleanColumnStatisticsData
  pNumberOfTrues_
  pNumberOfFalses_
  pNumberOfNulls_ =
    BooleanColumnStatisticsData'
      { numberOfTrues =
          pNumberOfTrues_,
        numberOfFalses = pNumberOfFalses_,
        numberOfNulls = pNumberOfNulls_
      }

-- | The number of true values in the column.
booleanColumnStatisticsData_numberOfTrues :: Lens.Lens' BooleanColumnStatisticsData Prelude.Natural
booleanColumnStatisticsData_numberOfTrues = Lens.lens (\BooleanColumnStatisticsData' {numberOfTrues} -> numberOfTrues) (\s@BooleanColumnStatisticsData' {} a -> s {numberOfTrues = a} :: BooleanColumnStatisticsData)

-- | The number of false values in the column.
booleanColumnStatisticsData_numberOfFalses :: Lens.Lens' BooleanColumnStatisticsData Prelude.Natural
booleanColumnStatisticsData_numberOfFalses = Lens.lens (\BooleanColumnStatisticsData' {numberOfFalses} -> numberOfFalses) (\s@BooleanColumnStatisticsData' {} a -> s {numberOfFalses = a} :: BooleanColumnStatisticsData)

-- | The number of null values in the column.
booleanColumnStatisticsData_numberOfNulls :: Lens.Lens' BooleanColumnStatisticsData Prelude.Natural
booleanColumnStatisticsData_numberOfNulls = Lens.lens (\BooleanColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@BooleanColumnStatisticsData' {} a -> s {numberOfNulls = a} :: BooleanColumnStatisticsData)

instance Core.FromJSON BooleanColumnStatisticsData where
  parseJSON =
    Core.withObject
      "BooleanColumnStatisticsData"
      ( \x ->
          BooleanColumnStatisticsData'
            Prelude.<$> (x Core..: "NumberOfTrues")
            Prelude.<*> (x Core..: "NumberOfFalses")
            Prelude.<*> (x Core..: "NumberOfNulls")
      )

instance Prelude.Hashable BooleanColumnStatisticsData where
  hashWithSalt _salt BooleanColumnStatisticsData' {..} =
    _salt `Prelude.hashWithSalt` numberOfTrues
      `Prelude.hashWithSalt` numberOfFalses
      `Prelude.hashWithSalt` numberOfNulls

instance Prelude.NFData BooleanColumnStatisticsData where
  rnf BooleanColumnStatisticsData' {..} =
    Prelude.rnf numberOfTrues
      `Prelude.seq` Prelude.rnf numberOfFalses
      `Prelude.seq` Prelude.rnf numberOfNulls

instance Core.ToJSON BooleanColumnStatisticsData where
  toJSON BooleanColumnStatisticsData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NumberOfTrues" Core..= numberOfTrues),
            Prelude.Just
              ("NumberOfFalses" Core..= numberOfFalses),
            Prelude.Just
              ("NumberOfNulls" Core..= numberOfNulls)
          ]
      )
