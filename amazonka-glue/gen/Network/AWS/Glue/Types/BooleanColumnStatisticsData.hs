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
-- Module      : Network.AWS.Glue.Types.BooleanColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BooleanColumnStatisticsData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines column statistics supported for Boolean data columns.
--
-- /See:/ 'newBooleanColumnStatisticsData' smart constructor.
data BooleanColumnStatisticsData = BooleanColumnStatisticsData'
  { -- | The number of true values in the column.
    numberOfTrues :: Core.Natural,
    -- | The number of false values in the column.
    numberOfFalses :: Core.Natural,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Natural ->
  -- | 'numberOfFalses'
  Core.Natural ->
  -- | 'numberOfNulls'
  Core.Natural ->
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
booleanColumnStatisticsData_numberOfTrues :: Lens.Lens' BooleanColumnStatisticsData Core.Natural
booleanColumnStatisticsData_numberOfTrues = Lens.lens (\BooleanColumnStatisticsData' {numberOfTrues} -> numberOfTrues) (\s@BooleanColumnStatisticsData' {} a -> s {numberOfTrues = a} :: BooleanColumnStatisticsData)

-- | The number of false values in the column.
booleanColumnStatisticsData_numberOfFalses :: Lens.Lens' BooleanColumnStatisticsData Core.Natural
booleanColumnStatisticsData_numberOfFalses = Lens.lens (\BooleanColumnStatisticsData' {numberOfFalses} -> numberOfFalses) (\s@BooleanColumnStatisticsData' {} a -> s {numberOfFalses = a} :: BooleanColumnStatisticsData)

-- | The number of null values in the column.
booleanColumnStatisticsData_numberOfNulls :: Lens.Lens' BooleanColumnStatisticsData Core.Natural
booleanColumnStatisticsData_numberOfNulls = Lens.lens (\BooleanColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@BooleanColumnStatisticsData' {} a -> s {numberOfNulls = a} :: BooleanColumnStatisticsData)

instance Core.FromJSON BooleanColumnStatisticsData where
  parseJSON =
    Core.withObject
      "BooleanColumnStatisticsData"
      ( \x ->
          BooleanColumnStatisticsData'
            Core.<$> (x Core..: "NumberOfTrues")
            Core.<*> (x Core..: "NumberOfFalses")
            Core.<*> (x Core..: "NumberOfNulls")
      )

instance Core.Hashable BooleanColumnStatisticsData

instance Core.NFData BooleanColumnStatisticsData

instance Core.ToJSON BooleanColumnStatisticsData where
  toJSON BooleanColumnStatisticsData' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NumberOfTrues" Core..= numberOfTrues),
            Core.Just ("NumberOfFalses" Core..= numberOfFalses),
            Core.Just ("NumberOfNulls" Core..= numberOfNulls)
          ]
      )
