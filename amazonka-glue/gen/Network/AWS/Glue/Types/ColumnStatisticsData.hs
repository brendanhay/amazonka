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
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsData where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.BinaryColumnStatisticsData
import Network.AWS.Glue.Types.BooleanColumnStatisticsData
import Network.AWS.Glue.Types.ColumnStatisticsType
import Network.AWS.Glue.Types.DateColumnStatisticsData
import Network.AWS.Glue.Types.DecimalColumnStatisticsData
import Network.AWS.Glue.Types.DoubleColumnStatisticsData
import Network.AWS.Glue.Types.LongColumnStatisticsData
import Network.AWS.Glue.Types.StringColumnStatisticsData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the individual types of column statistics data. Only one data
-- object should be set and indicated by the @Type@ attribute.
--
-- /See:/ 'newColumnStatisticsData' smart constructor.
data ColumnStatisticsData = ColumnStatisticsData'
  { -- | Date column statistics data.
    dateColumnStatisticsData :: Prelude.Maybe DateColumnStatisticsData,
    -- | Binary column statistics data.
    binaryColumnStatisticsData :: Prelude.Maybe BinaryColumnStatisticsData,
    -- | Boolean column statistics data.
    booleanColumnStatisticsData :: Prelude.Maybe BooleanColumnStatisticsData,
    -- | Long column statistics data.
    longColumnStatisticsData :: Prelude.Maybe LongColumnStatisticsData,
    -- | String column statistics data.
    stringColumnStatisticsData :: Prelude.Maybe StringColumnStatisticsData,
    -- | Double column statistics data.
    doubleColumnStatisticsData :: Prelude.Maybe DoubleColumnStatisticsData,
    -- | Decimal column statistics data.
    decimalColumnStatisticsData :: Prelude.Maybe DecimalColumnStatisticsData,
    -- | The type of column statistics data.
    type' :: ColumnStatisticsType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnStatisticsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateColumnStatisticsData', 'columnStatisticsData_dateColumnStatisticsData' - Date column statistics data.
--
-- 'binaryColumnStatisticsData', 'columnStatisticsData_binaryColumnStatisticsData' - Binary column statistics data.
--
-- 'booleanColumnStatisticsData', 'columnStatisticsData_booleanColumnStatisticsData' - Boolean column statistics data.
--
-- 'longColumnStatisticsData', 'columnStatisticsData_longColumnStatisticsData' - Long column statistics data.
--
-- 'stringColumnStatisticsData', 'columnStatisticsData_stringColumnStatisticsData' - String column statistics data.
--
-- 'doubleColumnStatisticsData', 'columnStatisticsData_doubleColumnStatisticsData' - Double column statistics data.
--
-- 'decimalColumnStatisticsData', 'columnStatisticsData_decimalColumnStatisticsData' - Decimal column statistics data.
--
-- 'type'', 'columnStatisticsData_type' - The type of column statistics data.
newColumnStatisticsData ::
  -- | 'type''
  ColumnStatisticsType ->
  ColumnStatisticsData
newColumnStatisticsData pType_ =
  ColumnStatisticsData'
    { dateColumnStatisticsData =
        Prelude.Nothing,
      binaryColumnStatisticsData = Prelude.Nothing,
      booleanColumnStatisticsData = Prelude.Nothing,
      longColumnStatisticsData = Prelude.Nothing,
      stringColumnStatisticsData = Prelude.Nothing,
      doubleColumnStatisticsData = Prelude.Nothing,
      decimalColumnStatisticsData = Prelude.Nothing,
      type' = pType_
    }

-- | Date column statistics data.
columnStatisticsData_dateColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Prelude.Maybe DateColumnStatisticsData)
columnStatisticsData_dateColumnStatisticsData = Lens.lens (\ColumnStatisticsData' {dateColumnStatisticsData} -> dateColumnStatisticsData) (\s@ColumnStatisticsData' {} a -> s {dateColumnStatisticsData = a} :: ColumnStatisticsData)

-- | Binary column statistics data.
columnStatisticsData_binaryColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Prelude.Maybe BinaryColumnStatisticsData)
columnStatisticsData_binaryColumnStatisticsData = Lens.lens (\ColumnStatisticsData' {binaryColumnStatisticsData} -> binaryColumnStatisticsData) (\s@ColumnStatisticsData' {} a -> s {binaryColumnStatisticsData = a} :: ColumnStatisticsData)

-- | Boolean column statistics data.
columnStatisticsData_booleanColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Prelude.Maybe BooleanColumnStatisticsData)
columnStatisticsData_booleanColumnStatisticsData = Lens.lens (\ColumnStatisticsData' {booleanColumnStatisticsData} -> booleanColumnStatisticsData) (\s@ColumnStatisticsData' {} a -> s {booleanColumnStatisticsData = a} :: ColumnStatisticsData)

-- | Long column statistics data.
columnStatisticsData_longColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Prelude.Maybe LongColumnStatisticsData)
columnStatisticsData_longColumnStatisticsData = Lens.lens (\ColumnStatisticsData' {longColumnStatisticsData} -> longColumnStatisticsData) (\s@ColumnStatisticsData' {} a -> s {longColumnStatisticsData = a} :: ColumnStatisticsData)

-- | String column statistics data.
columnStatisticsData_stringColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Prelude.Maybe StringColumnStatisticsData)
columnStatisticsData_stringColumnStatisticsData = Lens.lens (\ColumnStatisticsData' {stringColumnStatisticsData} -> stringColumnStatisticsData) (\s@ColumnStatisticsData' {} a -> s {stringColumnStatisticsData = a} :: ColumnStatisticsData)

-- | Double column statistics data.
columnStatisticsData_doubleColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Prelude.Maybe DoubleColumnStatisticsData)
columnStatisticsData_doubleColumnStatisticsData = Lens.lens (\ColumnStatisticsData' {doubleColumnStatisticsData} -> doubleColumnStatisticsData) (\s@ColumnStatisticsData' {} a -> s {doubleColumnStatisticsData = a} :: ColumnStatisticsData)

-- | Decimal column statistics data.
columnStatisticsData_decimalColumnStatisticsData :: Lens.Lens' ColumnStatisticsData (Prelude.Maybe DecimalColumnStatisticsData)
columnStatisticsData_decimalColumnStatisticsData = Lens.lens (\ColumnStatisticsData' {decimalColumnStatisticsData} -> decimalColumnStatisticsData) (\s@ColumnStatisticsData' {} a -> s {decimalColumnStatisticsData = a} :: ColumnStatisticsData)

-- | The type of column statistics data.
columnStatisticsData_type :: Lens.Lens' ColumnStatisticsData ColumnStatisticsType
columnStatisticsData_type = Lens.lens (\ColumnStatisticsData' {type'} -> type') (\s@ColumnStatisticsData' {} a -> s {type' = a} :: ColumnStatisticsData)

instance Core.FromJSON ColumnStatisticsData where
  parseJSON =
    Core.withObject
      "ColumnStatisticsData"
      ( \x ->
          ColumnStatisticsData'
            Prelude.<$> (x Core..:? "DateColumnStatisticsData")
            Prelude.<*> (x Core..:? "BinaryColumnStatisticsData")
            Prelude.<*> (x Core..:? "BooleanColumnStatisticsData")
            Prelude.<*> (x Core..:? "LongColumnStatisticsData")
            Prelude.<*> (x Core..:? "StringColumnStatisticsData")
            Prelude.<*> (x Core..:? "DoubleColumnStatisticsData")
            Prelude.<*> (x Core..:? "DecimalColumnStatisticsData")
            Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable ColumnStatisticsData

instance Prelude.NFData ColumnStatisticsData

instance Core.ToJSON ColumnStatisticsData where
  toJSON ColumnStatisticsData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DateColumnStatisticsData" Core..=)
              Prelude.<$> dateColumnStatisticsData,
            ("BinaryColumnStatisticsData" Core..=)
              Prelude.<$> binaryColumnStatisticsData,
            ("BooleanColumnStatisticsData" Core..=)
              Prelude.<$> booleanColumnStatisticsData,
            ("LongColumnStatisticsData" Core..=)
              Prelude.<$> longColumnStatisticsData,
            ("StringColumnStatisticsData" Core..=)
              Prelude.<$> stringColumnStatisticsData,
            ("DoubleColumnStatisticsData" Core..=)
              Prelude.<$> doubleColumnStatisticsData,
            ("DecimalColumnStatisticsData" Core..=)
              Prelude.<$> decimalColumnStatisticsData,
            Prelude.Just ("Type" Core..= type')
          ]
      )
