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
-- Module      : Amazonka.QuickSight.Types.DataLabelType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataLabelType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataPathLabelType
import Amazonka.QuickSight.Types.FieldLabelType
import Amazonka.QuickSight.Types.MaximumLabelType
import Amazonka.QuickSight.Types.MinimumLabelType
import Amazonka.QuickSight.Types.RangeEndsLabelType

-- | The option that determines the data label type.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newDataLabelType' smart constructor.
data DataLabelType = DataLabelType'
  { -- | The option that specifies individual data values for labels.
    dataPathLabelType :: Prelude.Maybe DataPathLabelType,
    -- | Determines the label configuration for the entire field.
    fieldLabelType :: Prelude.Maybe FieldLabelType,
    -- | Determines the label configuration for the maximum value in a visual.
    maximumLabelType :: Prelude.Maybe MaximumLabelType,
    -- | Determines the label configuration for the minimum value in a visual.
    minimumLabelType :: Prelude.Maybe MinimumLabelType,
    -- | Determines the label configuration for range end value in a visual.
    rangeEndsLabelType :: Prelude.Maybe RangeEndsLabelType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLabelType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataPathLabelType', 'dataLabelType_dataPathLabelType' - The option that specifies individual data values for labels.
--
-- 'fieldLabelType', 'dataLabelType_fieldLabelType' - Determines the label configuration for the entire field.
--
-- 'maximumLabelType', 'dataLabelType_maximumLabelType' - Determines the label configuration for the maximum value in a visual.
--
-- 'minimumLabelType', 'dataLabelType_minimumLabelType' - Determines the label configuration for the minimum value in a visual.
--
-- 'rangeEndsLabelType', 'dataLabelType_rangeEndsLabelType' - Determines the label configuration for range end value in a visual.
newDataLabelType ::
  DataLabelType
newDataLabelType =
  DataLabelType'
    { dataPathLabelType = Prelude.Nothing,
      fieldLabelType = Prelude.Nothing,
      maximumLabelType = Prelude.Nothing,
      minimumLabelType = Prelude.Nothing,
      rangeEndsLabelType = Prelude.Nothing
    }

-- | The option that specifies individual data values for labels.
dataLabelType_dataPathLabelType :: Lens.Lens' DataLabelType (Prelude.Maybe DataPathLabelType)
dataLabelType_dataPathLabelType = Lens.lens (\DataLabelType' {dataPathLabelType} -> dataPathLabelType) (\s@DataLabelType' {} a -> s {dataPathLabelType = a} :: DataLabelType)

-- | Determines the label configuration for the entire field.
dataLabelType_fieldLabelType :: Lens.Lens' DataLabelType (Prelude.Maybe FieldLabelType)
dataLabelType_fieldLabelType = Lens.lens (\DataLabelType' {fieldLabelType} -> fieldLabelType) (\s@DataLabelType' {} a -> s {fieldLabelType = a} :: DataLabelType)

-- | Determines the label configuration for the maximum value in a visual.
dataLabelType_maximumLabelType :: Lens.Lens' DataLabelType (Prelude.Maybe MaximumLabelType)
dataLabelType_maximumLabelType = Lens.lens (\DataLabelType' {maximumLabelType} -> maximumLabelType) (\s@DataLabelType' {} a -> s {maximumLabelType = a} :: DataLabelType)

-- | Determines the label configuration for the minimum value in a visual.
dataLabelType_minimumLabelType :: Lens.Lens' DataLabelType (Prelude.Maybe MinimumLabelType)
dataLabelType_minimumLabelType = Lens.lens (\DataLabelType' {minimumLabelType} -> minimumLabelType) (\s@DataLabelType' {} a -> s {minimumLabelType = a} :: DataLabelType)

-- | Determines the label configuration for range end value in a visual.
dataLabelType_rangeEndsLabelType :: Lens.Lens' DataLabelType (Prelude.Maybe RangeEndsLabelType)
dataLabelType_rangeEndsLabelType = Lens.lens (\DataLabelType' {rangeEndsLabelType} -> rangeEndsLabelType) (\s@DataLabelType' {} a -> s {rangeEndsLabelType = a} :: DataLabelType)

instance Data.FromJSON DataLabelType where
  parseJSON =
    Data.withObject
      "DataLabelType"
      ( \x ->
          DataLabelType'
            Prelude.<$> (x Data..:? "DataPathLabelType")
            Prelude.<*> (x Data..:? "FieldLabelType")
            Prelude.<*> (x Data..:? "MaximumLabelType")
            Prelude.<*> (x Data..:? "MinimumLabelType")
            Prelude.<*> (x Data..:? "RangeEndsLabelType")
      )

instance Prelude.Hashable DataLabelType where
  hashWithSalt _salt DataLabelType' {..} =
    _salt
      `Prelude.hashWithSalt` dataPathLabelType
      `Prelude.hashWithSalt` fieldLabelType
      `Prelude.hashWithSalt` maximumLabelType
      `Prelude.hashWithSalt` minimumLabelType
      `Prelude.hashWithSalt` rangeEndsLabelType

instance Prelude.NFData DataLabelType where
  rnf DataLabelType' {..} =
    Prelude.rnf dataPathLabelType
      `Prelude.seq` Prelude.rnf fieldLabelType
      `Prelude.seq` Prelude.rnf maximumLabelType
      `Prelude.seq` Prelude.rnf minimumLabelType
      `Prelude.seq` Prelude.rnf rangeEndsLabelType

instance Data.ToJSON DataLabelType where
  toJSON DataLabelType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataPathLabelType" Data..=)
              Prelude.<$> dataPathLabelType,
            ("FieldLabelType" Data..=)
              Prelude.<$> fieldLabelType,
            ("MaximumLabelType" Data..=)
              Prelude.<$> maximumLabelType,
            ("MinimumLabelType" Data..=)
              Prelude.<$> minimumLabelType,
            ("RangeEndsLabelType" Data..=)
              Prelude.<$> rangeEndsLabelType
          ]
      )
