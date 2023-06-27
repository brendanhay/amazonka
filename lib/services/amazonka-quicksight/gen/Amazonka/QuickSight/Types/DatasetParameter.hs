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
-- Module      : Amazonka.QuickSight.Types.DatasetParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DatasetParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateTimeDatasetParameter
import Amazonka.QuickSight.Types.DecimalDatasetParameter
import Amazonka.QuickSight.Types.IntegerDatasetParameter
import Amazonka.QuickSight.Types.StringDatasetParameter

-- | A dataset parameter.
--
-- /See:/ 'newDatasetParameter' smart constructor.
data DatasetParameter = DatasetParameter'
  { -- | A date time parameter that is created in the dataset.
    dateTimeDatasetParameter :: Prelude.Maybe DateTimeDatasetParameter,
    -- | A decimal parameter that is created in the dataset.
    decimalDatasetParameter :: Prelude.Maybe DecimalDatasetParameter,
    -- | An integer parameter that is created in the dataset.
    integerDatasetParameter :: Prelude.Maybe IntegerDatasetParameter,
    -- | A string parameter that is created in the dataset.
    stringDatasetParameter :: Prelude.Maybe StringDatasetParameter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeDatasetParameter', 'datasetParameter_dateTimeDatasetParameter' - A date time parameter that is created in the dataset.
--
-- 'decimalDatasetParameter', 'datasetParameter_decimalDatasetParameter' - A decimal parameter that is created in the dataset.
--
-- 'integerDatasetParameter', 'datasetParameter_integerDatasetParameter' - An integer parameter that is created in the dataset.
--
-- 'stringDatasetParameter', 'datasetParameter_stringDatasetParameter' - A string parameter that is created in the dataset.
newDatasetParameter ::
  DatasetParameter
newDatasetParameter =
  DatasetParameter'
    { dateTimeDatasetParameter =
        Prelude.Nothing,
      decimalDatasetParameter = Prelude.Nothing,
      integerDatasetParameter = Prelude.Nothing,
      stringDatasetParameter = Prelude.Nothing
    }

-- | A date time parameter that is created in the dataset.
datasetParameter_dateTimeDatasetParameter :: Lens.Lens' DatasetParameter (Prelude.Maybe DateTimeDatasetParameter)
datasetParameter_dateTimeDatasetParameter = Lens.lens (\DatasetParameter' {dateTimeDatasetParameter} -> dateTimeDatasetParameter) (\s@DatasetParameter' {} a -> s {dateTimeDatasetParameter = a} :: DatasetParameter)

-- | A decimal parameter that is created in the dataset.
datasetParameter_decimalDatasetParameter :: Lens.Lens' DatasetParameter (Prelude.Maybe DecimalDatasetParameter)
datasetParameter_decimalDatasetParameter = Lens.lens (\DatasetParameter' {decimalDatasetParameter} -> decimalDatasetParameter) (\s@DatasetParameter' {} a -> s {decimalDatasetParameter = a} :: DatasetParameter)

-- | An integer parameter that is created in the dataset.
datasetParameter_integerDatasetParameter :: Lens.Lens' DatasetParameter (Prelude.Maybe IntegerDatasetParameter)
datasetParameter_integerDatasetParameter = Lens.lens (\DatasetParameter' {integerDatasetParameter} -> integerDatasetParameter) (\s@DatasetParameter' {} a -> s {integerDatasetParameter = a} :: DatasetParameter)

-- | A string parameter that is created in the dataset.
datasetParameter_stringDatasetParameter :: Lens.Lens' DatasetParameter (Prelude.Maybe StringDatasetParameter)
datasetParameter_stringDatasetParameter = Lens.lens (\DatasetParameter' {stringDatasetParameter} -> stringDatasetParameter) (\s@DatasetParameter' {} a -> s {stringDatasetParameter = a} :: DatasetParameter)

instance Data.FromJSON DatasetParameter where
  parseJSON =
    Data.withObject
      "DatasetParameter"
      ( \x ->
          DatasetParameter'
            Prelude.<$> (x Data..:? "DateTimeDatasetParameter")
            Prelude.<*> (x Data..:? "DecimalDatasetParameter")
            Prelude.<*> (x Data..:? "IntegerDatasetParameter")
            Prelude.<*> (x Data..:? "StringDatasetParameter")
      )

instance Prelude.Hashable DatasetParameter where
  hashWithSalt _salt DatasetParameter' {..} =
    _salt
      `Prelude.hashWithSalt` dateTimeDatasetParameter
      `Prelude.hashWithSalt` decimalDatasetParameter
      `Prelude.hashWithSalt` integerDatasetParameter
      `Prelude.hashWithSalt` stringDatasetParameter

instance Prelude.NFData DatasetParameter where
  rnf DatasetParameter' {..} =
    Prelude.rnf dateTimeDatasetParameter
      `Prelude.seq` Prelude.rnf decimalDatasetParameter
      `Prelude.seq` Prelude.rnf integerDatasetParameter
      `Prelude.seq` Prelude.rnf stringDatasetParameter

instance Data.ToJSON DatasetParameter where
  toJSON DatasetParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeDatasetParameter" Data..=)
              Prelude.<$> dateTimeDatasetParameter,
            ("DecimalDatasetParameter" Data..=)
              Prelude.<$> decimalDatasetParameter,
            ("IntegerDatasetParameter" Data..=)
              Prelude.<$> integerDatasetParameter,
            ("StringDatasetParameter" Data..=)
              Prelude.<$> stringDatasetParameter
          ]
      )
