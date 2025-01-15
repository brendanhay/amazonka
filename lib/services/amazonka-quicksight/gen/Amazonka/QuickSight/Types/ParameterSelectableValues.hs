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
-- Module      : Amazonka.QuickSight.Types.ParameterSelectableValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterSelectableValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier

-- | A list of selectable values that are used in a control.
--
-- /See:/ 'newParameterSelectableValues' smart constructor.
data ParameterSelectableValues = ParameterSelectableValues'
  { -- | The column identifier that fetches values from the data set.
    linkToDataSetColumn :: Prelude.Maybe ColumnIdentifier,
    -- | The values that are used in @ParameterSelectableValues@.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterSelectableValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkToDataSetColumn', 'parameterSelectableValues_linkToDataSetColumn' - The column identifier that fetches values from the data set.
--
-- 'values', 'parameterSelectableValues_values' - The values that are used in @ParameterSelectableValues@.
newParameterSelectableValues ::
  ParameterSelectableValues
newParameterSelectableValues =
  ParameterSelectableValues'
    { linkToDataSetColumn =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The column identifier that fetches values from the data set.
parameterSelectableValues_linkToDataSetColumn :: Lens.Lens' ParameterSelectableValues (Prelude.Maybe ColumnIdentifier)
parameterSelectableValues_linkToDataSetColumn = Lens.lens (\ParameterSelectableValues' {linkToDataSetColumn} -> linkToDataSetColumn) (\s@ParameterSelectableValues' {} a -> s {linkToDataSetColumn = a} :: ParameterSelectableValues)

-- | The values that are used in @ParameterSelectableValues@.
parameterSelectableValues_values :: Lens.Lens' ParameterSelectableValues (Prelude.Maybe [Prelude.Text])
parameterSelectableValues_values = Lens.lens (\ParameterSelectableValues' {values} -> values) (\s@ParameterSelectableValues' {} a -> s {values = a} :: ParameterSelectableValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ParameterSelectableValues where
  parseJSON =
    Data.withObject
      "ParameterSelectableValues"
      ( \x ->
          ParameterSelectableValues'
            Prelude.<$> (x Data..:? "LinkToDataSetColumn")
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ParameterSelectableValues where
  hashWithSalt _salt ParameterSelectableValues' {..} =
    _salt
      `Prelude.hashWithSalt` linkToDataSetColumn
      `Prelude.hashWithSalt` values

instance Prelude.NFData ParameterSelectableValues where
  rnf ParameterSelectableValues' {..} =
    Prelude.rnf linkToDataSetColumn `Prelude.seq`
      Prelude.rnf values

instance Data.ToJSON ParameterSelectableValues where
  toJSON ParameterSelectableValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LinkToDataSetColumn" Data..=)
              Prelude.<$> linkToDataSetColumn,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
