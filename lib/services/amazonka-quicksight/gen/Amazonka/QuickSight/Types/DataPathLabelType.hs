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
-- Module      : Amazonka.QuickSight.Types.DataPathLabelType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataPathLabelType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The option that specifies individual data values for labels.
--
-- /See:/ 'newDataPathLabelType' smart constructor.
data DataPathLabelType = DataPathLabelType'
  { -- | The field ID of the field that the data label needs to be applied to.
    fieldId :: Prelude.Maybe Prelude.Text,
    -- | The actual value of the field that is labeled.
    fieldValue :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The visibility of the data label.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPathLabelType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'dataPathLabelType_fieldId' - The field ID of the field that the data label needs to be applied to.
--
-- 'fieldValue', 'dataPathLabelType_fieldValue' - The actual value of the field that is labeled.
--
-- 'visibility', 'dataPathLabelType_visibility' - The visibility of the data label.
newDataPathLabelType ::
  DataPathLabelType
newDataPathLabelType =
  DataPathLabelType'
    { fieldId = Prelude.Nothing,
      fieldValue = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The field ID of the field that the data label needs to be applied to.
dataPathLabelType_fieldId :: Lens.Lens' DataPathLabelType (Prelude.Maybe Prelude.Text)
dataPathLabelType_fieldId = Lens.lens (\DataPathLabelType' {fieldId} -> fieldId) (\s@DataPathLabelType' {} a -> s {fieldId = a} :: DataPathLabelType)

-- | The actual value of the field that is labeled.
dataPathLabelType_fieldValue :: Lens.Lens' DataPathLabelType (Prelude.Maybe Prelude.Text)
dataPathLabelType_fieldValue = Lens.lens (\DataPathLabelType' {fieldValue} -> fieldValue) (\s@DataPathLabelType' {} a -> s {fieldValue = a} :: DataPathLabelType) Prelude.. Lens.mapping Data._Sensitive

-- | The visibility of the data label.
dataPathLabelType_visibility :: Lens.Lens' DataPathLabelType (Prelude.Maybe Visibility)
dataPathLabelType_visibility = Lens.lens (\DataPathLabelType' {visibility} -> visibility) (\s@DataPathLabelType' {} a -> s {visibility = a} :: DataPathLabelType)

instance Data.FromJSON DataPathLabelType where
  parseJSON =
    Data.withObject
      "DataPathLabelType"
      ( \x ->
          DataPathLabelType'
            Prelude.<$> (x Data..:? "FieldId")
            Prelude.<*> (x Data..:? "FieldValue")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable DataPathLabelType where
  hashWithSalt _salt DataPathLabelType' {..} =
    _salt
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` fieldValue
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData DataPathLabelType where
  rnf DataPathLabelType' {..} =
    Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf fieldValue
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON DataPathLabelType where
  toJSON DataPathLabelType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldId" Data..=) Prelude.<$> fieldId,
            ("FieldValue" Data..=) Prelude.<$> fieldValue,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
