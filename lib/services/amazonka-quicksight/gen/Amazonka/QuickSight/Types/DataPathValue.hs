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
-- Module      : Amazonka.QuickSight.Types.DataPathValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataPathValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data path that needs to be sorted.
--
-- /See:/ 'newDataPathValue' smart constructor.
data DataPathValue = DataPathValue'
  { -- | The field ID of the field that needs to be sorted.
    fieldId :: Prelude.Text,
    -- | The actual value of the field that needs to be sorted.
    fieldValue :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPathValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'dataPathValue_fieldId' - The field ID of the field that needs to be sorted.
--
-- 'fieldValue', 'dataPathValue_fieldValue' - The actual value of the field that needs to be sorted.
newDataPathValue ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'fieldValue'
  Prelude.Text ->
  DataPathValue
newDataPathValue pFieldId_ pFieldValue_ =
  DataPathValue'
    { fieldId = pFieldId_,
      fieldValue = Data._Sensitive Lens.# pFieldValue_
    }

-- | The field ID of the field that needs to be sorted.
dataPathValue_fieldId :: Lens.Lens' DataPathValue Prelude.Text
dataPathValue_fieldId = Lens.lens (\DataPathValue' {fieldId} -> fieldId) (\s@DataPathValue' {} a -> s {fieldId = a} :: DataPathValue)

-- | The actual value of the field that needs to be sorted.
dataPathValue_fieldValue :: Lens.Lens' DataPathValue Prelude.Text
dataPathValue_fieldValue = Lens.lens (\DataPathValue' {fieldValue} -> fieldValue) (\s@DataPathValue' {} a -> s {fieldValue = a} :: DataPathValue) Prelude.. Data._Sensitive

instance Data.FromJSON DataPathValue where
  parseJSON =
    Data.withObject
      "DataPathValue"
      ( \x ->
          DataPathValue'
            Prelude.<$> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "FieldValue")
      )

instance Prelude.Hashable DataPathValue where
  hashWithSalt _salt DataPathValue' {..} =
    _salt `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` fieldValue

instance Prelude.NFData DataPathValue where
  rnf DataPathValue' {..} =
    Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf fieldValue

instance Data.ToJSON DataPathValue where
  toJSON DataPathValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("FieldValue" Data..= fieldValue)
          ]
      )
