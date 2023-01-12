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
-- Module      : Amazonka.Glue.Types.NullValueField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.NullValueField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Datatype
import qualified Amazonka.Prelude as Prelude

-- | Represents a custom null value such as a zeros or other value being used
-- as a null placeholder unique to the dataset.
--
-- /See:/ 'newNullValueField' smart constructor.
data NullValueField = NullValueField'
  { -- | The value of the null placeholder.
    value :: Prelude.Text,
    -- | The datatype of the value.
    datatype :: Datatype
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NullValueField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'nullValueField_value' - The value of the null placeholder.
--
-- 'datatype', 'nullValueField_datatype' - The datatype of the value.
newNullValueField ::
  -- | 'value'
  Prelude.Text ->
  -- | 'datatype'
  Datatype ->
  NullValueField
newNullValueField pValue_ pDatatype_ =
  NullValueField'
    { value = pValue_,
      datatype = pDatatype_
    }

-- | The value of the null placeholder.
nullValueField_value :: Lens.Lens' NullValueField Prelude.Text
nullValueField_value = Lens.lens (\NullValueField' {value} -> value) (\s@NullValueField' {} a -> s {value = a} :: NullValueField)

-- | The datatype of the value.
nullValueField_datatype :: Lens.Lens' NullValueField Datatype
nullValueField_datatype = Lens.lens (\NullValueField' {datatype} -> datatype) (\s@NullValueField' {} a -> s {datatype = a} :: NullValueField)

instance Data.FromJSON NullValueField where
  parseJSON =
    Data.withObject
      "NullValueField"
      ( \x ->
          NullValueField'
            Prelude.<$> (x Data..: "Value")
            Prelude.<*> (x Data..: "Datatype")
      )

instance Prelude.Hashable NullValueField where
  hashWithSalt _salt NullValueField' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` datatype

instance Prelude.NFData NullValueField where
  rnf NullValueField' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf datatype

instance Data.ToJSON NullValueField where
  toJSON NullValueField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Value" Data..= value),
            Prelude.Just ("Datatype" Data..= datatype)
          ]
      )
