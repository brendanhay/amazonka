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
-- Module      : Amazonka.ConnectCases.Types.FieldValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldValue where

import Amazonka.ConnectCases.Types.FieldValueUnion
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for case field values.
--
-- /See:/ 'newFieldValue' smart constructor.
data FieldValue = FieldValue'
  { -- | Unique identifier of a field.
    id :: Prelude.Text,
    -- | Union of potential field value types.
    value :: FieldValueUnion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'fieldValue_id' - Unique identifier of a field.
--
-- 'value', 'fieldValue_value' - Union of potential field value types.
newFieldValue ::
  -- | 'id'
  Prelude.Text ->
  -- | 'value'
  FieldValueUnion ->
  FieldValue
newFieldValue pId_ pValue_ =
  FieldValue' {id = pId_, value = pValue_}

-- | Unique identifier of a field.
fieldValue_id :: Lens.Lens' FieldValue Prelude.Text
fieldValue_id = Lens.lens (\FieldValue' {id} -> id) (\s@FieldValue' {} a -> s {id = a} :: FieldValue)

-- | Union of potential field value types.
fieldValue_value :: Lens.Lens' FieldValue FieldValueUnion
fieldValue_value = Lens.lens (\FieldValue' {value} -> value) (\s@FieldValue' {} a -> s {value = a} :: FieldValue)

instance Data.FromJSON FieldValue where
  parseJSON =
    Data.withObject
      "FieldValue"
      ( \x ->
          FieldValue'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable FieldValue where
  hashWithSalt _salt FieldValue' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` value

instance Prelude.NFData FieldValue where
  rnf FieldValue' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf value

instance Data.ToJSON FieldValue where
  toJSON FieldValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Data..= id),
            Prelude.Just ("value" Data..= value)
          ]
      )
