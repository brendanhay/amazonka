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
-- Module      : Amazonka.IoT.Types.Field
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Field where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.FieldType
import qualified Amazonka.Prelude as Prelude

-- | Describes the name and data type at a field.
--
-- /See:/ 'newField' smart constructor.
data Field = Field'
  { -- | The name of the field.
    name :: Prelude.Maybe Prelude.Text,
    -- | The data type of the field.
    type' :: Prelude.Maybe FieldType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Field' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'field_name' - The name of the field.
--
-- 'type'', 'field_type' - The data type of the field.
newField ::
  Field
newField =
  Field'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the field.
field_name :: Lens.Lens' Field (Prelude.Maybe Prelude.Text)
field_name = Lens.lens (\Field' {name} -> name) (\s@Field' {} a -> s {name = a} :: Field)

-- | The data type of the field.
field_type :: Lens.Lens' Field (Prelude.Maybe FieldType)
field_type = Lens.lens (\Field' {type'} -> type') (\s@Field' {} a -> s {type' = a} :: Field)

instance Data.FromJSON Field where
  parseJSON =
    Data.withObject
      "Field"
      ( \x ->
          Field'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Field where
  hashWithSalt _salt Field' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Field where
  rnf Field' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON Field where
  toJSON Field' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("type" Data..=) Prelude.<$> type'
          ]
      )
