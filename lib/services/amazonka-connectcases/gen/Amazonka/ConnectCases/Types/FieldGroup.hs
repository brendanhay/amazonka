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
-- Module      : Amazonka.ConnectCases.Types.FieldGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldGroup where

import Amazonka.ConnectCases.Types.FieldItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for a group of fields and associated properties.
--
-- /See:/ 'newFieldGroup' smart constructor.
data FieldGroup = FieldGroup'
  { -- | Name of the field group.
    name :: Prelude.Maybe Prelude.Text,
    -- | Represents an ordered list containing field related information.
    fields :: [FieldItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'fieldGroup_name' - Name of the field group.
--
-- 'fields', 'fieldGroup_fields' - Represents an ordered list containing field related information.
newFieldGroup ::
  FieldGroup
newFieldGroup =
  FieldGroup'
    { name = Prelude.Nothing,
      fields = Prelude.mempty
    }

-- | Name of the field group.
fieldGroup_name :: Lens.Lens' FieldGroup (Prelude.Maybe Prelude.Text)
fieldGroup_name = Lens.lens (\FieldGroup' {name} -> name) (\s@FieldGroup' {} a -> s {name = a} :: FieldGroup)

-- | Represents an ordered list containing field related information.
fieldGroup_fields :: Lens.Lens' FieldGroup [FieldItem]
fieldGroup_fields = Lens.lens (\FieldGroup' {fields} -> fields) (\s@FieldGroup' {} a -> s {fields = a} :: FieldGroup) Prelude.. Lens.coerced

instance Data.FromJSON FieldGroup where
  parseJSON =
    Data.withObject
      "FieldGroup"
      ( \x ->
          FieldGroup'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "fields" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FieldGroup where
  hashWithSalt _salt FieldGroup' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` fields

instance Prelude.NFData FieldGroup where
  rnf FieldGroup' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf fields

instance Data.ToJSON FieldGroup where
  toJSON FieldGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("fields" Data..= fields)
          ]
      )
