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
-- Module      : Amazonka.Connect.Types.TaskTemplateField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TaskTemplateField where

import Amazonka.Connect.Types.TaskTemplateFieldIdentifier
import Amazonka.Connect.Types.TaskTemplateFieldType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a single task template field.
--
-- /See:/ 'newTaskTemplateField' smart constructor.
data TaskTemplateField = TaskTemplateField'
  { -- | The description of the field.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of options for a single select field.
    singleSelectOptions :: Prelude.Maybe [Prelude.Text],
    -- | Indicates the type of field.
    type' :: Prelude.Maybe TaskTemplateFieldType,
    -- | The unique identifier for the field.
    id :: TaskTemplateFieldIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskTemplateField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'taskTemplateField_description' - The description of the field.
--
-- 'singleSelectOptions', 'taskTemplateField_singleSelectOptions' - A list of options for a single select field.
--
-- 'type'', 'taskTemplateField_type' - Indicates the type of field.
--
-- 'id', 'taskTemplateField_id' - The unique identifier for the field.
newTaskTemplateField ::
  -- | 'id'
  TaskTemplateFieldIdentifier ->
  TaskTemplateField
newTaskTemplateField pId_ =
  TaskTemplateField'
    { description = Prelude.Nothing,
      singleSelectOptions = Prelude.Nothing,
      type' = Prelude.Nothing,
      id = pId_
    }

-- | The description of the field.
taskTemplateField_description :: Lens.Lens' TaskTemplateField (Prelude.Maybe Prelude.Text)
taskTemplateField_description = Lens.lens (\TaskTemplateField' {description} -> description) (\s@TaskTemplateField' {} a -> s {description = a} :: TaskTemplateField)

-- | A list of options for a single select field.
taskTemplateField_singleSelectOptions :: Lens.Lens' TaskTemplateField (Prelude.Maybe [Prelude.Text])
taskTemplateField_singleSelectOptions = Lens.lens (\TaskTemplateField' {singleSelectOptions} -> singleSelectOptions) (\s@TaskTemplateField' {} a -> s {singleSelectOptions = a} :: TaskTemplateField) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the type of field.
taskTemplateField_type :: Lens.Lens' TaskTemplateField (Prelude.Maybe TaskTemplateFieldType)
taskTemplateField_type = Lens.lens (\TaskTemplateField' {type'} -> type') (\s@TaskTemplateField' {} a -> s {type' = a} :: TaskTemplateField)

-- | The unique identifier for the field.
taskTemplateField_id :: Lens.Lens' TaskTemplateField TaskTemplateFieldIdentifier
taskTemplateField_id = Lens.lens (\TaskTemplateField' {id} -> id) (\s@TaskTemplateField' {} a -> s {id = a} :: TaskTemplateField)

instance Data.FromJSON TaskTemplateField where
  parseJSON =
    Data.withObject
      "TaskTemplateField"
      ( \x ->
          TaskTemplateField'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> ( x
                            Data..:? "SingleSelectOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "Id")
      )

instance Prelude.Hashable TaskTemplateField where
  hashWithSalt _salt TaskTemplateField' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` singleSelectOptions
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id

instance Prelude.NFData TaskTemplateField where
  rnf TaskTemplateField' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf singleSelectOptions
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON TaskTemplateField where
  toJSON TaskTemplateField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("SingleSelectOptions" Data..=)
              Prelude.<$> singleSelectOptions,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("Id" Data..= id)
          ]
      )
