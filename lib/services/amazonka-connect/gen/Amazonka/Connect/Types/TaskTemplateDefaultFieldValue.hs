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
-- Module      : Amazonka.Connect.Types.TaskTemplateDefaultFieldValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TaskTemplateDefaultFieldValue where

import Amazonka.Connect.Types.TaskTemplateFieldIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a default field and its corresponding value.
--
-- /See:/ 'newTaskTemplateDefaultFieldValue' smart constructor.
data TaskTemplateDefaultFieldValue = TaskTemplateDefaultFieldValue'
  { -- | Default value for the field.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Identifier of a field.
    id :: Prelude.Maybe TaskTemplateFieldIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskTemplateDefaultFieldValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'taskTemplateDefaultFieldValue_defaultValue' - Default value for the field.
--
-- 'id', 'taskTemplateDefaultFieldValue_id' - Identifier of a field.
newTaskTemplateDefaultFieldValue ::
  TaskTemplateDefaultFieldValue
newTaskTemplateDefaultFieldValue =
  TaskTemplateDefaultFieldValue'
    { defaultValue =
        Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Default value for the field.
taskTemplateDefaultFieldValue_defaultValue :: Lens.Lens' TaskTemplateDefaultFieldValue (Prelude.Maybe Prelude.Text)
taskTemplateDefaultFieldValue_defaultValue = Lens.lens (\TaskTemplateDefaultFieldValue' {defaultValue} -> defaultValue) (\s@TaskTemplateDefaultFieldValue' {} a -> s {defaultValue = a} :: TaskTemplateDefaultFieldValue)

-- | Identifier of a field.
taskTemplateDefaultFieldValue_id :: Lens.Lens' TaskTemplateDefaultFieldValue (Prelude.Maybe TaskTemplateFieldIdentifier)
taskTemplateDefaultFieldValue_id = Lens.lens (\TaskTemplateDefaultFieldValue' {id} -> id) (\s@TaskTemplateDefaultFieldValue' {} a -> s {id = a} :: TaskTemplateDefaultFieldValue)

instance Data.FromJSON TaskTemplateDefaultFieldValue where
  parseJSON =
    Data.withObject
      "TaskTemplateDefaultFieldValue"
      ( \x ->
          TaskTemplateDefaultFieldValue'
            Prelude.<$> (x Data..:? "DefaultValue")
            Prelude.<*> (x Data..:? "Id")
      )

instance
  Prelude.Hashable
    TaskTemplateDefaultFieldValue
  where
  hashWithSalt _salt TaskTemplateDefaultFieldValue' {..} =
    _salt `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` id

instance Prelude.NFData TaskTemplateDefaultFieldValue where
  rnf TaskTemplateDefaultFieldValue' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON TaskTemplateDefaultFieldValue where
  toJSON TaskTemplateDefaultFieldValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValue" Data..=) Prelude.<$> defaultValue,
            ("Id" Data..=) Prelude.<$> id
          ]
      )
