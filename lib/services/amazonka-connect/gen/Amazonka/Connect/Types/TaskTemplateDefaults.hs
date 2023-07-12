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
-- Module      : Amazonka.Connect.Types.TaskTemplateDefaults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TaskTemplateDefaults where

import Amazonka.Connect.Types.TaskTemplateDefaultFieldValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes default values for fields on a template.
--
-- /See:/ 'newTaskTemplateDefaults' smart constructor.
data TaskTemplateDefaults = TaskTemplateDefaults'
  { -- | Default value for the field.
    defaultFieldValues :: Prelude.Maybe [TaskTemplateDefaultFieldValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskTemplateDefaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultFieldValues', 'taskTemplateDefaults_defaultFieldValues' - Default value for the field.
newTaskTemplateDefaults ::
  TaskTemplateDefaults
newTaskTemplateDefaults =
  TaskTemplateDefaults'
    { defaultFieldValues =
        Prelude.Nothing
    }

-- | Default value for the field.
taskTemplateDefaults_defaultFieldValues :: Lens.Lens' TaskTemplateDefaults (Prelude.Maybe [TaskTemplateDefaultFieldValue])
taskTemplateDefaults_defaultFieldValues = Lens.lens (\TaskTemplateDefaults' {defaultFieldValues} -> defaultFieldValues) (\s@TaskTemplateDefaults' {} a -> s {defaultFieldValues = a} :: TaskTemplateDefaults) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TaskTemplateDefaults where
  parseJSON =
    Data.withObject
      "TaskTemplateDefaults"
      ( \x ->
          TaskTemplateDefaults'
            Prelude.<$> ( x
                            Data..:? "DefaultFieldValues"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TaskTemplateDefaults where
  hashWithSalt _salt TaskTemplateDefaults' {..} =
    _salt `Prelude.hashWithSalt` defaultFieldValues

instance Prelude.NFData TaskTemplateDefaults where
  rnf TaskTemplateDefaults' {..} =
    Prelude.rnf defaultFieldValues

instance Data.ToJSON TaskTemplateDefaults where
  toJSON TaskTemplateDefaults' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultFieldValues" Data..=)
              Prelude.<$> defaultFieldValues
          ]
      )
