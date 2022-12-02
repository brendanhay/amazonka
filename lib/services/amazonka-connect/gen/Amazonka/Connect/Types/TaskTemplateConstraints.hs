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
-- Module      : Amazonka.Connect.Types.TaskTemplateConstraints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TaskTemplateConstraints where

import Amazonka.Connect.Types.InvisibleFieldInfo
import Amazonka.Connect.Types.ReadOnlyFieldInfo
import Amazonka.Connect.Types.RequiredFieldInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes constraints that apply to the template fields.
--
-- /See:/ 'newTaskTemplateConstraints' smart constructor.
data TaskTemplateConstraints = TaskTemplateConstraints'
  { -- | Lists the fields that are invisible to agents.
    invisibleFields :: Prelude.Maybe [InvisibleFieldInfo],
    -- | Lists the fields that are read-only to agents, and cannot be edited.
    readOnlyFields :: Prelude.Maybe [ReadOnlyFieldInfo],
    -- | Lists the fields that are required to be filled by agents.
    requiredFields :: Prelude.Maybe [RequiredFieldInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskTemplateConstraints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invisibleFields', 'taskTemplateConstraints_invisibleFields' - Lists the fields that are invisible to agents.
--
-- 'readOnlyFields', 'taskTemplateConstraints_readOnlyFields' - Lists the fields that are read-only to agents, and cannot be edited.
--
-- 'requiredFields', 'taskTemplateConstraints_requiredFields' - Lists the fields that are required to be filled by agents.
newTaskTemplateConstraints ::
  TaskTemplateConstraints
newTaskTemplateConstraints =
  TaskTemplateConstraints'
    { invisibleFields =
        Prelude.Nothing,
      readOnlyFields = Prelude.Nothing,
      requiredFields = Prelude.Nothing
    }

-- | Lists the fields that are invisible to agents.
taskTemplateConstraints_invisibleFields :: Lens.Lens' TaskTemplateConstraints (Prelude.Maybe [InvisibleFieldInfo])
taskTemplateConstraints_invisibleFields = Lens.lens (\TaskTemplateConstraints' {invisibleFields} -> invisibleFields) (\s@TaskTemplateConstraints' {} a -> s {invisibleFields = a} :: TaskTemplateConstraints) Prelude.. Lens.mapping Lens.coerced

-- | Lists the fields that are read-only to agents, and cannot be edited.
taskTemplateConstraints_readOnlyFields :: Lens.Lens' TaskTemplateConstraints (Prelude.Maybe [ReadOnlyFieldInfo])
taskTemplateConstraints_readOnlyFields = Lens.lens (\TaskTemplateConstraints' {readOnlyFields} -> readOnlyFields) (\s@TaskTemplateConstraints' {} a -> s {readOnlyFields = a} :: TaskTemplateConstraints) Prelude.. Lens.mapping Lens.coerced

-- | Lists the fields that are required to be filled by agents.
taskTemplateConstraints_requiredFields :: Lens.Lens' TaskTemplateConstraints (Prelude.Maybe [RequiredFieldInfo])
taskTemplateConstraints_requiredFields = Lens.lens (\TaskTemplateConstraints' {requiredFields} -> requiredFields) (\s@TaskTemplateConstraints' {} a -> s {requiredFields = a} :: TaskTemplateConstraints) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TaskTemplateConstraints where
  parseJSON =
    Data.withObject
      "TaskTemplateConstraints"
      ( \x ->
          TaskTemplateConstraints'
            Prelude.<$> ( x Data..:? "InvisibleFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ReadOnlyFields" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "RequiredFields"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TaskTemplateConstraints where
  hashWithSalt _salt TaskTemplateConstraints' {..} =
    _salt `Prelude.hashWithSalt` invisibleFields
      `Prelude.hashWithSalt` readOnlyFields
      `Prelude.hashWithSalt` requiredFields

instance Prelude.NFData TaskTemplateConstraints where
  rnf TaskTemplateConstraints' {..} =
    Prelude.rnf invisibleFields
      `Prelude.seq` Prelude.rnf readOnlyFields
      `Prelude.seq` Prelude.rnf requiredFields

instance Data.ToJSON TaskTemplateConstraints where
  toJSON TaskTemplateConstraints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InvisibleFields" Data..=)
              Prelude.<$> invisibleFields,
            ("ReadOnlyFields" Data..=)
              Prelude.<$> readOnlyFields,
            ("RequiredFields" Data..=)
              Prelude.<$> requiredFields
          ]
      )
