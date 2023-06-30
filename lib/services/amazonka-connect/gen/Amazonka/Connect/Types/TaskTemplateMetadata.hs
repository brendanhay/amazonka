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
-- Module      : Amazonka.Connect.Types.TaskTemplateMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TaskTemplateMetadata where

import Amazonka.Connect.Types.TaskTemplateStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about the task template.
--
-- /See:/ 'newTaskTemplateMetadata' smart constructor.
data TaskTemplateMetadata = TaskTemplateMetadata'
  { -- | The Amazon Resource Name (ARN) of the task template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the task template was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the task template.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the task template.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the task template was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the task template.
    name :: Prelude.Maybe Prelude.Text,
    -- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
    -- Tasks can only be created from @ACTIVE@ templates. If a template is
    -- marked as @INACTIVE@, then a task that refers to this template cannot be
    -- created.
    status :: Prelude.Maybe TaskTemplateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskTemplateMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'taskTemplateMetadata_arn' - The Amazon Resource Name (ARN) of the task template.
--
-- 'createdTime', 'taskTemplateMetadata_createdTime' - The timestamp when the task template was created.
--
-- 'description', 'taskTemplateMetadata_description' - The description of the task template.
--
-- 'id', 'taskTemplateMetadata_id' - A unique identifier for the task template.
--
-- 'lastModifiedTime', 'taskTemplateMetadata_lastModifiedTime' - The timestamp when the task template was last modified.
--
-- 'name', 'taskTemplateMetadata_name' - The name of the task template.
--
-- 'status', 'taskTemplateMetadata_status' - Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
newTaskTemplateMetadata ::
  TaskTemplateMetadata
newTaskTemplateMetadata =
  TaskTemplateMetadata'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the task template.
taskTemplateMetadata_arn :: Lens.Lens' TaskTemplateMetadata (Prelude.Maybe Prelude.Text)
taskTemplateMetadata_arn = Lens.lens (\TaskTemplateMetadata' {arn} -> arn) (\s@TaskTemplateMetadata' {} a -> s {arn = a} :: TaskTemplateMetadata)

-- | The timestamp when the task template was created.
taskTemplateMetadata_createdTime :: Lens.Lens' TaskTemplateMetadata (Prelude.Maybe Prelude.UTCTime)
taskTemplateMetadata_createdTime = Lens.lens (\TaskTemplateMetadata' {createdTime} -> createdTime) (\s@TaskTemplateMetadata' {} a -> s {createdTime = a} :: TaskTemplateMetadata) Prelude.. Lens.mapping Data._Time

-- | The description of the task template.
taskTemplateMetadata_description :: Lens.Lens' TaskTemplateMetadata (Prelude.Maybe Prelude.Text)
taskTemplateMetadata_description = Lens.lens (\TaskTemplateMetadata' {description} -> description) (\s@TaskTemplateMetadata' {} a -> s {description = a} :: TaskTemplateMetadata)

-- | A unique identifier for the task template.
taskTemplateMetadata_id :: Lens.Lens' TaskTemplateMetadata (Prelude.Maybe Prelude.Text)
taskTemplateMetadata_id = Lens.lens (\TaskTemplateMetadata' {id} -> id) (\s@TaskTemplateMetadata' {} a -> s {id = a} :: TaskTemplateMetadata)

-- | The timestamp when the task template was last modified.
taskTemplateMetadata_lastModifiedTime :: Lens.Lens' TaskTemplateMetadata (Prelude.Maybe Prelude.UTCTime)
taskTemplateMetadata_lastModifiedTime = Lens.lens (\TaskTemplateMetadata' {lastModifiedTime} -> lastModifiedTime) (\s@TaskTemplateMetadata' {} a -> s {lastModifiedTime = a} :: TaskTemplateMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the task template.
taskTemplateMetadata_name :: Lens.Lens' TaskTemplateMetadata (Prelude.Maybe Prelude.Text)
taskTemplateMetadata_name = Lens.lens (\TaskTemplateMetadata' {name} -> name) (\s@TaskTemplateMetadata' {} a -> s {name = a} :: TaskTemplateMetadata)

-- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
taskTemplateMetadata_status :: Lens.Lens' TaskTemplateMetadata (Prelude.Maybe TaskTemplateStatus)
taskTemplateMetadata_status = Lens.lens (\TaskTemplateMetadata' {status} -> status) (\s@TaskTemplateMetadata' {} a -> s {status = a} :: TaskTemplateMetadata)

instance Data.FromJSON TaskTemplateMetadata where
  parseJSON =
    Data.withObject
      "TaskTemplateMetadata"
      ( \x ->
          TaskTemplateMetadata'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable TaskTemplateMetadata where
  hashWithSalt _salt TaskTemplateMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData TaskTemplateMetadata where
  rnf TaskTemplateMetadata' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
