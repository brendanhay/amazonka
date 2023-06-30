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
-- Module      : Amazonka.Athena.Types.NotebookMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.NotebookMetadata where

import Amazonka.Athena.Types.NotebookType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata for notebook, including the notebook name, ID,
-- workgroup, and time created.
--
-- /See:/ 'newNotebookMetadata' smart constructor.
data NotebookMetadata = NotebookMetadata'
  { -- | The time when the notebook was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the notebook was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the notebook.
    name :: Prelude.Maybe Prelude.Text,
    -- | The notebook ID.
    notebookId :: Prelude.Maybe Prelude.Text,
    -- | The type of notebook. Currently, the only valid type is @IPYNB@.
    type' :: Prelude.Maybe NotebookType,
    -- | The name of the Spark enabled workgroup to which the notebook belongs.
    workGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotebookMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'notebookMetadata_creationTime' - The time when the notebook was created.
--
-- 'lastModifiedTime', 'notebookMetadata_lastModifiedTime' - The time when the notebook was last modified.
--
-- 'name', 'notebookMetadata_name' - The name of the notebook.
--
-- 'notebookId', 'notebookMetadata_notebookId' - The notebook ID.
--
-- 'type'', 'notebookMetadata_type' - The type of notebook. Currently, the only valid type is @IPYNB@.
--
-- 'workGroup', 'notebookMetadata_workGroup' - The name of the Spark enabled workgroup to which the notebook belongs.
newNotebookMetadata ::
  NotebookMetadata
newNotebookMetadata =
  NotebookMetadata'
    { creationTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      notebookId = Prelude.Nothing,
      type' = Prelude.Nothing,
      workGroup = Prelude.Nothing
    }

-- | The time when the notebook was created.
notebookMetadata_creationTime :: Lens.Lens' NotebookMetadata (Prelude.Maybe Prelude.UTCTime)
notebookMetadata_creationTime = Lens.lens (\NotebookMetadata' {creationTime} -> creationTime) (\s@NotebookMetadata' {} a -> s {creationTime = a} :: NotebookMetadata) Prelude.. Lens.mapping Data._Time

-- | The time when the notebook was last modified.
notebookMetadata_lastModifiedTime :: Lens.Lens' NotebookMetadata (Prelude.Maybe Prelude.UTCTime)
notebookMetadata_lastModifiedTime = Lens.lens (\NotebookMetadata' {lastModifiedTime} -> lastModifiedTime) (\s@NotebookMetadata' {} a -> s {lastModifiedTime = a} :: NotebookMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the notebook.
notebookMetadata_name :: Lens.Lens' NotebookMetadata (Prelude.Maybe Prelude.Text)
notebookMetadata_name = Lens.lens (\NotebookMetadata' {name} -> name) (\s@NotebookMetadata' {} a -> s {name = a} :: NotebookMetadata)

-- | The notebook ID.
notebookMetadata_notebookId :: Lens.Lens' NotebookMetadata (Prelude.Maybe Prelude.Text)
notebookMetadata_notebookId = Lens.lens (\NotebookMetadata' {notebookId} -> notebookId) (\s@NotebookMetadata' {} a -> s {notebookId = a} :: NotebookMetadata)

-- | The type of notebook. Currently, the only valid type is @IPYNB@.
notebookMetadata_type :: Lens.Lens' NotebookMetadata (Prelude.Maybe NotebookType)
notebookMetadata_type = Lens.lens (\NotebookMetadata' {type'} -> type') (\s@NotebookMetadata' {} a -> s {type' = a} :: NotebookMetadata)

-- | The name of the Spark enabled workgroup to which the notebook belongs.
notebookMetadata_workGroup :: Lens.Lens' NotebookMetadata (Prelude.Maybe Prelude.Text)
notebookMetadata_workGroup = Lens.lens (\NotebookMetadata' {workGroup} -> workGroup) (\s@NotebookMetadata' {} a -> s {workGroup = a} :: NotebookMetadata)

instance Data.FromJSON NotebookMetadata where
  parseJSON =
    Data.withObject
      "NotebookMetadata"
      ( \x ->
          NotebookMetadata'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NotebookId")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "WorkGroup")
      )

instance Prelude.Hashable NotebookMetadata where
  hashWithSalt _salt NotebookMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notebookId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData NotebookMetadata where
  rnf NotebookMetadata' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notebookId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf workGroup
