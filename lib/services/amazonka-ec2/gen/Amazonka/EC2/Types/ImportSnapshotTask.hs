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
-- Module      : Amazonka.EC2.Types.ImportSnapshotTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImportSnapshotTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SnapshotTaskDetail
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an import snapshot task.
--
-- /See:/ 'newImportSnapshotTask' smart constructor.
data ImportSnapshotTask = ImportSnapshotTask'
  { -- | The tags for the import snapshot task.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the import snapshot task.
    importTaskId :: Prelude.Maybe Prelude.Text,
    -- | A description of the import snapshot task.
    description :: Prelude.Maybe Prelude.Text,
    -- | Describes an import snapshot task.
    snapshotTaskDetail :: Prelude.Maybe SnapshotTaskDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSnapshotTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'importSnapshotTask_tags' - The tags for the import snapshot task.
--
-- 'importTaskId', 'importSnapshotTask_importTaskId' - The ID of the import snapshot task.
--
-- 'description', 'importSnapshotTask_description' - A description of the import snapshot task.
--
-- 'snapshotTaskDetail', 'importSnapshotTask_snapshotTaskDetail' - Describes an import snapshot task.
newImportSnapshotTask ::
  ImportSnapshotTask
newImportSnapshotTask =
  ImportSnapshotTask'
    { tags = Prelude.Nothing,
      importTaskId = Prelude.Nothing,
      description = Prelude.Nothing,
      snapshotTaskDetail = Prelude.Nothing
    }

-- | The tags for the import snapshot task.
importSnapshotTask_tags :: Lens.Lens' ImportSnapshotTask (Prelude.Maybe [Tag])
importSnapshotTask_tags = Lens.lens (\ImportSnapshotTask' {tags} -> tags) (\s@ImportSnapshotTask' {} a -> s {tags = a} :: ImportSnapshotTask) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the import snapshot task.
importSnapshotTask_importTaskId :: Lens.Lens' ImportSnapshotTask (Prelude.Maybe Prelude.Text)
importSnapshotTask_importTaskId = Lens.lens (\ImportSnapshotTask' {importTaskId} -> importTaskId) (\s@ImportSnapshotTask' {} a -> s {importTaskId = a} :: ImportSnapshotTask)

-- | A description of the import snapshot task.
importSnapshotTask_description :: Lens.Lens' ImportSnapshotTask (Prelude.Maybe Prelude.Text)
importSnapshotTask_description = Lens.lens (\ImportSnapshotTask' {description} -> description) (\s@ImportSnapshotTask' {} a -> s {description = a} :: ImportSnapshotTask)

-- | Describes an import snapshot task.
importSnapshotTask_snapshotTaskDetail :: Lens.Lens' ImportSnapshotTask (Prelude.Maybe SnapshotTaskDetail)
importSnapshotTask_snapshotTaskDetail = Lens.lens (\ImportSnapshotTask' {snapshotTaskDetail} -> snapshotTaskDetail) (\s@ImportSnapshotTask' {} a -> s {snapshotTaskDetail = a} :: ImportSnapshotTask)

instance Core.FromXML ImportSnapshotTask where
  parseXML x =
    ImportSnapshotTask'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "importTaskId")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "snapshotTaskDetail")

instance Prelude.Hashable ImportSnapshotTask where
  hashWithSalt _salt ImportSnapshotTask' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` importTaskId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` snapshotTaskDetail

instance Prelude.NFData ImportSnapshotTask where
  rnf ImportSnapshotTask' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf importTaskId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf snapshotTaskDetail
