{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.ImportSnapshotTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportSnapshotTask where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SnapshotTaskDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an import snapshot task.
--
-- /See:/ 'newImportSnapshotTask' smart constructor.
data ImportSnapshotTask = ImportSnapshotTask'
  { -- | Describes an import snapshot task.
    snapshotTaskDetail :: Prelude.Maybe SnapshotTaskDetail,
    -- | The ID of the import snapshot task.
    importTaskId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the import snapshot task.
    tags :: Prelude.Maybe [Tag],
    -- | A description of the import snapshot task.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportSnapshotTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotTaskDetail', 'importSnapshotTask_snapshotTaskDetail' - Describes an import snapshot task.
--
-- 'importTaskId', 'importSnapshotTask_importTaskId' - The ID of the import snapshot task.
--
-- 'tags', 'importSnapshotTask_tags' - The tags for the import snapshot task.
--
-- 'description', 'importSnapshotTask_description' - A description of the import snapshot task.
newImportSnapshotTask ::
  ImportSnapshotTask
newImportSnapshotTask =
  ImportSnapshotTask'
    { snapshotTaskDetail =
        Prelude.Nothing,
      importTaskId = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Describes an import snapshot task.
importSnapshotTask_snapshotTaskDetail :: Lens.Lens' ImportSnapshotTask (Prelude.Maybe SnapshotTaskDetail)
importSnapshotTask_snapshotTaskDetail = Lens.lens (\ImportSnapshotTask' {snapshotTaskDetail} -> snapshotTaskDetail) (\s@ImportSnapshotTask' {} a -> s {snapshotTaskDetail = a} :: ImportSnapshotTask)

-- | The ID of the import snapshot task.
importSnapshotTask_importTaskId :: Lens.Lens' ImportSnapshotTask (Prelude.Maybe Prelude.Text)
importSnapshotTask_importTaskId = Lens.lens (\ImportSnapshotTask' {importTaskId} -> importTaskId) (\s@ImportSnapshotTask' {} a -> s {importTaskId = a} :: ImportSnapshotTask)

-- | The tags for the import snapshot task.
importSnapshotTask_tags :: Lens.Lens' ImportSnapshotTask (Prelude.Maybe [Tag])
importSnapshotTask_tags = Lens.lens (\ImportSnapshotTask' {tags} -> tags) (\s@ImportSnapshotTask' {} a -> s {tags = a} :: ImportSnapshotTask) Prelude.. Lens.mapping Prelude._Coerce

-- | A description of the import snapshot task.
importSnapshotTask_description :: Lens.Lens' ImportSnapshotTask (Prelude.Maybe Prelude.Text)
importSnapshotTask_description = Lens.lens (\ImportSnapshotTask' {description} -> description) (\s@ImportSnapshotTask' {} a -> s {description = a} :: ImportSnapshotTask)

instance Prelude.FromXML ImportSnapshotTask where
  parseXML x =
    ImportSnapshotTask'
      Prelude.<$> (x Prelude..@? "snapshotTaskDetail")
      Prelude.<*> (x Prelude..@? "importTaskId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "description")

instance Prelude.Hashable ImportSnapshotTask

instance Prelude.NFData ImportSnapshotTask
