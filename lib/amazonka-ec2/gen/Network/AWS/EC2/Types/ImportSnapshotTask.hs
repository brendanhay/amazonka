{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportSnapshotTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportSnapshotTask
  ( ImportSnapshotTask (..),

    -- * Smart constructor
    mkImportSnapshotTask,

    -- * Lenses
    istSnapshotTaskDetail,
    istImportTaskId,
    istDescription,
    istTags,
  )
where

import Network.AWS.EC2.Types.SnapshotTaskDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an import snapshot task.
--
-- /See:/ 'mkImportSnapshotTask' smart constructor.
data ImportSnapshotTask = ImportSnapshotTask'
  { snapshotTaskDetail ::
      Lude.Maybe SnapshotTaskDetail,
    importTaskId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportSnapshotTask' with the minimum fields required to make a request.
--
-- * 'description' - A description of the import snapshot task.
-- * 'importTaskId' - The ID of the import snapshot task.
-- * 'snapshotTaskDetail' - Describes an import snapshot task.
-- * 'tags' - The tags for the import snapshot task.
mkImportSnapshotTask ::
  ImportSnapshotTask
mkImportSnapshotTask =
  ImportSnapshotTask'
    { snapshotTaskDetail = Lude.Nothing,
      importTaskId = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Describes an import snapshot task.
--
-- /Note:/ Consider using 'snapshotTaskDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
istSnapshotTaskDetail :: Lens.Lens' ImportSnapshotTask (Lude.Maybe SnapshotTaskDetail)
istSnapshotTaskDetail = Lens.lens (snapshotTaskDetail :: ImportSnapshotTask -> Lude.Maybe SnapshotTaskDetail) (\s a -> s {snapshotTaskDetail = a} :: ImportSnapshotTask)
{-# DEPRECATED istSnapshotTaskDetail "Use generic-lens or generic-optics with 'snapshotTaskDetail' instead." #-}

-- | The ID of the import snapshot task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
istImportTaskId :: Lens.Lens' ImportSnapshotTask (Lude.Maybe Lude.Text)
istImportTaskId = Lens.lens (importTaskId :: ImportSnapshotTask -> Lude.Maybe Lude.Text) (\s a -> s {importTaskId = a} :: ImportSnapshotTask)
{-# DEPRECATED istImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | A description of the import snapshot task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
istDescription :: Lens.Lens' ImportSnapshotTask (Lude.Maybe Lude.Text)
istDescription = Lens.lens (description :: ImportSnapshotTask -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportSnapshotTask)
{-# DEPRECATED istDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags for the import snapshot task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
istTags :: Lens.Lens' ImportSnapshotTask (Lude.Maybe [Tag])
istTags = Lens.lens (tags :: ImportSnapshotTask -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ImportSnapshotTask)
{-# DEPRECATED istTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ImportSnapshotTask where
  parseXML x =
    ImportSnapshotTask'
      Lude.<$> (x Lude..@? "snapshotTaskDetail")
      Lude.<*> (x Lude..@? "importTaskId")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
