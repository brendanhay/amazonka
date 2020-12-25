{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
  ( ExportSnapshotRecordSourceInfo (..),

    -- * Smart constructor
    mkExportSnapshotRecordSourceInfo,

    -- * Lenses
    esrsiArn,
    esrsiCreatedAt,
    esrsiDiskSnapshotInfo,
    esrsiFromResourceArn,
    esrsiFromResourceName,
    esrsiInstanceSnapshotInfo,
    esrsiName,
    esrsiResourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.DiskSnapshotInfo as Types
import qualified Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType as Types
import qualified Network.AWS.Lightsail.Types.InstanceSnapshotInfo as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the source of an export snapshot record.
--
-- /See:/ 'mkExportSnapshotRecordSourceInfo' smart constructor.
data ExportSnapshotRecordSourceInfo = ExportSnapshotRecordSourceInfo'
  { -- | The Amazon Resource Name (ARN) of the source instance or disk snapshot.
    arn :: Core.Maybe Types.NonEmptyString,
    -- | The date when the source instance or disk snapshot was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | A list of objects describing a disk snapshot.
    diskSnapshotInfo :: Core.Maybe Types.DiskSnapshotInfo,
    -- | The Amazon Resource Name (ARN) of the snapshot's source instance or disk.
    fromResourceArn :: Core.Maybe Types.NonEmptyString,
    -- | The name of the snapshot's source instance or disk.
    fromResourceName :: Core.Maybe Types.NonEmptyString,
    -- | A list of objects describing an instance snapshot.
    instanceSnapshotInfo :: Core.Maybe Types.InstanceSnapshotInfo,
    -- | The name of the source instance or disk snapshot.
    name :: Core.Maybe Types.NonEmptyString,
    -- | The Lightsail resource type (e.g., @InstanceSnapshot@ or @DiskSnapshot@ ).
    resourceType :: Core.Maybe Types.ExportSnapshotRecordSourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ExportSnapshotRecordSourceInfo' value with any optional fields omitted.
mkExportSnapshotRecordSourceInfo ::
  ExportSnapshotRecordSourceInfo
mkExportSnapshotRecordSourceInfo =
  ExportSnapshotRecordSourceInfo'
    { arn = Core.Nothing,
      createdAt = Core.Nothing,
      diskSnapshotInfo = Core.Nothing,
      fromResourceArn = Core.Nothing,
      fromResourceName = Core.Nothing,
      instanceSnapshotInfo = Core.Nothing,
      name = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the source instance or disk snapshot.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsiArn :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Types.NonEmptyString)
esrsiArn = Lens.field @"arn"
{-# DEPRECATED esrsiArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the source instance or disk snapshot was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsiCreatedAt :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Core.NominalDiffTime)
esrsiCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED esrsiCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | A list of objects describing a disk snapshot.
--
-- /Note:/ Consider using 'diskSnapshotInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsiDiskSnapshotInfo :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Types.DiskSnapshotInfo)
esrsiDiskSnapshotInfo = Lens.field @"diskSnapshotInfo"
{-# DEPRECATED esrsiDiskSnapshotInfo "Use generic-lens or generic-optics with 'diskSnapshotInfo' instead." #-}

-- | The Amazon Resource Name (ARN) of the snapshot's source instance or disk.
--
-- /Note:/ Consider using 'fromResourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsiFromResourceArn :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Types.NonEmptyString)
esrsiFromResourceArn = Lens.field @"fromResourceArn"
{-# DEPRECATED esrsiFromResourceArn "Use generic-lens or generic-optics with 'fromResourceArn' instead." #-}

-- | The name of the snapshot's source instance or disk.
--
-- /Note:/ Consider using 'fromResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsiFromResourceName :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Types.NonEmptyString)
esrsiFromResourceName = Lens.field @"fromResourceName"
{-# DEPRECATED esrsiFromResourceName "Use generic-lens or generic-optics with 'fromResourceName' instead." #-}

-- | A list of objects describing an instance snapshot.
--
-- /Note:/ Consider using 'instanceSnapshotInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsiInstanceSnapshotInfo :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Types.InstanceSnapshotInfo)
esrsiInstanceSnapshotInfo = Lens.field @"instanceSnapshotInfo"
{-# DEPRECATED esrsiInstanceSnapshotInfo "Use generic-lens or generic-optics with 'instanceSnapshotInfo' instead." #-}

-- | The name of the source instance or disk snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsiName :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Types.NonEmptyString)
esrsiName = Lens.field @"name"
{-# DEPRECATED esrsiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Lightsail resource type (e.g., @InstanceSnapshot@ or @DiskSnapshot@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsiResourceType :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Types.ExportSnapshotRecordSourceType)
esrsiResourceType = Lens.field @"resourceType"
{-# DEPRECATED esrsiResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON ExportSnapshotRecordSourceInfo where
  parseJSON =
    Core.withObject "ExportSnapshotRecordSourceInfo" Core.$
      \x ->
        ExportSnapshotRecordSourceInfo'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "diskSnapshotInfo")
          Core.<*> (x Core..:? "fromResourceArn")
          Core.<*> (x Core..:? "fromResourceName")
          Core.<*> (x Core..:? "instanceSnapshotInfo")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "resourceType")
