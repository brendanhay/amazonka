{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ExportSnapshotRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ExportSnapshotRecord
  ( ExportSnapshotRecord (..),

    -- * Smart constructor
    mkExportSnapshotRecord,

    -- * Lenses
    esrArn,
    esrCreatedAt,
    esrDestinationInfo,
    esrLocation,
    esrName,
    esrResourceType,
    esrSourceInfo,
    esrState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.DestinationInfo as Types
import qualified Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.RecordState as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an export snapshot record.
--
-- /See:/ 'mkExportSnapshotRecord' smart constructor.
data ExportSnapshotRecord = ExportSnapshotRecord'
  { -- | The Amazon Resource Name (ARN) of the export snapshot record.
    arn :: Core.Maybe Types.NonEmptyString,
    -- | The date when the export snapshot record was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | A list of objects describing the destination of the export snapshot record.
    destinationInfo :: Core.Maybe Types.DestinationInfo,
    -- | The AWS Region and Availability Zone where the export snapshot record is located.
    location :: Core.Maybe Types.ResourceLocation,
    -- | The export snapshot record name.
    name :: Core.Maybe Types.ResourceName,
    -- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
    resourceType :: Core.Maybe Types.ResourceType,
    -- | A list of objects describing the source of the export snapshot record.
    sourceInfo :: Core.Maybe Types.ExportSnapshotRecordSourceInfo,
    -- | The state of the export snapshot record.
    state :: Core.Maybe Types.RecordState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ExportSnapshotRecord' value with any optional fields omitted.
mkExportSnapshotRecord ::
  ExportSnapshotRecord
mkExportSnapshotRecord =
  ExportSnapshotRecord'
    { arn = Core.Nothing,
      createdAt = Core.Nothing,
      destinationInfo = Core.Nothing,
      location = Core.Nothing,
      name = Core.Nothing,
      resourceType = Core.Nothing,
      sourceInfo = Core.Nothing,
      state = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the export snapshot record.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrArn :: Lens.Lens' ExportSnapshotRecord (Core.Maybe Types.NonEmptyString)
esrArn = Lens.field @"arn"
{-# DEPRECATED esrArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the export snapshot record was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrCreatedAt :: Lens.Lens' ExportSnapshotRecord (Core.Maybe Core.NominalDiffTime)
esrCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED esrCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | A list of objects describing the destination of the export snapshot record.
--
-- /Note:/ Consider using 'destinationInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrDestinationInfo :: Lens.Lens' ExportSnapshotRecord (Core.Maybe Types.DestinationInfo)
esrDestinationInfo = Lens.field @"destinationInfo"
{-# DEPRECATED esrDestinationInfo "Use generic-lens or generic-optics with 'destinationInfo' instead." #-}

-- | The AWS Region and Availability Zone where the export snapshot record is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrLocation :: Lens.Lens' ExportSnapshotRecord (Core.Maybe Types.ResourceLocation)
esrLocation = Lens.field @"location"
{-# DEPRECATED esrLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The export snapshot record name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrName :: Lens.Lens' ExportSnapshotRecord (Core.Maybe Types.ResourceName)
esrName = Lens.field @"name"
{-# DEPRECATED esrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrResourceType :: Lens.Lens' ExportSnapshotRecord (Core.Maybe Types.ResourceType)
esrResourceType = Lens.field @"resourceType"
{-# DEPRECATED esrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A list of objects describing the source of the export snapshot record.
--
-- /Note:/ Consider using 'sourceInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrSourceInfo :: Lens.Lens' ExportSnapshotRecord (Core.Maybe Types.ExportSnapshotRecordSourceInfo)
esrSourceInfo = Lens.field @"sourceInfo"
{-# DEPRECATED esrSourceInfo "Use generic-lens or generic-optics with 'sourceInfo' instead." #-}

-- | The state of the export snapshot record.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrState :: Lens.Lens' ExportSnapshotRecord (Core.Maybe Types.RecordState)
esrState = Lens.field @"state"
{-# DEPRECATED esrState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON ExportSnapshotRecord where
  parseJSON =
    Core.withObject "ExportSnapshotRecord" Core.$
      \x ->
        ExportSnapshotRecord'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "destinationInfo")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "resourceType")
          Core.<*> (x Core..:? "sourceInfo")
          Core.<*> (x Core..:? "state")
