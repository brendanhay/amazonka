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
    esrState,
    esrDestinationInfo,
    esrResourceType,
    esrArn,
    esrCreatedAt,
    esrLocation,
    esrName,
    esrSourceInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes an export snapshot record.
--
-- /See:/ 'mkExportSnapshotRecord' smart constructor.
data ExportSnapshotRecord = ExportSnapshotRecord'
  { state ::
      Lude.Maybe RecordState,
    destinationInfo :: Lude.Maybe DestinationInfo,
    resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    name :: Lude.Maybe Lude.Text,
    sourceInfo ::
      Lude.Maybe ExportSnapshotRecordSourceInfo
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportSnapshotRecord' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the export snapshot record.
-- * 'createdAt' - The date when the export snapshot record was created.
-- * 'destinationInfo' - A list of objects describing the destination of the export snapshot record.
-- * 'location' - The AWS Region and Availability Zone where the export snapshot record is located.
-- * 'name' - The export snapshot record name.
-- * 'resourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
-- * 'sourceInfo' - A list of objects describing the source of the export snapshot record.
-- * 'state' - The state of the export snapshot record.
mkExportSnapshotRecord ::
  ExportSnapshotRecord
mkExportSnapshotRecord =
  ExportSnapshotRecord'
    { state = Lude.Nothing,
      destinationInfo = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      name = Lude.Nothing,
      sourceInfo = Lude.Nothing
    }

-- | The state of the export snapshot record.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrState :: Lens.Lens' ExportSnapshotRecord (Lude.Maybe RecordState)
esrState = Lens.lens (state :: ExportSnapshotRecord -> Lude.Maybe RecordState) (\s a -> s {state = a} :: ExportSnapshotRecord)
{-# DEPRECATED esrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A list of objects describing the destination of the export snapshot record.
--
-- /Note:/ Consider using 'destinationInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrDestinationInfo :: Lens.Lens' ExportSnapshotRecord (Lude.Maybe DestinationInfo)
esrDestinationInfo = Lens.lens (destinationInfo :: ExportSnapshotRecord -> Lude.Maybe DestinationInfo) (\s a -> s {destinationInfo = a} :: ExportSnapshotRecord)
{-# DEPRECATED esrDestinationInfo "Use generic-lens or generic-optics with 'destinationInfo' instead." #-}

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrResourceType :: Lens.Lens' ExportSnapshotRecord (Lude.Maybe ResourceType)
esrResourceType = Lens.lens (resourceType :: ExportSnapshotRecord -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ExportSnapshotRecord)
{-# DEPRECATED esrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the export snapshot record.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrArn :: Lens.Lens' ExportSnapshotRecord (Lude.Maybe Lude.Text)
esrArn = Lens.lens (arn :: ExportSnapshotRecord -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ExportSnapshotRecord)
{-# DEPRECATED esrArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the export snapshot record was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrCreatedAt :: Lens.Lens' ExportSnapshotRecord (Lude.Maybe Lude.Timestamp)
esrCreatedAt = Lens.lens (createdAt :: ExportSnapshotRecord -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ExportSnapshotRecord)
{-# DEPRECATED esrCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS Region and Availability Zone where the export snapshot record is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrLocation :: Lens.Lens' ExportSnapshotRecord (Lude.Maybe ResourceLocation)
esrLocation = Lens.lens (location :: ExportSnapshotRecord -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: ExportSnapshotRecord)
{-# DEPRECATED esrLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The export snapshot record name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrName :: Lens.Lens' ExportSnapshotRecord (Lude.Maybe Lude.Text)
esrName = Lens.lens (name :: ExportSnapshotRecord -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ExportSnapshotRecord)
{-# DEPRECATED esrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of objects describing the source of the export snapshot record.
--
-- /Note:/ Consider using 'sourceInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrSourceInfo :: Lens.Lens' ExportSnapshotRecord (Lude.Maybe ExportSnapshotRecordSourceInfo)
esrSourceInfo = Lens.lens (sourceInfo :: ExportSnapshotRecord -> Lude.Maybe ExportSnapshotRecordSourceInfo) (\s a -> s {sourceInfo = a} :: ExportSnapshotRecord)
{-# DEPRECATED esrSourceInfo "Use generic-lens or generic-optics with 'sourceInfo' instead." #-}

instance Lude.FromJSON ExportSnapshotRecord where
  parseJSON =
    Lude.withObject
      "ExportSnapshotRecord"
      ( \x ->
          ExportSnapshotRecord'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "destinationInfo")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "sourceInfo")
      )
