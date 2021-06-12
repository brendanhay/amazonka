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
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceSnapshot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Lightsail.Types.InstanceSnapshotState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag

-- | Describes an instance snapshot.
--
-- /See:/ 'newInstanceSnapshot' smart constructor.
data InstanceSnapshot = InstanceSnapshot'
  { -- | A Boolean value indicating whether the snapshot was created from an
    -- automatic snapshot.
    isFromAutoSnapshot :: Core.Maybe Core.Bool,
    -- | The timestamp when the snapshot was created (e.g., @1479907467.024@).
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the snapshot (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot\/d23b5706-3322-4d83-81e5-12345EXAMPLE@).
    arn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the instance from which the snapshot
    -- was created (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:Instance\/64b8404c-ccb1-430b-8daf-12345EXAMPLE@).
    fromInstanceArn :: Core.Maybe Core.Text,
    -- | The type of resource (usually @InstanceSnapshot@).
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The size in GB of the SSD.
    sizeInGb :: Core.Maybe Core.Int,
    -- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@).
    fromBundleId :: Core.Maybe Core.Text,
    -- | The state the snapshot is in.
    state :: Core.Maybe InstanceSnapshotState,
    -- | The name of the snapshot.
    name :: Core.Maybe Core.Text,
    -- | The blueprint ID from which you created the snapshot (e.g.,
    -- @os_debian_8_3@). A blueprint is a virtual private server (or
    -- /instance/) image used to create instances quickly.
    fromBlueprintId :: Core.Maybe Core.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Core.Maybe [Tag],
    -- | The instance from which the snapshot was created.
    fromInstanceName :: Core.Maybe Core.Text,
    -- | The region name and Availability Zone where you created the snapshot.
    location :: Core.Maybe ResourceLocation,
    -- | The progress of the snapshot.
    --
    -- This is populated only for disk snapshots, and is @null@ for instance
    -- snapshots.
    progress :: Core.Maybe Core.Text,
    -- | An array of disk objects containing information about all block storage
    -- disks.
    fromAttachedDisks :: Core.Maybe [Disk]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isFromAutoSnapshot', 'instanceSnapshot_isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
--
-- 'createdAt', 'instanceSnapshot_createdAt' - The timestamp when the snapshot was created (e.g., @1479907467.024@).
--
-- 'arn', 'instanceSnapshot_arn' - The Amazon Resource Name (ARN) of the snapshot (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot\/d23b5706-3322-4d83-81e5-12345EXAMPLE@).
--
-- 'fromInstanceArn', 'instanceSnapshot_fromInstanceArn' - The Amazon Resource Name (ARN) of the instance from which the snapshot
-- was created (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/64b8404c-ccb1-430b-8daf-12345EXAMPLE@).
--
-- 'resourceType', 'instanceSnapshot_resourceType' - The type of resource (usually @InstanceSnapshot@).
--
-- 'supportCode', 'instanceSnapshot_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'sizeInGb', 'instanceSnapshot_sizeInGb' - The size in GB of the SSD.
--
-- 'fromBundleId', 'instanceSnapshot_fromBundleId' - The bundle ID from which you created the snapshot (e.g., @micro_1_0@).
--
-- 'state', 'instanceSnapshot_state' - The state the snapshot is in.
--
-- 'name', 'instanceSnapshot_name' - The name of the snapshot.
--
-- 'fromBlueprintId', 'instanceSnapshot_fromBlueprintId' - The blueprint ID from which you created the snapshot (e.g.,
-- @os_debian_8_3@). A blueprint is a virtual private server (or
-- /instance/) image used to create instances quickly.
--
-- 'tags', 'instanceSnapshot_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'fromInstanceName', 'instanceSnapshot_fromInstanceName' - The instance from which the snapshot was created.
--
-- 'location', 'instanceSnapshot_location' - The region name and Availability Zone where you created the snapshot.
--
-- 'progress', 'instanceSnapshot_progress' - The progress of the snapshot.
--
-- This is populated only for disk snapshots, and is @null@ for instance
-- snapshots.
--
-- 'fromAttachedDisks', 'instanceSnapshot_fromAttachedDisks' - An array of disk objects containing information about all block storage
-- disks.
newInstanceSnapshot ::
  InstanceSnapshot
newInstanceSnapshot =
  InstanceSnapshot'
    { isFromAutoSnapshot =
        Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      fromInstanceArn = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      sizeInGb = Core.Nothing,
      fromBundleId = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      fromBlueprintId = Core.Nothing,
      tags = Core.Nothing,
      fromInstanceName = Core.Nothing,
      location = Core.Nothing,
      progress = Core.Nothing,
      fromAttachedDisks = Core.Nothing
    }

-- | A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
instanceSnapshot_isFromAutoSnapshot :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Bool)
instanceSnapshot_isFromAutoSnapshot = Lens.lens (\InstanceSnapshot' {isFromAutoSnapshot} -> isFromAutoSnapshot) (\s@InstanceSnapshot' {} a -> s {isFromAutoSnapshot = a} :: InstanceSnapshot)

-- | The timestamp when the snapshot was created (e.g., @1479907467.024@).
instanceSnapshot_createdAt :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.UTCTime)
instanceSnapshot_createdAt = Lens.lens (\InstanceSnapshot' {createdAt} -> createdAt) (\s@InstanceSnapshot' {} a -> s {createdAt = a} :: InstanceSnapshot) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the snapshot (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot\/d23b5706-3322-4d83-81e5-12345EXAMPLE@).
instanceSnapshot_arn :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
instanceSnapshot_arn = Lens.lens (\InstanceSnapshot' {arn} -> arn) (\s@InstanceSnapshot' {} a -> s {arn = a} :: InstanceSnapshot)

-- | The Amazon Resource Name (ARN) of the instance from which the snapshot
-- was created (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/64b8404c-ccb1-430b-8daf-12345EXAMPLE@).
instanceSnapshot_fromInstanceArn :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
instanceSnapshot_fromInstanceArn = Lens.lens (\InstanceSnapshot' {fromInstanceArn} -> fromInstanceArn) (\s@InstanceSnapshot' {} a -> s {fromInstanceArn = a} :: InstanceSnapshot)

-- | The type of resource (usually @InstanceSnapshot@).
instanceSnapshot_resourceType :: Lens.Lens' InstanceSnapshot (Core.Maybe ResourceType)
instanceSnapshot_resourceType = Lens.lens (\InstanceSnapshot' {resourceType} -> resourceType) (\s@InstanceSnapshot' {} a -> s {resourceType = a} :: InstanceSnapshot)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
instanceSnapshot_supportCode :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
instanceSnapshot_supportCode = Lens.lens (\InstanceSnapshot' {supportCode} -> supportCode) (\s@InstanceSnapshot' {} a -> s {supportCode = a} :: InstanceSnapshot)

-- | The size in GB of the SSD.
instanceSnapshot_sizeInGb :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Int)
instanceSnapshot_sizeInGb = Lens.lens (\InstanceSnapshot' {sizeInGb} -> sizeInGb) (\s@InstanceSnapshot' {} a -> s {sizeInGb = a} :: InstanceSnapshot)

-- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@).
instanceSnapshot_fromBundleId :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
instanceSnapshot_fromBundleId = Lens.lens (\InstanceSnapshot' {fromBundleId} -> fromBundleId) (\s@InstanceSnapshot' {} a -> s {fromBundleId = a} :: InstanceSnapshot)

-- | The state the snapshot is in.
instanceSnapshot_state :: Lens.Lens' InstanceSnapshot (Core.Maybe InstanceSnapshotState)
instanceSnapshot_state = Lens.lens (\InstanceSnapshot' {state} -> state) (\s@InstanceSnapshot' {} a -> s {state = a} :: InstanceSnapshot)

-- | The name of the snapshot.
instanceSnapshot_name :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
instanceSnapshot_name = Lens.lens (\InstanceSnapshot' {name} -> name) (\s@InstanceSnapshot' {} a -> s {name = a} :: InstanceSnapshot)

-- | The blueprint ID from which you created the snapshot (e.g.,
-- @os_debian_8_3@). A blueprint is a virtual private server (or
-- /instance/) image used to create instances quickly.
instanceSnapshot_fromBlueprintId :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
instanceSnapshot_fromBlueprintId = Lens.lens (\InstanceSnapshot' {fromBlueprintId} -> fromBlueprintId) (\s@InstanceSnapshot' {} a -> s {fromBlueprintId = a} :: InstanceSnapshot)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
instanceSnapshot_tags :: Lens.Lens' InstanceSnapshot (Core.Maybe [Tag])
instanceSnapshot_tags = Lens.lens (\InstanceSnapshot' {tags} -> tags) (\s@InstanceSnapshot' {} a -> s {tags = a} :: InstanceSnapshot) Core.. Lens.mapping Lens._Coerce

-- | The instance from which the snapshot was created.
instanceSnapshot_fromInstanceName :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
instanceSnapshot_fromInstanceName = Lens.lens (\InstanceSnapshot' {fromInstanceName} -> fromInstanceName) (\s@InstanceSnapshot' {} a -> s {fromInstanceName = a} :: InstanceSnapshot)

-- | The region name and Availability Zone where you created the snapshot.
instanceSnapshot_location :: Lens.Lens' InstanceSnapshot (Core.Maybe ResourceLocation)
instanceSnapshot_location = Lens.lens (\InstanceSnapshot' {location} -> location) (\s@InstanceSnapshot' {} a -> s {location = a} :: InstanceSnapshot)

-- | The progress of the snapshot.
--
-- This is populated only for disk snapshots, and is @null@ for instance
-- snapshots.
instanceSnapshot_progress :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
instanceSnapshot_progress = Lens.lens (\InstanceSnapshot' {progress} -> progress) (\s@InstanceSnapshot' {} a -> s {progress = a} :: InstanceSnapshot)

-- | An array of disk objects containing information about all block storage
-- disks.
instanceSnapshot_fromAttachedDisks :: Lens.Lens' InstanceSnapshot (Core.Maybe [Disk])
instanceSnapshot_fromAttachedDisks = Lens.lens (\InstanceSnapshot' {fromAttachedDisks} -> fromAttachedDisks) (\s@InstanceSnapshot' {} a -> s {fromAttachedDisks = a} :: InstanceSnapshot) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON InstanceSnapshot where
  parseJSON =
    Core.withObject
      "InstanceSnapshot"
      ( \x ->
          InstanceSnapshot'
            Core.<$> (x Core..:? "isFromAutoSnapshot")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "fromInstanceArn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "sizeInGb")
            Core.<*> (x Core..:? "fromBundleId")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "fromBlueprintId")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "fromInstanceName")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "progress")
            Core.<*> ( x Core..:? "fromAttachedDisks"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable InstanceSnapshot

instance Core.NFData InstanceSnapshot
