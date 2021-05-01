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
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceSnapshot where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Lightsail.Types.InstanceSnapshotState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance snapshot.
--
-- /See:/ 'newInstanceSnapshot' smart constructor.
data InstanceSnapshot = InstanceSnapshot'
  { -- | A Boolean value indicating whether the snapshot was created from an
    -- automatic snapshot.
    isFromAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the snapshot was created (e.g., @1479907467.024@).
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the snapshot (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot\/d23b5706-3322-4d83-81e5-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the instance from which the snapshot
    -- was created (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:Instance\/64b8404c-ccb1-430b-8daf-12345EXAMPLE@).
    fromInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of resource (usually @InstanceSnapshot@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The size in GB of the SSD.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@).
    fromBundleId :: Prelude.Maybe Prelude.Text,
    -- | The state the snapshot is in.
    state :: Prelude.Maybe InstanceSnapshotState,
    -- | The name of the snapshot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The blueprint ID from which you created the snapshot (e.g.,
    -- @os_debian_8_3@). A blueprint is a virtual private server (or
    -- /instance/) image used to create instances quickly.
    fromBlueprintId :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The instance from which the snapshot was created.
    fromInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The region name and Availability Zone where you created the snapshot.
    location :: Prelude.Maybe ResourceLocation,
    -- | The progress of the snapshot.
    --
    -- This is populated only for disk snapshots, and is @null@ for instance
    -- snapshots.
    progress :: Prelude.Maybe Prelude.Text,
    -- | An array of disk objects containing information about all block storage
    -- disks.
    fromAttachedDisks :: Prelude.Maybe [Disk]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      fromInstanceArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      fromBundleId = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      fromBlueprintId = Prelude.Nothing,
      tags = Prelude.Nothing,
      fromInstanceName = Prelude.Nothing,
      location = Prelude.Nothing,
      progress = Prelude.Nothing,
      fromAttachedDisks = Prelude.Nothing
    }

-- | A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
instanceSnapshot_isFromAutoSnapshot :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Bool)
instanceSnapshot_isFromAutoSnapshot = Lens.lens (\InstanceSnapshot' {isFromAutoSnapshot} -> isFromAutoSnapshot) (\s@InstanceSnapshot' {} a -> s {isFromAutoSnapshot = a} :: InstanceSnapshot)

-- | The timestamp when the snapshot was created (e.g., @1479907467.024@).
instanceSnapshot_createdAt :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.UTCTime)
instanceSnapshot_createdAt = Lens.lens (\InstanceSnapshot' {createdAt} -> createdAt) (\s@InstanceSnapshot' {} a -> s {createdAt = a} :: InstanceSnapshot) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the snapshot (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot\/d23b5706-3322-4d83-81e5-12345EXAMPLE@).
instanceSnapshot_arn :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_arn = Lens.lens (\InstanceSnapshot' {arn} -> arn) (\s@InstanceSnapshot' {} a -> s {arn = a} :: InstanceSnapshot)

-- | The Amazon Resource Name (ARN) of the instance from which the snapshot
-- was created (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/64b8404c-ccb1-430b-8daf-12345EXAMPLE@).
instanceSnapshot_fromInstanceArn :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_fromInstanceArn = Lens.lens (\InstanceSnapshot' {fromInstanceArn} -> fromInstanceArn) (\s@InstanceSnapshot' {} a -> s {fromInstanceArn = a} :: InstanceSnapshot)

-- | The type of resource (usually @InstanceSnapshot@).
instanceSnapshot_resourceType :: Lens.Lens' InstanceSnapshot (Prelude.Maybe ResourceType)
instanceSnapshot_resourceType = Lens.lens (\InstanceSnapshot' {resourceType} -> resourceType) (\s@InstanceSnapshot' {} a -> s {resourceType = a} :: InstanceSnapshot)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
instanceSnapshot_supportCode :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_supportCode = Lens.lens (\InstanceSnapshot' {supportCode} -> supportCode) (\s@InstanceSnapshot' {} a -> s {supportCode = a} :: InstanceSnapshot)

-- | The size in GB of the SSD.
instanceSnapshot_sizeInGb :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Int)
instanceSnapshot_sizeInGb = Lens.lens (\InstanceSnapshot' {sizeInGb} -> sizeInGb) (\s@InstanceSnapshot' {} a -> s {sizeInGb = a} :: InstanceSnapshot)

-- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@).
instanceSnapshot_fromBundleId :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_fromBundleId = Lens.lens (\InstanceSnapshot' {fromBundleId} -> fromBundleId) (\s@InstanceSnapshot' {} a -> s {fromBundleId = a} :: InstanceSnapshot)

-- | The state the snapshot is in.
instanceSnapshot_state :: Lens.Lens' InstanceSnapshot (Prelude.Maybe InstanceSnapshotState)
instanceSnapshot_state = Lens.lens (\InstanceSnapshot' {state} -> state) (\s@InstanceSnapshot' {} a -> s {state = a} :: InstanceSnapshot)

-- | The name of the snapshot.
instanceSnapshot_name :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_name = Lens.lens (\InstanceSnapshot' {name} -> name) (\s@InstanceSnapshot' {} a -> s {name = a} :: InstanceSnapshot)

-- | The blueprint ID from which you created the snapshot (e.g.,
-- @os_debian_8_3@). A blueprint is a virtual private server (or
-- /instance/) image used to create instances quickly.
instanceSnapshot_fromBlueprintId :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_fromBlueprintId = Lens.lens (\InstanceSnapshot' {fromBlueprintId} -> fromBlueprintId) (\s@InstanceSnapshot' {} a -> s {fromBlueprintId = a} :: InstanceSnapshot)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
instanceSnapshot_tags :: Lens.Lens' InstanceSnapshot (Prelude.Maybe [Tag])
instanceSnapshot_tags = Lens.lens (\InstanceSnapshot' {tags} -> tags) (\s@InstanceSnapshot' {} a -> s {tags = a} :: InstanceSnapshot) Prelude.. Lens.mapping Prelude._Coerce

-- | The instance from which the snapshot was created.
instanceSnapshot_fromInstanceName :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_fromInstanceName = Lens.lens (\InstanceSnapshot' {fromInstanceName} -> fromInstanceName) (\s@InstanceSnapshot' {} a -> s {fromInstanceName = a} :: InstanceSnapshot)

-- | The region name and Availability Zone where you created the snapshot.
instanceSnapshot_location :: Lens.Lens' InstanceSnapshot (Prelude.Maybe ResourceLocation)
instanceSnapshot_location = Lens.lens (\InstanceSnapshot' {location} -> location) (\s@InstanceSnapshot' {} a -> s {location = a} :: InstanceSnapshot)

-- | The progress of the snapshot.
--
-- This is populated only for disk snapshots, and is @null@ for instance
-- snapshots.
instanceSnapshot_progress :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_progress = Lens.lens (\InstanceSnapshot' {progress} -> progress) (\s@InstanceSnapshot' {} a -> s {progress = a} :: InstanceSnapshot)

-- | An array of disk objects containing information about all block storage
-- disks.
instanceSnapshot_fromAttachedDisks :: Lens.Lens' InstanceSnapshot (Prelude.Maybe [Disk])
instanceSnapshot_fromAttachedDisks = Lens.lens (\InstanceSnapshot' {fromAttachedDisks} -> fromAttachedDisks) (\s@InstanceSnapshot' {} a -> s {fromAttachedDisks = a} :: InstanceSnapshot) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON InstanceSnapshot where
  parseJSON =
    Prelude.withObject
      "InstanceSnapshot"
      ( \x ->
          InstanceSnapshot'
            Prelude.<$> (x Prelude..:? "isFromAutoSnapshot")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "fromInstanceArn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> (x Prelude..:? "sizeInGb")
            Prelude.<*> (x Prelude..:? "fromBundleId")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "fromBlueprintId")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "fromInstanceName")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "progress")
            Prelude.<*> ( x Prelude..:? "fromAttachedDisks"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InstanceSnapshot

instance Prelude.NFData InstanceSnapshot
