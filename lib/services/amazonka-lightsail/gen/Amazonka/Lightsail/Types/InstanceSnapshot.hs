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
-- Module      : Amazonka.Lightsail.Types.InstanceSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.Disk
import Amazonka.Lightsail.Types.InstanceSnapshotState
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance snapshot.
--
-- /See:/ 'newInstanceSnapshot' smart constructor.
data InstanceSnapshot = InstanceSnapshot'
  { -- | The Amazon Resource Name (ARN) of the snapshot (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot\/d23b5706-3322-4d83-81e5-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the snapshot was created (e.g., @1479907467.024@).
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | An array of disk objects containing information about all block storage
    -- disks.
    fromAttachedDisks :: Prelude.Maybe [Disk],
    -- | The blueprint ID from which you created the snapshot (e.g.,
    -- @os_debian_8_3@). A blueprint is a virtual private server (or
    -- /instance/) image used to create instances quickly.
    fromBlueprintId :: Prelude.Maybe Prelude.Text,
    -- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@).
    fromBundleId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the instance from which the snapshot
    -- was created (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:Instance\/64b8404c-ccb1-430b-8daf-12345EXAMPLE@).
    fromInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The instance from which the snapshot was created.
    fromInstanceName :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the snapshot was created from an
    -- automatic snapshot.
    isFromAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The region name and Availability Zone where you created the snapshot.
    location :: Prelude.Maybe ResourceLocation,
    -- | The name of the snapshot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The progress of the snapshot.
    --
    -- This is populated only for disk snapshots, and is @null@ for instance
    -- snapshots.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The type of resource (usually @InstanceSnapshot@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The size in GB of the SSD.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The state the snapshot is in.
    state :: Prelude.Maybe InstanceSnapshotState,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'instanceSnapshot_arn' - The Amazon Resource Name (ARN) of the snapshot (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot\/d23b5706-3322-4d83-81e5-12345EXAMPLE@).
--
-- 'createdAt', 'instanceSnapshot_createdAt' - The timestamp when the snapshot was created (e.g., @1479907467.024@).
--
-- 'fromAttachedDisks', 'instanceSnapshot_fromAttachedDisks' - An array of disk objects containing information about all block storage
-- disks.
--
-- 'fromBlueprintId', 'instanceSnapshot_fromBlueprintId' - The blueprint ID from which you created the snapshot (e.g.,
-- @os_debian_8_3@). A blueprint is a virtual private server (or
-- /instance/) image used to create instances quickly.
--
-- 'fromBundleId', 'instanceSnapshot_fromBundleId' - The bundle ID from which you created the snapshot (e.g., @micro_1_0@).
--
-- 'fromInstanceArn', 'instanceSnapshot_fromInstanceArn' - The Amazon Resource Name (ARN) of the instance from which the snapshot
-- was created (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/64b8404c-ccb1-430b-8daf-12345EXAMPLE@).
--
-- 'fromInstanceName', 'instanceSnapshot_fromInstanceName' - The instance from which the snapshot was created.
--
-- 'isFromAutoSnapshot', 'instanceSnapshot_isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
--
-- 'location', 'instanceSnapshot_location' - The region name and Availability Zone where you created the snapshot.
--
-- 'name', 'instanceSnapshot_name' - The name of the snapshot.
--
-- 'progress', 'instanceSnapshot_progress' - The progress of the snapshot.
--
-- This is populated only for disk snapshots, and is @null@ for instance
-- snapshots.
--
-- 'resourceType', 'instanceSnapshot_resourceType' - The type of resource (usually @InstanceSnapshot@).
--
-- 'sizeInGb', 'instanceSnapshot_sizeInGb' - The size in GB of the SSD.
--
-- 'state', 'instanceSnapshot_state' - The state the snapshot is in.
--
-- 'supportCode', 'instanceSnapshot_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'tags', 'instanceSnapshot_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
newInstanceSnapshot ::
  InstanceSnapshot
newInstanceSnapshot =
  InstanceSnapshot'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      fromAttachedDisks = Prelude.Nothing,
      fromBlueprintId = Prelude.Nothing,
      fromBundleId = Prelude.Nothing,
      fromInstanceArn = Prelude.Nothing,
      fromInstanceName = Prelude.Nothing,
      isFromAutoSnapshot = Prelude.Nothing,
      location = Prelude.Nothing,
      name = Prelude.Nothing,
      progress = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      state = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the snapshot (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot\/d23b5706-3322-4d83-81e5-12345EXAMPLE@).
instanceSnapshot_arn :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_arn = Lens.lens (\InstanceSnapshot' {arn} -> arn) (\s@InstanceSnapshot' {} a -> s {arn = a} :: InstanceSnapshot)

-- | The timestamp when the snapshot was created (e.g., @1479907467.024@).
instanceSnapshot_createdAt :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.UTCTime)
instanceSnapshot_createdAt = Lens.lens (\InstanceSnapshot' {createdAt} -> createdAt) (\s@InstanceSnapshot' {} a -> s {createdAt = a} :: InstanceSnapshot) Prelude.. Lens.mapping Data._Time

-- | An array of disk objects containing information about all block storage
-- disks.
instanceSnapshot_fromAttachedDisks :: Lens.Lens' InstanceSnapshot (Prelude.Maybe [Disk])
instanceSnapshot_fromAttachedDisks = Lens.lens (\InstanceSnapshot' {fromAttachedDisks} -> fromAttachedDisks) (\s@InstanceSnapshot' {} a -> s {fromAttachedDisks = a} :: InstanceSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The blueprint ID from which you created the snapshot (e.g.,
-- @os_debian_8_3@). A blueprint is a virtual private server (or
-- /instance/) image used to create instances quickly.
instanceSnapshot_fromBlueprintId :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_fromBlueprintId = Lens.lens (\InstanceSnapshot' {fromBlueprintId} -> fromBlueprintId) (\s@InstanceSnapshot' {} a -> s {fromBlueprintId = a} :: InstanceSnapshot)

-- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@).
instanceSnapshot_fromBundleId :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_fromBundleId = Lens.lens (\InstanceSnapshot' {fromBundleId} -> fromBundleId) (\s@InstanceSnapshot' {} a -> s {fromBundleId = a} :: InstanceSnapshot)

-- | The Amazon Resource Name (ARN) of the instance from which the snapshot
-- was created (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/64b8404c-ccb1-430b-8daf-12345EXAMPLE@).
instanceSnapshot_fromInstanceArn :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_fromInstanceArn = Lens.lens (\InstanceSnapshot' {fromInstanceArn} -> fromInstanceArn) (\s@InstanceSnapshot' {} a -> s {fromInstanceArn = a} :: InstanceSnapshot)

-- | The instance from which the snapshot was created.
instanceSnapshot_fromInstanceName :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_fromInstanceName = Lens.lens (\InstanceSnapshot' {fromInstanceName} -> fromInstanceName) (\s@InstanceSnapshot' {} a -> s {fromInstanceName = a} :: InstanceSnapshot)

-- | A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
instanceSnapshot_isFromAutoSnapshot :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Bool)
instanceSnapshot_isFromAutoSnapshot = Lens.lens (\InstanceSnapshot' {isFromAutoSnapshot} -> isFromAutoSnapshot) (\s@InstanceSnapshot' {} a -> s {isFromAutoSnapshot = a} :: InstanceSnapshot)

-- | The region name and Availability Zone where you created the snapshot.
instanceSnapshot_location :: Lens.Lens' InstanceSnapshot (Prelude.Maybe ResourceLocation)
instanceSnapshot_location = Lens.lens (\InstanceSnapshot' {location} -> location) (\s@InstanceSnapshot' {} a -> s {location = a} :: InstanceSnapshot)

-- | The name of the snapshot.
instanceSnapshot_name :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_name = Lens.lens (\InstanceSnapshot' {name} -> name) (\s@InstanceSnapshot' {} a -> s {name = a} :: InstanceSnapshot)

-- | The progress of the snapshot.
--
-- This is populated only for disk snapshots, and is @null@ for instance
-- snapshots.
instanceSnapshot_progress :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_progress = Lens.lens (\InstanceSnapshot' {progress} -> progress) (\s@InstanceSnapshot' {} a -> s {progress = a} :: InstanceSnapshot)

-- | The type of resource (usually @InstanceSnapshot@).
instanceSnapshot_resourceType :: Lens.Lens' InstanceSnapshot (Prelude.Maybe ResourceType)
instanceSnapshot_resourceType = Lens.lens (\InstanceSnapshot' {resourceType} -> resourceType) (\s@InstanceSnapshot' {} a -> s {resourceType = a} :: InstanceSnapshot)

-- | The size in GB of the SSD.
instanceSnapshot_sizeInGb :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Int)
instanceSnapshot_sizeInGb = Lens.lens (\InstanceSnapshot' {sizeInGb} -> sizeInGb) (\s@InstanceSnapshot' {} a -> s {sizeInGb = a} :: InstanceSnapshot)

-- | The state the snapshot is in.
instanceSnapshot_state :: Lens.Lens' InstanceSnapshot (Prelude.Maybe InstanceSnapshotState)
instanceSnapshot_state = Lens.lens (\InstanceSnapshot' {state} -> state) (\s@InstanceSnapshot' {} a -> s {state = a} :: InstanceSnapshot)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
instanceSnapshot_supportCode :: Lens.Lens' InstanceSnapshot (Prelude.Maybe Prelude.Text)
instanceSnapshot_supportCode = Lens.lens (\InstanceSnapshot' {supportCode} -> supportCode) (\s@InstanceSnapshot' {} a -> s {supportCode = a} :: InstanceSnapshot)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
instanceSnapshot_tags :: Lens.Lens' InstanceSnapshot (Prelude.Maybe [Tag])
instanceSnapshot_tags = Lens.lens (\InstanceSnapshot' {tags} -> tags) (\s@InstanceSnapshot' {} a -> s {tags = a} :: InstanceSnapshot) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InstanceSnapshot where
  parseJSON =
    Data.withObject
      "InstanceSnapshot"
      ( \x ->
          InstanceSnapshot'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> ( x
                            Data..:? "fromAttachedDisks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "fromBlueprintId")
            Prelude.<*> (x Data..:? "fromBundleId")
            Prelude.<*> (x Data..:? "fromInstanceArn")
            Prelude.<*> (x Data..:? "fromInstanceName")
            Prelude.<*> (x Data..:? "isFromAutoSnapshot")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "progress")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "sizeInGb")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable InstanceSnapshot where
  hashWithSalt _salt InstanceSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` fromAttachedDisks
      `Prelude.hashWithSalt` fromBlueprintId
      `Prelude.hashWithSalt` fromBundleId
      `Prelude.hashWithSalt` fromInstanceArn
      `Prelude.hashWithSalt` fromInstanceName
      `Prelude.hashWithSalt` isFromAutoSnapshot
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sizeInGb
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` tags

instance Prelude.NFData InstanceSnapshot where
  rnf InstanceSnapshot' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf fromAttachedDisks
      `Prelude.seq` Prelude.rnf fromBlueprintId
      `Prelude.seq` Prelude.rnf fromBundleId
      `Prelude.seq` Prelude.rnf fromInstanceArn
      `Prelude.seq` Prelude.rnf fromInstanceName
      `Prelude.seq` Prelude.rnf isFromAutoSnapshot
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sizeInGb
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf tags
