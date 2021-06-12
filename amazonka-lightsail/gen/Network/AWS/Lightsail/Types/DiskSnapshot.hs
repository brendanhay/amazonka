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
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DiskSnapshotState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag

-- | Describes a block storage disk snapshot.
--
-- /See:/ 'newDiskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { -- | A Boolean value indicating whether the snapshot was created from an
    -- automatic snapshot.
    isFromAutoSnapshot :: Core.Maybe Core.Bool,
    -- | The unique name of the source disk from which the disk snapshot was
    -- created.
    fromDiskName :: Core.Maybe Core.Text,
    -- | The date when the disk snapshot was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the disk snapshot.
    arn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the source instance from which the
    -- disk (system volume) snapshot was created.
    fromInstanceArn :: Core.Maybe Core.Text,
    -- | The Lightsail resource type (e.g., @DiskSnapshot@).
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The size of the disk in GB.
    sizeInGb :: Core.Maybe Core.Int,
    -- | The status of the disk snapshot operation.
    state :: Core.Maybe DiskSnapshotState,
    -- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
    name :: Core.Maybe Core.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Core.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the source disk from which the disk
    -- snapshot was created.
    fromDiskArn :: Core.Maybe Core.Text,
    -- | The unique name of the source instance from which the disk (system
    -- volume) snapshot was created.
    fromInstanceName :: Core.Maybe Core.Text,
    -- | The AWS Region and Availability Zone where the disk snapshot was
    -- created.
    location :: Core.Maybe ResourceLocation,
    -- | The progress of the snapshot.
    progress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiskSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isFromAutoSnapshot', 'diskSnapshot_isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
--
-- 'fromDiskName', 'diskSnapshot_fromDiskName' - The unique name of the source disk from which the disk snapshot was
-- created.
--
-- 'createdAt', 'diskSnapshot_createdAt' - The date when the disk snapshot was created.
--
-- 'arn', 'diskSnapshot_arn' - The Amazon Resource Name (ARN) of the disk snapshot.
--
-- 'fromInstanceArn', 'diskSnapshot_fromInstanceArn' - The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
--
-- 'resourceType', 'diskSnapshot_resourceType' - The Lightsail resource type (e.g., @DiskSnapshot@).
--
-- 'supportCode', 'diskSnapshot_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'sizeInGb', 'diskSnapshot_sizeInGb' - The size of the disk in GB.
--
-- 'state', 'diskSnapshot_state' - The status of the disk snapshot operation.
--
-- 'name', 'diskSnapshot_name' - The name of the disk snapshot (e.g., @my-disk-snapshot@).
--
-- 'tags', 'diskSnapshot_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'fromDiskArn', 'diskSnapshot_fromDiskArn' - The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
--
-- 'fromInstanceName', 'diskSnapshot_fromInstanceName' - The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
--
-- 'location', 'diskSnapshot_location' - The AWS Region and Availability Zone where the disk snapshot was
-- created.
--
-- 'progress', 'diskSnapshot_progress' - The progress of the snapshot.
newDiskSnapshot ::
  DiskSnapshot
newDiskSnapshot =
  DiskSnapshot'
    { isFromAutoSnapshot = Core.Nothing,
      fromDiskName = Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      fromInstanceArn = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      sizeInGb = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      fromDiskArn = Core.Nothing,
      fromInstanceName = Core.Nothing,
      location = Core.Nothing,
      progress = Core.Nothing
    }

-- | A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
diskSnapshot_isFromAutoSnapshot :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Bool)
diskSnapshot_isFromAutoSnapshot = Lens.lens (\DiskSnapshot' {isFromAutoSnapshot} -> isFromAutoSnapshot) (\s@DiskSnapshot' {} a -> s {isFromAutoSnapshot = a} :: DiskSnapshot)

-- | The unique name of the source disk from which the disk snapshot was
-- created.
diskSnapshot_fromDiskName :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Text)
diskSnapshot_fromDiskName = Lens.lens (\DiskSnapshot' {fromDiskName} -> fromDiskName) (\s@DiskSnapshot' {} a -> s {fromDiskName = a} :: DiskSnapshot)

-- | The date when the disk snapshot was created.
diskSnapshot_createdAt :: Lens.Lens' DiskSnapshot (Core.Maybe Core.UTCTime)
diskSnapshot_createdAt = Lens.lens (\DiskSnapshot' {createdAt} -> createdAt) (\s@DiskSnapshot' {} a -> s {createdAt = a} :: DiskSnapshot) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the disk snapshot.
diskSnapshot_arn :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Text)
diskSnapshot_arn = Lens.lens (\DiskSnapshot' {arn} -> arn) (\s@DiskSnapshot' {} a -> s {arn = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
diskSnapshot_fromInstanceArn :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Text)
diskSnapshot_fromInstanceArn = Lens.lens (\DiskSnapshot' {fromInstanceArn} -> fromInstanceArn) (\s@DiskSnapshot' {} a -> s {fromInstanceArn = a} :: DiskSnapshot)

-- | The Lightsail resource type (e.g., @DiskSnapshot@).
diskSnapshot_resourceType :: Lens.Lens' DiskSnapshot (Core.Maybe ResourceType)
diskSnapshot_resourceType = Lens.lens (\DiskSnapshot' {resourceType} -> resourceType) (\s@DiskSnapshot' {} a -> s {resourceType = a} :: DiskSnapshot)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
diskSnapshot_supportCode :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Text)
diskSnapshot_supportCode = Lens.lens (\DiskSnapshot' {supportCode} -> supportCode) (\s@DiskSnapshot' {} a -> s {supportCode = a} :: DiskSnapshot)

-- | The size of the disk in GB.
diskSnapshot_sizeInGb :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Int)
diskSnapshot_sizeInGb = Lens.lens (\DiskSnapshot' {sizeInGb} -> sizeInGb) (\s@DiskSnapshot' {} a -> s {sizeInGb = a} :: DiskSnapshot)

-- | The status of the disk snapshot operation.
diskSnapshot_state :: Lens.Lens' DiskSnapshot (Core.Maybe DiskSnapshotState)
diskSnapshot_state = Lens.lens (\DiskSnapshot' {state} -> state) (\s@DiskSnapshot' {} a -> s {state = a} :: DiskSnapshot)

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
diskSnapshot_name :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Text)
diskSnapshot_name = Lens.lens (\DiskSnapshot' {name} -> name) (\s@DiskSnapshot' {} a -> s {name = a} :: DiskSnapshot)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
diskSnapshot_tags :: Lens.Lens' DiskSnapshot (Core.Maybe [Tag])
diskSnapshot_tags = Lens.lens (\DiskSnapshot' {tags} -> tags) (\s@DiskSnapshot' {} a -> s {tags = a} :: DiskSnapshot) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
diskSnapshot_fromDiskArn :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Text)
diskSnapshot_fromDiskArn = Lens.lens (\DiskSnapshot' {fromDiskArn} -> fromDiskArn) (\s@DiskSnapshot' {} a -> s {fromDiskArn = a} :: DiskSnapshot)

-- | The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
diskSnapshot_fromInstanceName :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Text)
diskSnapshot_fromInstanceName = Lens.lens (\DiskSnapshot' {fromInstanceName} -> fromInstanceName) (\s@DiskSnapshot' {} a -> s {fromInstanceName = a} :: DiskSnapshot)

-- | The AWS Region and Availability Zone where the disk snapshot was
-- created.
diskSnapshot_location :: Lens.Lens' DiskSnapshot (Core.Maybe ResourceLocation)
diskSnapshot_location = Lens.lens (\DiskSnapshot' {location} -> location) (\s@DiskSnapshot' {} a -> s {location = a} :: DiskSnapshot)

-- | The progress of the snapshot.
diskSnapshot_progress :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Text)
diskSnapshot_progress = Lens.lens (\DiskSnapshot' {progress} -> progress) (\s@DiskSnapshot' {} a -> s {progress = a} :: DiskSnapshot)

instance Core.FromJSON DiskSnapshot where
  parseJSON =
    Core.withObject
      "DiskSnapshot"
      ( \x ->
          DiskSnapshot'
            Core.<$> (x Core..:? "isFromAutoSnapshot")
            Core.<*> (x Core..:? "fromDiskName")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "fromInstanceArn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "sizeInGb")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "fromDiskArn")
            Core.<*> (x Core..:? "fromInstanceName")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "progress")
      )

instance Core.Hashable DiskSnapshot

instance Core.NFData DiskSnapshot
