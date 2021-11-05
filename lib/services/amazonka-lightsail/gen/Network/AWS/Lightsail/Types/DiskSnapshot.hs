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
-- Module      : Amazonka.Lightsail.Types.DiskSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DiskSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.DiskSnapshotState
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a block storage disk snapshot.
--
-- /See:/ 'newDiskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { -- | The unique name of the source disk from which the disk snapshot was
    -- created.
    fromDiskName :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the snapshot was created from an
    -- automatic snapshot.
    isFromAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The status of the disk snapshot operation.
    state :: Prelude.Maybe DiskSnapshotState,
    -- | The Lightsail resource type (e.g., @DiskSnapshot@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the disk snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the disk snapshot was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The AWS Region and Availability Zone where the disk snapshot was
    -- created.
    location :: Prelude.Maybe ResourceLocation,
    -- | The progress of the snapshot.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in GB.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source instance from which the
    -- disk (system volume) snapshot was created.
    fromInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the source instance from which the disk (system
    -- volume) snapshot was created.
    fromInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source disk from which the disk
    -- snapshot was created.
    fromDiskArn :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiskSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromDiskName', 'diskSnapshot_fromDiskName' - The unique name of the source disk from which the disk snapshot was
-- created.
--
-- 'isFromAutoSnapshot', 'diskSnapshot_isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
--
-- 'state', 'diskSnapshot_state' - The status of the disk snapshot operation.
--
-- 'resourceType', 'diskSnapshot_resourceType' - The Lightsail resource type (e.g., @DiskSnapshot@).
--
-- 'arn', 'diskSnapshot_arn' - The Amazon Resource Name (ARN) of the disk snapshot.
--
-- 'createdAt', 'diskSnapshot_createdAt' - The date when the disk snapshot was created.
--
-- 'location', 'diskSnapshot_location' - The AWS Region and Availability Zone where the disk snapshot was
-- created.
--
-- 'progress', 'diskSnapshot_progress' - The progress of the snapshot.
--
-- 'name', 'diskSnapshot_name' - The name of the disk snapshot (e.g., @my-disk-snapshot@).
--
-- 'sizeInGb', 'diskSnapshot_sizeInGb' - The size of the disk in GB.
--
-- 'supportCode', 'diskSnapshot_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'fromInstanceArn', 'diskSnapshot_fromInstanceArn' - The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
--
-- 'fromInstanceName', 'diskSnapshot_fromInstanceName' - The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
--
-- 'fromDiskArn', 'diskSnapshot_fromDiskArn' - The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
--
-- 'tags', 'diskSnapshot_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
newDiskSnapshot ::
  DiskSnapshot
newDiskSnapshot =
  DiskSnapshot'
    { fromDiskName = Prelude.Nothing,
      isFromAutoSnapshot = Prelude.Nothing,
      state = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      location = Prelude.Nothing,
      progress = Prelude.Nothing,
      name = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      fromInstanceArn = Prelude.Nothing,
      fromInstanceName = Prelude.Nothing,
      fromDiskArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The unique name of the source disk from which the disk snapshot was
-- created.
diskSnapshot_fromDiskName :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromDiskName = Lens.lens (\DiskSnapshot' {fromDiskName} -> fromDiskName) (\s@DiskSnapshot' {} a -> s {fromDiskName = a} :: DiskSnapshot)

-- | A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
diskSnapshot_isFromAutoSnapshot :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Bool)
diskSnapshot_isFromAutoSnapshot = Lens.lens (\DiskSnapshot' {isFromAutoSnapshot} -> isFromAutoSnapshot) (\s@DiskSnapshot' {} a -> s {isFromAutoSnapshot = a} :: DiskSnapshot)

-- | The status of the disk snapshot operation.
diskSnapshot_state :: Lens.Lens' DiskSnapshot (Prelude.Maybe DiskSnapshotState)
diskSnapshot_state = Lens.lens (\DiskSnapshot' {state} -> state) (\s@DiskSnapshot' {} a -> s {state = a} :: DiskSnapshot)

-- | The Lightsail resource type (e.g., @DiskSnapshot@).
diskSnapshot_resourceType :: Lens.Lens' DiskSnapshot (Prelude.Maybe ResourceType)
diskSnapshot_resourceType = Lens.lens (\DiskSnapshot' {resourceType} -> resourceType) (\s@DiskSnapshot' {} a -> s {resourceType = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the disk snapshot.
diskSnapshot_arn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_arn = Lens.lens (\DiskSnapshot' {arn} -> arn) (\s@DiskSnapshot' {} a -> s {arn = a} :: DiskSnapshot)

-- | The date when the disk snapshot was created.
diskSnapshot_createdAt :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.UTCTime)
diskSnapshot_createdAt = Lens.lens (\DiskSnapshot' {createdAt} -> createdAt) (\s@DiskSnapshot' {} a -> s {createdAt = a} :: DiskSnapshot) Prelude.. Lens.mapping Core._Time

-- | The AWS Region and Availability Zone where the disk snapshot was
-- created.
diskSnapshot_location :: Lens.Lens' DiskSnapshot (Prelude.Maybe ResourceLocation)
diskSnapshot_location = Lens.lens (\DiskSnapshot' {location} -> location) (\s@DiskSnapshot' {} a -> s {location = a} :: DiskSnapshot)

-- | The progress of the snapshot.
diskSnapshot_progress :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_progress = Lens.lens (\DiskSnapshot' {progress} -> progress) (\s@DiskSnapshot' {} a -> s {progress = a} :: DiskSnapshot)

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
diskSnapshot_name :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_name = Lens.lens (\DiskSnapshot' {name} -> name) (\s@DiskSnapshot' {} a -> s {name = a} :: DiskSnapshot)

-- | The size of the disk in GB.
diskSnapshot_sizeInGb :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Int)
diskSnapshot_sizeInGb = Lens.lens (\DiskSnapshot' {sizeInGb} -> sizeInGb) (\s@DiskSnapshot' {} a -> s {sizeInGb = a} :: DiskSnapshot)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
diskSnapshot_supportCode :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_supportCode = Lens.lens (\DiskSnapshot' {supportCode} -> supportCode) (\s@DiskSnapshot' {} a -> s {supportCode = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
diskSnapshot_fromInstanceArn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromInstanceArn = Lens.lens (\DiskSnapshot' {fromInstanceArn} -> fromInstanceArn) (\s@DiskSnapshot' {} a -> s {fromInstanceArn = a} :: DiskSnapshot)

-- | The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
diskSnapshot_fromInstanceName :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromInstanceName = Lens.lens (\DiskSnapshot' {fromInstanceName} -> fromInstanceName) (\s@DiskSnapshot' {} a -> s {fromInstanceName = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
diskSnapshot_fromDiskArn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromDiskArn = Lens.lens (\DiskSnapshot' {fromDiskArn} -> fromDiskArn) (\s@DiskSnapshot' {} a -> s {fromDiskArn = a} :: DiskSnapshot)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
diskSnapshot_tags :: Lens.Lens' DiskSnapshot (Prelude.Maybe [Tag])
diskSnapshot_tags = Lens.lens (\DiskSnapshot' {tags} -> tags) (\s@DiskSnapshot' {} a -> s {tags = a} :: DiskSnapshot) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DiskSnapshot where
  parseJSON =
    Core.withObject
      "DiskSnapshot"
      ( \x ->
          DiskSnapshot'
            Prelude.<$> (x Core..:? "fromDiskName")
            Prelude.<*> (x Core..:? "isFromAutoSnapshot")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "progress")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "sizeInGb")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "fromInstanceArn")
            Prelude.<*> (x Core..:? "fromInstanceName")
            Prelude.<*> (x Core..:? "fromDiskArn")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable DiskSnapshot

instance Prelude.NFData DiskSnapshot
