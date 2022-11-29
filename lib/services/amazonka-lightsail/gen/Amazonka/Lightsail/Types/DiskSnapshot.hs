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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DiskSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.DiskSnapshotState
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a block storage disk snapshot.
--
-- /See:/ 'newDiskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The progress of the snapshot.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @DiskSnapshot@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source instance from which the
    -- disk (system volume) snapshot was created.
    fromInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in GB.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The unique name of the source disk from which the disk snapshot was
    -- created.
    fromDiskName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the disk snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the disk snapshot operation.
    state :: Prelude.Maybe DiskSnapshotState,
    -- | The Amazon Resource Name (ARN) of the source disk from which the disk
    -- snapshot was created.
    fromDiskArn :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region and Availability Zone where the disk snapshot was
    -- created.
    location :: Prelude.Maybe ResourceLocation,
    -- | The unique name of the source instance from which the disk (system
    -- volume) snapshot was created.
    fromInstanceName :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the snapshot was created from an
    -- automatic snapshot.
    isFromAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The date when the disk snapshot was created.
    createdAt :: Prelude.Maybe Core.POSIX
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
-- 'tags', 'diskSnapshot_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'progress', 'diskSnapshot_progress' - The progress of the snapshot.
--
-- 'resourceType', 'diskSnapshot_resourceType' - The Lightsail resource type (e.g., @DiskSnapshot@).
--
-- 'name', 'diskSnapshot_name' - The name of the disk snapshot (e.g., @my-disk-snapshot@).
--
-- 'fromInstanceArn', 'diskSnapshot_fromInstanceArn' - The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
--
-- 'sizeInGb', 'diskSnapshot_sizeInGb' - The size of the disk in GB.
--
-- 'fromDiskName', 'diskSnapshot_fromDiskName' - The unique name of the source disk from which the disk snapshot was
-- created.
--
-- 'arn', 'diskSnapshot_arn' - The Amazon Resource Name (ARN) of the disk snapshot.
--
-- 'state', 'diskSnapshot_state' - The status of the disk snapshot operation.
--
-- 'fromDiskArn', 'diskSnapshot_fromDiskArn' - The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
--
-- 'location', 'diskSnapshot_location' - The AWS Region and Availability Zone where the disk snapshot was
-- created.
--
-- 'fromInstanceName', 'diskSnapshot_fromInstanceName' - The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
--
-- 'isFromAutoSnapshot', 'diskSnapshot_isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
--
-- 'supportCode', 'diskSnapshot_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'createdAt', 'diskSnapshot_createdAt' - The date when the disk snapshot was created.
newDiskSnapshot ::
  DiskSnapshot
newDiskSnapshot =
  DiskSnapshot'
    { tags = Prelude.Nothing,
      progress = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      fromInstanceArn = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      fromDiskName = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      fromDiskArn = Prelude.Nothing,
      location = Prelude.Nothing,
      fromInstanceName = Prelude.Nothing,
      isFromAutoSnapshot = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
diskSnapshot_tags :: Lens.Lens' DiskSnapshot (Prelude.Maybe [Tag])
diskSnapshot_tags = Lens.lens (\DiskSnapshot' {tags} -> tags) (\s@DiskSnapshot' {} a -> s {tags = a} :: DiskSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The progress of the snapshot.
diskSnapshot_progress :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_progress = Lens.lens (\DiskSnapshot' {progress} -> progress) (\s@DiskSnapshot' {} a -> s {progress = a} :: DiskSnapshot)

-- | The Lightsail resource type (e.g., @DiskSnapshot@).
diskSnapshot_resourceType :: Lens.Lens' DiskSnapshot (Prelude.Maybe ResourceType)
diskSnapshot_resourceType = Lens.lens (\DiskSnapshot' {resourceType} -> resourceType) (\s@DiskSnapshot' {} a -> s {resourceType = a} :: DiskSnapshot)

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
diskSnapshot_name :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_name = Lens.lens (\DiskSnapshot' {name} -> name) (\s@DiskSnapshot' {} a -> s {name = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
diskSnapshot_fromInstanceArn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromInstanceArn = Lens.lens (\DiskSnapshot' {fromInstanceArn} -> fromInstanceArn) (\s@DiskSnapshot' {} a -> s {fromInstanceArn = a} :: DiskSnapshot)

-- | The size of the disk in GB.
diskSnapshot_sizeInGb :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Int)
diskSnapshot_sizeInGb = Lens.lens (\DiskSnapshot' {sizeInGb} -> sizeInGb) (\s@DiskSnapshot' {} a -> s {sizeInGb = a} :: DiskSnapshot)

-- | The unique name of the source disk from which the disk snapshot was
-- created.
diskSnapshot_fromDiskName :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromDiskName = Lens.lens (\DiskSnapshot' {fromDiskName} -> fromDiskName) (\s@DiskSnapshot' {} a -> s {fromDiskName = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the disk snapshot.
diskSnapshot_arn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_arn = Lens.lens (\DiskSnapshot' {arn} -> arn) (\s@DiskSnapshot' {} a -> s {arn = a} :: DiskSnapshot)

-- | The status of the disk snapshot operation.
diskSnapshot_state :: Lens.Lens' DiskSnapshot (Prelude.Maybe DiskSnapshotState)
diskSnapshot_state = Lens.lens (\DiskSnapshot' {state} -> state) (\s@DiskSnapshot' {} a -> s {state = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
diskSnapshot_fromDiskArn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromDiskArn = Lens.lens (\DiskSnapshot' {fromDiskArn} -> fromDiskArn) (\s@DiskSnapshot' {} a -> s {fromDiskArn = a} :: DiskSnapshot)

-- | The AWS Region and Availability Zone where the disk snapshot was
-- created.
diskSnapshot_location :: Lens.Lens' DiskSnapshot (Prelude.Maybe ResourceLocation)
diskSnapshot_location = Lens.lens (\DiskSnapshot' {location} -> location) (\s@DiskSnapshot' {} a -> s {location = a} :: DiskSnapshot)

-- | The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
diskSnapshot_fromInstanceName :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromInstanceName = Lens.lens (\DiskSnapshot' {fromInstanceName} -> fromInstanceName) (\s@DiskSnapshot' {} a -> s {fromInstanceName = a} :: DiskSnapshot)

-- | A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
diskSnapshot_isFromAutoSnapshot :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Bool)
diskSnapshot_isFromAutoSnapshot = Lens.lens (\DiskSnapshot' {isFromAutoSnapshot} -> isFromAutoSnapshot) (\s@DiskSnapshot' {} a -> s {isFromAutoSnapshot = a} :: DiskSnapshot)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
diskSnapshot_supportCode :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_supportCode = Lens.lens (\DiskSnapshot' {supportCode} -> supportCode) (\s@DiskSnapshot' {} a -> s {supportCode = a} :: DiskSnapshot)

-- | The date when the disk snapshot was created.
diskSnapshot_createdAt :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.UTCTime)
diskSnapshot_createdAt = Lens.lens (\DiskSnapshot' {createdAt} -> createdAt) (\s@DiskSnapshot' {} a -> s {createdAt = a} :: DiskSnapshot) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DiskSnapshot where
  parseJSON =
    Core.withObject
      "DiskSnapshot"
      ( \x ->
          DiskSnapshot'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "progress")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "fromInstanceArn")
            Prelude.<*> (x Core..:? "sizeInGb")
            Prelude.<*> (x Core..:? "fromDiskName")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "fromDiskArn")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "fromInstanceName")
            Prelude.<*> (x Core..:? "isFromAutoSnapshot")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable DiskSnapshot where
  hashWithSalt _salt DiskSnapshot' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` fromInstanceArn
      `Prelude.hashWithSalt` sizeInGb
      `Prelude.hashWithSalt` fromDiskName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` fromDiskArn
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` fromInstanceName
      `Prelude.hashWithSalt` isFromAutoSnapshot
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData DiskSnapshot where
  rnf DiskSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf fromInstanceArn
      `Prelude.seq` Prelude.rnf sizeInGb
      `Prelude.seq` Prelude.rnf fromDiskName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf fromDiskArn
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf fromInstanceName
      `Prelude.seq` Prelude.rnf isFromAutoSnapshot
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf createdAt
