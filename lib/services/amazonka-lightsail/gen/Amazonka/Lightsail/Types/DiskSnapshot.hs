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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DiskSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.DiskSnapshotState
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a block storage disk snapshot.
--
-- /See:/ 'newDiskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { -- | The Amazon Resource Name (ARN) of the disk snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the disk snapshot was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the source disk from which the disk
    -- snapshot was created.
    fromDiskArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the source disk from which the disk snapshot was
    -- created.
    fromDiskName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source instance from which the
    -- disk (system volume) snapshot was created.
    fromInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the source instance from which the disk (system
    -- volume) snapshot was created.
    fromInstanceName :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the snapshot was created from an
    -- automatic snapshot.
    isFromAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The AWS Region and Availability Zone where the disk snapshot was
    -- created.
    location :: Prelude.Maybe ResourceLocation,
    -- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The progress of the snapshot.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @DiskSnapshot@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The size of the disk in GB.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The status of the disk snapshot operation.
    state :: Prelude.Maybe DiskSnapshotState,
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
-- Create a value of 'DiskSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'diskSnapshot_arn' - The Amazon Resource Name (ARN) of the disk snapshot.
--
-- 'createdAt', 'diskSnapshot_createdAt' - The date when the disk snapshot was created.
--
-- 'fromDiskArn', 'diskSnapshot_fromDiskArn' - The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
--
-- 'fromDiskName', 'diskSnapshot_fromDiskName' - The unique name of the source disk from which the disk snapshot was
-- created.
--
-- 'fromInstanceArn', 'diskSnapshot_fromInstanceArn' - The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
--
-- 'fromInstanceName', 'diskSnapshot_fromInstanceName' - The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
--
-- 'isFromAutoSnapshot', 'diskSnapshot_isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
--
-- 'location', 'diskSnapshot_location' - The AWS Region and Availability Zone where the disk snapshot was
-- created.
--
-- 'name', 'diskSnapshot_name' - The name of the disk snapshot (e.g., @my-disk-snapshot@).
--
-- 'progress', 'diskSnapshot_progress' - The progress of the snapshot.
--
-- 'resourceType', 'diskSnapshot_resourceType' - The Lightsail resource type (e.g., @DiskSnapshot@).
--
-- 'sizeInGb', 'diskSnapshot_sizeInGb' - The size of the disk in GB.
--
-- 'state', 'diskSnapshot_state' - The status of the disk snapshot operation.
--
-- 'supportCode', 'diskSnapshot_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'tags', 'diskSnapshot_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
newDiskSnapshot ::
  DiskSnapshot
newDiskSnapshot =
  DiskSnapshot'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      fromDiskArn = Prelude.Nothing,
      fromDiskName = Prelude.Nothing,
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

-- | The Amazon Resource Name (ARN) of the disk snapshot.
diskSnapshot_arn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_arn = Lens.lens (\DiskSnapshot' {arn} -> arn) (\s@DiskSnapshot' {} a -> s {arn = a} :: DiskSnapshot)

-- | The date when the disk snapshot was created.
diskSnapshot_createdAt :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.UTCTime)
diskSnapshot_createdAt = Lens.lens (\DiskSnapshot' {createdAt} -> createdAt) (\s@DiskSnapshot' {} a -> s {createdAt = a} :: DiskSnapshot) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
diskSnapshot_fromDiskArn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromDiskArn = Lens.lens (\DiskSnapshot' {fromDiskArn} -> fromDiskArn) (\s@DiskSnapshot' {} a -> s {fromDiskArn = a} :: DiskSnapshot)

-- | The unique name of the source disk from which the disk snapshot was
-- created.
diskSnapshot_fromDiskName :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromDiskName = Lens.lens (\DiskSnapshot' {fromDiskName} -> fromDiskName) (\s@DiskSnapshot' {} a -> s {fromDiskName = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
diskSnapshot_fromInstanceArn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromInstanceArn = Lens.lens (\DiskSnapshot' {fromInstanceArn} -> fromInstanceArn) (\s@DiskSnapshot' {} a -> s {fromInstanceArn = a} :: DiskSnapshot)

-- | The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
diskSnapshot_fromInstanceName :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromInstanceName = Lens.lens (\DiskSnapshot' {fromInstanceName} -> fromInstanceName) (\s@DiskSnapshot' {} a -> s {fromInstanceName = a} :: DiskSnapshot)

-- | A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
diskSnapshot_isFromAutoSnapshot :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Bool)
diskSnapshot_isFromAutoSnapshot = Lens.lens (\DiskSnapshot' {isFromAutoSnapshot} -> isFromAutoSnapshot) (\s@DiskSnapshot' {} a -> s {isFromAutoSnapshot = a} :: DiskSnapshot)

-- | The AWS Region and Availability Zone where the disk snapshot was
-- created.
diskSnapshot_location :: Lens.Lens' DiskSnapshot (Prelude.Maybe ResourceLocation)
diskSnapshot_location = Lens.lens (\DiskSnapshot' {location} -> location) (\s@DiskSnapshot' {} a -> s {location = a} :: DiskSnapshot)

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
diskSnapshot_name :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_name = Lens.lens (\DiskSnapshot' {name} -> name) (\s@DiskSnapshot' {} a -> s {name = a} :: DiskSnapshot)

-- | The progress of the snapshot.
diskSnapshot_progress :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_progress = Lens.lens (\DiskSnapshot' {progress} -> progress) (\s@DiskSnapshot' {} a -> s {progress = a} :: DiskSnapshot)

-- | The Lightsail resource type (e.g., @DiskSnapshot@).
diskSnapshot_resourceType :: Lens.Lens' DiskSnapshot (Prelude.Maybe ResourceType)
diskSnapshot_resourceType = Lens.lens (\DiskSnapshot' {resourceType} -> resourceType) (\s@DiskSnapshot' {} a -> s {resourceType = a} :: DiskSnapshot)

-- | The size of the disk in GB.
diskSnapshot_sizeInGb :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Int)
diskSnapshot_sizeInGb = Lens.lens (\DiskSnapshot' {sizeInGb} -> sizeInGb) (\s@DiskSnapshot' {} a -> s {sizeInGb = a} :: DiskSnapshot)

-- | The status of the disk snapshot operation.
diskSnapshot_state :: Lens.Lens' DiskSnapshot (Prelude.Maybe DiskSnapshotState)
diskSnapshot_state = Lens.lens (\DiskSnapshot' {state} -> state) (\s@DiskSnapshot' {} a -> s {state = a} :: DiskSnapshot)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
diskSnapshot_supportCode :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_supportCode = Lens.lens (\DiskSnapshot' {supportCode} -> supportCode) (\s@DiskSnapshot' {} a -> s {supportCode = a} :: DiskSnapshot)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
diskSnapshot_tags :: Lens.Lens' DiskSnapshot (Prelude.Maybe [Tag])
diskSnapshot_tags = Lens.lens (\DiskSnapshot' {tags} -> tags) (\s@DiskSnapshot' {} a -> s {tags = a} :: DiskSnapshot) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DiskSnapshot where
  parseJSON =
    Data.withObject
      "DiskSnapshot"
      ( \x ->
          DiskSnapshot'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "fromDiskArn")
            Prelude.<*> (x Data..:? "fromDiskName")
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

instance Prelude.Hashable DiskSnapshot where
  hashWithSalt _salt DiskSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` fromDiskArn
      `Prelude.hashWithSalt` fromDiskName
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

instance Prelude.NFData DiskSnapshot where
  rnf DiskSnapshot' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf fromDiskArn
      `Prelude.seq` Prelude.rnf fromDiskName
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
