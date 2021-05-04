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
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshot where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DiskSnapshotState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes a block storage disk snapshot.
--
-- /See:/ 'newDiskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { -- | A Boolean value indicating whether the snapshot was created from an
    -- automatic snapshot.
    isFromAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The unique name of the source disk from which the disk snapshot was
    -- created.
    fromDiskName :: Prelude.Maybe Prelude.Text,
    -- | The date when the disk snapshot was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the disk snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source instance from which the
    -- disk (system volume) snapshot was created.
    fromInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @DiskSnapshot@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in GB.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The status of the disk snapshot operation.
    state :: Prelude.Maybe DiskSnapshotState,
    -- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the source disk from which the disk
    -- snapshot was created.
    fromDiskArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the source instance from which the disk (system
    -- volume) snapshot was created.
    fromInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region and Availability Zone where the disk snapshot was
    -- created.
    location :: Prelude.Maybe ResourceLocation,
    -- | The progress of the snapshot.
    progress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { isFromAutoSnapshot = Prelude.Nothing,
      fromDiskName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      fromInstanceArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      fromDiskArn = Prelude.Nothing,
      fromInstanceName = Prelude.Nothing,
      location = Prelude.Nothing,
      progress = Prelude.Nothing
    }

-- | A Boolean value indicating whether the snapshot was created from an
-- automatic snapshot.
diskSnapshot_isFromAutoSnapshot :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Bool)
diskSnapshot_isFromAutoSnapshot = Lens.lens (\DiskSnapshot' {isFromAutoSnapshot} -> isFromAutoSnapshot) (\s@DiskSnapshot' {} a -> s {isFromAutoSnapshot = a} :: DiskSnapshot)

-- | The unique name of the source disk from which the disk snapshot was
-- created.
diskSnapshot_fromDiskName :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromDiskName = Lens.lens (\DiskSnapshot' {fromDiskName} -> fromDiskName) (\s@DiskSnapshot' {} a -> s {fromDiskName = a} :: DiskSnapshot)

-- | The date when the disk snapshot was created.
diskSnapshot_createdAt :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.UTCTime)
diskSnapshot_createdAt = Lens.lens (\DiskSnapshot' {createdAt} -> createdAt) (\s@DiskSnapshot' {} a -> s {createdAt = a} :: DiskSnapshot) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the disk snapshot.
diskSnapshot_arn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_arn = Lens.lens (\DiskSnapshot' {arn} -> arn) (\s@DiskSnapshot' {} a -> s {arn = a} :: DiskSnapshot)

-- | The Amazon Resource Name (ARN) of the source instance from which the
-- disk (system volume) snapshot was created.
diskSnapshot_fromInstanceArn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromInstanceArn = Lens.lens (\DiskSnapshot' {fromInstanceArn} -> fromInstanceArn) (\s@DiskSnapshot' {} a -> s {fromInstanceArn = a} :: DiskSnapshot)

-- | The Lightsail resource type (e.g., @DiskSnapshot@).
diskSnapshot_resourceType :: Lens.Lens' DiskSnapshot (Prelude.Maybe ResourceType)
diskSnapshot_resourceType = Lens.lens (\DiskSnapshot' {resourceType} -> resourceType) (\s@DiskSnapshot' {} a -> s {resourceType = a} :: DiskSnapshot)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
diskSnapshot_supportCode :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_supportCode = Lens.lens (\DiskSnapshot' {supportCode} -> supportCode) (\s@DiskSnapshot' {} a -> s {supportCode = a} :: DiskSnapshot)

-- | The size of the disk in GB.
diskSnapshot_sizeInGb :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Int)
diskSnapshot_sizeInGb = Lens.lens (\DiskSnapshot' {sizeInGb} -> sizeInGb) (\s@DiskSnapshot' {} a -> s {sizeInGb = a} :: DiskSnapshot)

-- | The status of the disk snapshot operation.
diskSnapshot_state :: Lens.Lens' DiskSnapshot (Prelude.Maybe DiskSnapshotState)
diskSnapshot_state = Lens.lens (\DiskSnapshot' {state} -> state) (\s@DiskSnapshot' {} a -> s {state = a} :: DiskSnapshot)

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@).
diskSnapshot_name :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_name = Lens.lens (\DiskSnapshot' {name} -> name) (\s@DiskSnapshot' {} a -> s {name = a} :: DiskSnapshot)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
diskSnapshot_tags :: Lens.Lens' DiskSnapshot (Prelude.Maybe [Tag])
diskSnapshot_tags = Lens.lens (\DiskSnapshot' {tags} -> tags) (\s@DiskSnapshot' {} a -> s {tags = a} :: DiskSnapshot) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the source disk from which the disk
-- snapshot was created.
diskSnapshot_fromDiskArn :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromDiskArn = Lens.lens (\DiskSnapshot' {fromDiskArn} -> fromDiskArn) (\s@DiskSnapshot' {} a -> s {fromDiskArn = a} :: DiskSnapshot)

-- | The unique name of the source instance from which the disk (system
-- volume) snapshot was created.
diskSnapshot_fromInstanceName :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_fromInstanceName = Lens.lens (\DiskSnapshot' {fromInstanceName} -> fromInstanceName) (\s@DiskSnapshot' {} a -> s {fromInstanceName = a} :: DiskSnapshot)

-- | The AWS Region and Availability Zone where the disk snapshot was
-- created.
diskSnapshot_location :: Lens.Lens' DiskSnapshot (Prelude.Maybe ResourceLocation)
diskSnapshot_location = Lens.lens (\DiskSnapshot' {location} -> location) (\s@DiskSnapshot' {} a -> s {location = a} :: DiskSnapshot)

-- | The progress of the snapshot.
diskSnapshot_progress :: Lens.Lens' DiskSnapshot (Prelude.Maybe Prelude.Text)
diskSnapshot_progress = Lens.lens (\DiskSnapshot' {progress} -> progress) (\s@DiskSnapshot' {} a -> s {progress = a} :: DiskSnapshot)

instance Prelude.FromJSON DiskSnapshot where
  parseJSON =
    Prelude.withObject
      "DiskSnapshot"
      ( \x ->
          DiskSnapshot'
            Prelude.<$> (x Prelude..:? "isFromAutoSnapshot")
            Prelude.<*> (x Prelude..:? "fromDiskName")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "fromInstanceArn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> (x Prelude..:? "sizeInGb")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "fromDiskArn")
            Prelude.<*> (x Prelude..:? "fromInstanceName")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "progress")
      )

instance Prelude.Hashable DiskSnapshot

instance Prelude.NFData DiskSnapshot
